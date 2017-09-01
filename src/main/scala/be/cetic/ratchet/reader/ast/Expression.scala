package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.TypeConverter
import be.cetic.ratchet.reader.helpers.{VariableGenerator, _}
import be.cetic.ratchet.utils.{Event, Observer, Subject, UnsupportedTypeException}


abstract class Expression(val priority: Int) extends AstNode {

  override def clone = this

  def typeTree: String = "[" + this.getClass.getName + "=" + computeType + ":(" +
    children.map(c => c.asInstanceOf[Expression].typeTree).mkString(",") + ")]"

  /** returns the type resolved, that is,
    * where all typedefs, struct dereference, and enum dereference are resolved.
    * it actually stops at struct elements */
  def resolvedType: Type = outputType.resolvedType

  /** This computes the output type of the expression.
    * it might throw an exception in case parameter types are unsupported.
    * if returns false, an exception is thrown by the system
    */
  def computeType: Type

  val outputType: Type = {
    val tmp = computeType
    if (tmp == null)
      throw new Exception("unsupported type for " + this.getClass.getName)
    tmp
  }

  def isConstant: Boolean


  /** you must check that types are not changed when you change a part of the subtree.
    * */
  def checkUnchangedType() {
    val computedType = computeType
    if (computedType == null) {
      throw new UnsupportedTypeException("unsupported type for " + this.getClass.getName +
        " and BTW, you cannot change the type of expression by modifying AST")
    }
  }

  def isLvalue: Boolean

  def printExpr(outerPriority: Int): String = {
    if (outerPriority >= priority)
      "(" + printMe() + ")"
    else printMe()
  }

  /**
    * Add the expression in parenthesis
    *
    * @return
    */
  def encloseIfNeeded = this match {
    case x: terminalASTNode => x
    case x => Parenthesis(x)
  }

  def printMe(): String

  override def toString = printMe()

  def isSimple = this match {
    case e: terminalASTNode => true
    case e => false
  }

  def isComplex = !isSimple

  def lookForVariableParent: Option[Expression] = {
    def doit(cur: AstNode): Option[Expression] = {
      cur match {
        case e: ExpressionInstr => None
        case e: Expression =>
          if (e.isConstant) doit(cur.father)
          else Some(e)
        case e: VarDecl => None
        case e: Return => None
        case _ => throw new Exception("only apply on Expression, this was" + cur.getClass.getCanonicalName)
      }
    }

    doit(this)
  }

  def extractFrom(instr: Instruction, tvg: VariableGenerator) {

    def handle(e: Expression): Unit = {
      Logger.getLogger("Instruction").log(Level.INFO, "Simplifying expression " + e)
      if (e.isComplex || (e.isInstanceOf[Constant] && e.father.isInstanceOf[Neg])) {
        e.children.foreach(c => handle(c.asInstanceOf[Expression]))
        if (e.isConstant) handleConstantExpression(e)
        else makeSimplificationOf(e, TypeConverter.map(e.computeType))
      }
    }


    def handleConstantExpression(e: Expression) {
      val variableParent = e.lookForVariableParent
      val eType = variableParent match {
        case Some(vp) => vp match {
          case ei: ExpressionInstr => TypeConverter.map(e.computeType)
          case expr: BinOp => TypeConverter.map(expr.computeType)
          case uop: UnaryOp => TypeConverter.map(e.computeType)
          case prefixop: PrefixOp => prefixop match {
            case c: Cast => TypeConverter.map(c.to)
          }
          case pfop: PostFixOp => pfop match {
            case aa: ArrayAccess => TypeConverter.map(e.computeType)
            case fa: FieldAccess => throw new Exception("Cannot be a parent") //impossible
            case ifa: IndirectFieldAccess => throw new Exception("Cannot be a parent") //impossible
          }
          case exp: Variable => throw new Exception("Cannot be a parent") //impossible
          case exp: EnumElementExpr => throw new Exception("Cannot be a parent") //impossible
          case const: Constant => throw new Exception("Cannot be a parent") //impossible
          case ad: ArrayDef => throw new Exception("Cannot be a parent") //impossible
          case c: Call => TypeConverter.map(e.computeType)
        }
        case None => TypeConverter.map(e.computeType)
      }
      makeSimplificationOf(e, eType)
    }

    def makeSimplificationOf(e: Expression, t: Type) = e match {
      case p: Parenthesis => e.replaceBy(p.internalE)
      case _ =>
        val tmp = tvg.next("tmp", t)
        e.replaceBy(tmp.asVariable)
        instr.insertBefore(tmp, ExpressionInstr(Affect(tmp.asVariable, e)))
    }

    handle(this)
  }

}


abstract class BinOp(opStr: String, priority: Int) extends Expression(priority) {

  def internLeft: Expression

  def internLeft_=(a: Expression)

  def internRight: Expression

  def internRight_=(a: Expression)

  final def left = internLeft

  final def right = internRight

  final def repr = opStr

  final override def children: List[AstNode] = List(left, right)

  left.father = this
  right.father = this

  final override def replaceChild(child: AstNode, by: AstNode) {
    if (child == left) {
      left.father = null; left = by.asInstanceOf[Expression]; left.father = this
    }
    else if (child == right) {
      right.father = null; right = by.asInstanceOf[Expression]; right.father = this
    }
    else throw new Exception(" AST " + child + " is not a subtree of " + this)
  }

  override def isConstant = left.isConstant && right.isConstant

  /** You must call this method to change the left child */
  final def left_=(by: Expression) {
    internLeft = by
    internLeft.father = this
    checkUnchangedType()
  }

  /** You must call this method to change the right child */
  final def right_=(by: Expression) {
    internRight = by
    internRight.father = this
    checkUnchangedType()
  }

  override def isLvalue: Boolean = false

  def printMe(): String = left.printExpr(priority) + " " + opStr + " " + right.printExpr(priority)

  override def equals(obj: Any) = obj match {
    case t: BinOp => super.equals(t) && t.left.equals(left) && t.right.equals(right)
    case _ => false
  }
}


abstract class LogicalBinOp(opStr: String) extends BinOp(opStr, 1) {
  final def computeType: Type = {
    (left.resolvedType, right.resolvedType) match {
      case (_: Integral, _: Integral) => BoolType()
      case _ => null
    }
  }
}


case class LogicalOr(var internLeft: Expression, var internRight: Expression) extends LogicalBinOp("||") {
  override def clone = LogicalOr(internLeft.clone.asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}


case class LogicalAnd(var internLeft: Expression, var internRight: Expression) extends LogicalBinOp("&&") {
  override def clone = LogicalAnd(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}


abstract class BitWiseLogicalBinOp(opStr: String) extends ArithmeticsOperator(opStr, 1)


case class BitAnd(var internLeft: Expression, var internRight: Expression) extends BitWiseLogicalBinOp("&") {
  override def clone = BitAnd(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}


case class BitOr(var internLeft: Expression, var internRight: Expression) extends BitWiseLogicalBinOp("|") {
  override def clone = BitOr(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}


case class BitXor(var internLeft: Expression, var internRight: Expression) extends BitWiseLogicalBinOp("^") {
  override def clone = BitXor(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

abstract class ArithmeticsOperator(opstr: String, priority: Int) extends BinOp(opstr, 1) {

  final def computeType: Type = {
    if (left.isConstant)
      right.computeType
    else if (right.isConstant) left.computeType
    else (left.resolvedType, right.resolvedType) match {
      case (p: PointerType, _: Integral) => left.computeType
      case (_: Integral, p: PointerType) => right.computeType
      case (p: ArrayType, _: Integral) => left.computeType
      case (_: Integral, p: ArrayType) => right.computeType
      case (i: IntType, _: IntType) => left.computeType //TODO: check size and promotion
      case (i: IntType, _: Integral) => left.computeType // in this case integral is a char
      case (_: Integral, j: IntType) => right.computeType // in this case integral is a char
      case (i: CharType, _: CharType) => left.computeType
      case (e: Enum, _: Integral) => left.computeType
      case (_: Integral, e: Enum) => right.computeType
      case (_: BoolType, _: BoolType) => right.computeType
      case (_: BoolType, _: Integral) => right.computeType
      case (_: Integral, _: BoolType) => left.computeType
      case (l, r) => throw new Exception("Incorrect type for " + this.getClass.getName + " : (" + l + ") " + opstr + " (" + r + ")")
    }
  }
}


case class Plus(var internLeft: Expression, var internRight: Expression) extends ArithmeticsOperator("+", 1) {
  override def clone = Plus(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Minus(var internLeft: Expression, var internRight: Expression) extends ArithmeticsOperator("-", 1) {
  override def clone = Minus(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Mod(var internLeft: Expression, var internRight: Expression) extends ArithmeticsOperator("%", 1) {
  override def clone = Mod(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Times(var internLeft: Expression, var internRight: Expression) extends ArithmeticsOperator("*", 1) {
  override def clone = Times(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Div(var internLeft: Expression, var internRight: Expression) extends ArithmeticsOperator("/", 1) {
  override def clone = Div(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

abstract class EqualityBinOp(opStr: String) extends BinOp(opStr, 1) {
  final def computeType: Type = {
    (left.resolvedType, right.resolvedType) match {
      case (_: Integral, _: Integral) => BoolType()
      case _ => null
    }
  }
}

case class Equ(var internLeft: Expression, var internRight: Expression) extends EqualityBinOp("==") {
  override def clone = Equ(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Nequ(var internLeft: Expression, var internRight: Expression) extends EqualityBinOp("!=") {
  override def clone = Nequ(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

abstract class RelationalBinOp(opStr: String) extends BinOp(opStr, 1) {
  final def computeType: Type = (left.resolvedType, right.resolvedType) match {
    case (_: Integral, _: Integral) => BoolType()
    case _ => null
  }
}

case class G(var internLeft: Expression, var internRight: Expression) extends RelationalBinOp(">") {
  override def clone = G(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class L(var internLeft: Expression, var internRight: Expression) extends RelationalBinOp("<") {
  override def clone = L(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Ge(var internLeft: Expression, var internRight: Expression) extends RelationalBinOp(">=") {
  override def clone = Ge(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}

case class Le(var internLeft: Expression, var internRight: Expression) extends RelationalBinOp("<=") {
  override def clone = Le(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])
}


case class ShiftLeft(var internLeft: Expression, var internRight: Expression) extends BinOp("<<", 1) {
  override def clone = ShiftLeft(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  final def computeType: Type = {
    left.computeType
  }
}

case class ShiftRight(var internLeft: Expression, var internRight: Expression) extends BinOp(">>", 1) {
  override def clone = ShiftRight(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  final def computeType: Type = {
    left.computeType
  }
}

//NOTE: assignations cannot be used as expressions.
//Note: pre and post increment and decrement are forbidden

case class ArrayAccess(
                        private var _array: Expression,
                        private var _index: Expression)
  extends PostFixOp(2) {

  _array.father = this
  _index.father = this

  def computeType: Type = array.computeType match {
    case t: ArrayType => t.of
    case t: PointerType => t.to
    case t => throw new Exception("Unexpected type : " + t)
  }

  def array = _array

  def array_=(x: Expression): Unit = {
    _array.father = null
    _array = x
    _array.father = this
    checkUnchangedType()
  }

  def index = _index

  def index_=(x: Expression): Unit = {
    _index.father = null
    _index = x
    _index.father = this
  }

  override def isConstant = false

  override def isSimple = false

  override def printMe(): String = array.toString + "[" + index + "]"

  override def isLvalue: Boolean = array.isLvalue

  override def children: List[AstNode] = array :: index :: Nil

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == array) {
      array = by.asInstanceOf[Expression]
    }
    else if (child == index) {
      index = by.asInstanceOf[Expression]
    }
    else throw ASTException(" AST " + child + " is not a subtree of " + this)
  }

  override def clone = new ArrayAccess(array.clone().asInstanceOf[Expression], index.clone().asInstanceOf[Expression])

  override def equals(obj: Any) = obj match {
    case t: ArrayAccess => super.equals(t) && t.array.equals(array) && t.index.equals(index)
    case _ => false
  }

  override def baseExpression: Expression = array

  override def operator: String = "[" + index + "]"
}


abstract class AssignementExpression(op: String)
  extends BinOp(op, 0) {

  def computeType: Type = {
    if (!left.isLvalue) {
      throw ASTException(left.toString + " is not a Lvalue")
    }
    if (!right.resolvedType.canBeConvertedTo(left.resolvedType)) {
      throw ASTException(left + "(type=" + left.computeType + ") cannot be converted to " + right + "(type=" + right.computeType + ")")
    }
    left.outputType
  }

  override def equals(n: Any): Boolean = {
    super.equals(n)
  }

  def replaceByBinOp()
}


case class Affect(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("=") {
  override def clone = Affect(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {}
}


case class CompoundPlus(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("+=") {
  override def clone = CompoundPlus(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, Plus(left, right)))
  }
}


case class CompoundMinus(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("-=") {
  override def clone = CompoundMinus(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, Minus(left, right)))
  }
}


case class CompoundTimes(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("*=") {
  override def clone = CompoundTimes(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, Times(left, right)))
  }
}


case class CompoundDiv(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("/=") {
  override def clone = CompoundDiv(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, Div(left, right)))
  }
}


case class CompoundMod(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("%=") {
  override def clone = CompoundMod(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, Mod(left, right)))
  }
}


case class CompoundShiftLeft(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("<<=") {
  override def clone = CompoundShiftLeft(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, ShiftLeft(left, right)))
  }
}


case class CompoundShiftRight(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression(">>=") {
  override def clone = CompoundShiftRight(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, ShiftRight(left, right)))
  }
}

case class CompoundAnd(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("&=") {
  override def clone = CompoundAnd(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, BitAnd(left, right)))
  }
}


case class CompoundOr(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("|=") {
  override def clone = CompoundOr(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, BitOr(left, right)))
  }
}


case class CompoundXor(var internLeft: Expression, var internRight: Expression) extends
  AssignementExpression("^=") {
  override def clone = CompoundXor(internLeft.clone().asInstanceOf[Expression], internRight.clone().asInstanceOf[Expression])

  def replaceByBinOp() = {
    replaceBy(Affect(left, BitXor(left, right)))
  }
}


abstract class UnaryOp(opStr: String, priority: Int) extends Expression(priority) {
  def internalE: Expression

  def internalE_=(x: Expression)

  final def repr = opStr

  def e = internalE

  def e_=(x: Expression) {
    internalE.father = null
    internalE = x
    internalE.father = this
    checkUnchangedType()
  }

  override def children: List[AstNode] = List(e)

  if (e != null) e.father = this

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == e) {
      e = by.asInstanceOf[Expression]
    }
    else throw ASTException(" AST " + child + " is not a subtree of " + this)
  }

  override def isLvalue: Boolean = false

  override def equals(obj: Any) = obj match {
    case t: UnaryOp => super.equals(t) && ((e == null && t.e == null) || t.e.equals(e))
    case _ => false
  }
}


abstract class PrefixOp(priority: Int) extends Expression(priority) {
  def operator: String

  def baseExpression: Expression

  def printMe(): String = operator + baseExpression.printExpr(priority)
}


abstract class PrefixUnaryOp(opStr: String, priority: Int) extends UnaryOp(opStr, priority) {
  def operator: String = opStr

  def baseExpression: Expression = internalE

  def printMe(): String = operator + baseExpression.printExpr(priority)
}


abstract class PostFixOp(priority: Int) extends Expression(priority) {
  def operator: String

  def baseExpression: Expression

  def printMe(): String = baseExpression.printExpr(priority) + operator
}


abstract class PostFixUnaryOp(opStr: String, priority: Int) extends UnaryOp(opStr, priority) {
  def operator: String = opStr

  def baseExpression: Expression = internalE

  def printMe(): String = baseExpression.printExpr(priority) + operator
}


case class PostFixPlus(var expr: Expression) extends UnaryOp("++", 2) {
  assert(expr.isLvalue)

  override def internalE_=(x: Expression): Unit = {
    expr = x
    assert(expr.isLvalue)
  }

  override def clone = PostFixPlus(expr.clone.asInstanceOf[Expression])

  override def internalE: Expression = expr

  override def printMe(): String = expr.printMe + "++"

  override def isConstant = false

  /** This computes the output type of the expression.
    * it might throw an exception in case parameter types are unsupported.
    * if returns false, an exception is thrown by the system
    */
  override def computeType: Type = expr.computeType
}


case class PostFixMinus(var expr: Expression) extends UnaryOp("--", 2) {
  assert(expr.isLvalue)

  override def internalE_=(x: Expression): Unit = {
    expr = x
    assert(expr.isLvalue)
  }

  override def clone = PostFixMinus(expr.clone.asInstanceOf[Expression])

  override def internalE: Expression = expr

  override def printMe(): String = expr.printMe + "--"

  override def isConstant = false

  /** This computes the output type of the expression.
    * it might throw an exception in case parameter types are unsupported.
    * if returns false, an exception is thrown by the system
    */
  override def computeType: Type = expr.computeType
}

case class Not(var internalE: Expression) extends PrefixUnaryOp("!", 1) {
  override def clone = Not(internalE.clone().asInstanceOf[Expression])

  override def computeType = {
    if (!e.computeType.resolvedType.isInstanceOf[Integral]) null
    else e.computeType
  }

  override def isConstant = e.isConstant

}


case class BitNot(var internalE: Expression) extends PrefixUnaryOp("~", 1) {
  override def clone = BitNot(internalE.clone().asInstanceOf[Expression])

  override def computeType = e.outputType

  override def isConstant = e.isConstant
}


case class Neg(var internalE: Expression) extends PrefixUnaryOp("-", 3) {
  override def clone = Neg(internalE.clone().asInstanceOf[Expression])

  override def isSimple = false

  override def isConstant = e.isConstant

  override def computeType: Type = {
    if (!e.resolvedType.isInstanceOf[Integral]) throw ASTException("Type of expression must be an integer")
    e.outputType
  }
}


case class Adr(var internalE: Expression) extends PrefixUnaryOp("&", 10) {
  override def isLvalue = true

  override def clone = Adr(internalE.clone.asInstanceOf[Expression])

  override def isConstant = false

  override def isSimple = false

  override def computeType: Type = {
    if (!e.isLvalue)
      throw new Exception("Unsupported type : " + e.computeType + " for Adr (not a Lvalue), internal " + e.printMe())
    else PointerType(e.outputType)
  }
}


case class DecrementPrefix(var internalE: Expression) extends PrefixUnaryOp("--", 1) {
  override def computeType: Type = e.resolvedType

  override def clone = DecrementPrefix(internalE.clone.asInstanceOf[Expression])

  override def isConstant = false
}


case class IncrementPrefix(var internalE: Expression) extends PrefixUnaryOp("++", 1) {
  override def computeType: Type = e.resolvedType

  override def clone = IncrementPrefix(internalE.clone.asInstanceOf[Expression])

  override def isConstant = false
}


case class Ptr(var internalE: Expression) extends PostFixUnaryOp("*", 1) {
  override def clone = Ptr(internalE.clone().asInstanceOf[Expression])

  override def computeType: Type = e match {
    case f: FunDecl => f.mtype match {
      case t: PointerType => t.to
      case _ => null
    }
    case _ => e.resolvedType.asInstanceOf[PointerType].to
  }

  override def isConstant = false

  override def isSimple = false

  override def printMe(): String = "*" + e.printExpr(priority)

  override def isLvalue = e.resolvedType.asInstanceOf[PointerType].to.resolvedType match {
    case s: Struct => true
    case td: TypeDefReference => true // bonne idée?
    case i: IntType => true
    case i: CharType => true
    case p: PointerType => true
    case _ => false
  }

  def updateBaseExpr(expr: Expression): Ptr = internalE match {
    case e: Ptr => e.updateBaseExpr(expr); this
    case _ => this.internalE = e; this
  }

  def baseExpr: Expression = internalE match {
    case e: Ptr => e.baseExpr
    case _ => internalE
  }
}


case class UPlus(var internalE: Expression) extends PostFixUnaryOp("+", 3) {
  override def clone = UPlus(internalE.clone().asInstanceOf[Expression])

  override def isConstant = e.isConstant

  override def printMe() = "+" + e

  override def computeType: Type = {
    if (!e.resolvedType.isInstanceOf[IntType]) throw ASTException("Type of expression must be an integer")
    e.outputType
  }
}


abstract class SizeOf() extends UnaryOp("sizeof", 1) {
  def computeType: Type = IntType(32, signed = false)

  def toConstant = {
    val v = value
    Constant(v, IntType(32, signed = false), v.toString)
  }

  def value: Long
}

case class SizeOfE(var internalE: Expression) extends SizeOf {
  override def clone = SizeOfE(internalE.clone())

  override def printMe(): String = "sizeof " + e

  override def isConstant = true

  override def value: Long = e.resolvedType.resolvedType match {
    case t: Integral => t.getNbBytes / 8
    case t => throw new Exception("Unsupported type for sizeof : " + t)
  }
}

case class SizeOfT(var ctype: Type) extends SizeOf {
  override def clone = SizeOfT(ctype)

  override def isConstant = true

  override def printMe(): String = "sizeof (" + ctype.name + ")"

  override def internalE: Expression = null

  override def children: List[AstNode] = Nil

  override def internalE_=(x: Expression): Unit = throw new Exception("SizeOfT contains only type ref")

  override def value: Long = ctype.resolvedType match {
    case t: Integral => t.getNbBytes / 8
    case t => throw new Exception("Unsupported type for sizeof : " + t)
  }
}


case class FieldAccess(
                        private var _e: Expression,
                        private val _field: String)
  extends PostFixOp(2) {
  override def clone = new FieldAccess(e.clone().asInstanceOf[Expression], field)

  override def baseExpression: Expression = e

  override def operator: String = "." + field

  override def isConstant = false

  override def children: List[AstNode] = List(e)

  override def isSimple = false

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == e) {
      e = by.asInstanceOf[Expression]
    }
    else throw ASTException(" AST " + child + " is not a subtree of " + this)
  }

  def field = _field

  def e = _e

  def e_=(ne: Expression) {
    if (e.father == this) e.father = null
    _e = ne
    e.father = this
  }

  assert(e != null && field != null)
  e.resolvedType.resolvedType match {
    case x: StructOrUnion =>
    case _ => throw new ASTException("Expression in indirect field access must be a pointer to structure or union")
  }
  e.father = this

  override def computeType: Type = {
    val structType = e.resolvedType.asInstanceOf[StructOrUnion]
    structType.typeOf(field)
  }

  override def isLvalue: Boolean = true

  override def equals(obj: Any): Boolean = obj match {
    case t: FieldAccess => obj.getClass.getName.eq(this.getClass.getName) && e.eq(t.e) && field.eq(t.field) //&&this.father==t.father
    case _ => false
  }
}


case class IndirectFieldAccess(
                                private var _e: Expression,
                                private val _field: String)
  extends PostFixOp(2) {
  override def isLvalue: Boolean = true

  override def baseExpression: Expression = e

  override def operator: String = "->" + field

  override def isConstant = false

  override def isSimple = false

  override def children: List[AstNode] = List(e)

  override def clone = new IndirectFieldAccess(e.clone().asInstanceOf[Expression], field)

  def field = _field

  def e = _e

  def e_=(ne: Expression) {
    if (e.father == this) e.father = null
    _e = ne
    e.father = this
  }

  assert(e != null && field != null)
  e.resolvedType match {
    case PointerType(x) => x.resolvedType.resolvedType match {
      case t: StructOrUnion =>
      case _ => throw new ASTException("Expression in indirect field access must be a pointer to structure or union")
    }
    case _ => throw new ASTException("Expression in indirect field access must be a pointer to structure or union")
  }
  e.father = this

  override def computeType: Type = {
    val structType = e.resolvedType.asInstanceOf[PointerType].to.resolvedType.asInstanceOf[StructOrUnion]
    structType.typeOf(field)
  }

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == e) {
      e = by.asInstanceOf[Expression]
    }
    else throw ASTException(" AST " + child + " is not a subtree of " + this)
  }

  override def equals(obj: Any): Boolean = obj match {
    case t: IndirectFieldAccess => obj.getClass.getName.eq(this.getClass.getName) && e.eq(t.e) && field.eq(t.field) //&&this.father==t.father
    case _ => false
  }
}

case class Parenthesis(var internalE: Expression) extends UnaryOp("()", 11) {
  override def clone = Parenthesis(e.clone().asInstanceOf[Expression])

  override def computeType: Type = {
    e.computeType
  }

  override def printMe() = "(" + e + ")"

  override def isLvalue: Boolean = e.isLvalue

  override def isConstant = e.isConstant
}

case class Variable(declaration: VarDecl) extends Expression(12) with terminalASTNode {
  override def clone = Variable(declaration)

  override def isConstant = false

  override def isLvalue: Boolean = true

  override def printMe(): String = name

  override def computeType: Type = declaration.declaredType

  override def equals(obj: Any) = obj match {
    case t: Variable => super.equals(t) && t.declaration.equals(declaration)
    case _ => false
  }

  def name = declaration.name
}

case class EnumElementExpr(enumElem: EnumElement) extends Expression(10) with terminalASTNode {
  override def clone = EnumElementExpr(enumElem)

  override def isLvalue: Boolean = false

  override def isConstant = true

  override def printMe(): String = enumElem.name

  override def computeType: Type = enumElem.enumType

  override def equals(obj: Any) = obj match {
    case t: EnumElementExpr => super.equals(t) && t.enumElem.equals(enumElem)
    case _ => false
  }
}

case class Constant(
                     value: Any,
                     constType: Type,
                     var parserString: String = null)
  extends Expression(12) with terminalASTNode {
  override def clone = Constant(value, constType, parserString)

  override def isConstant = true

  override def computeType: Type = constType

  override def isLvalue: Boolean = false

  override def printMe(): String = parserString

  if (parserString == null) {
    parserString = value.toString
  }

  def asLong: Long = {
    def _asLong(ctype: Type) = value match {
      case x: Int => x.toLong
      case x: Long => x
      case x: String => x.toLong
      case _ => throw ASTException("Type not supported in constant : " + constType)
    }

    constType match {
      case t: TypeDefReference => _asLong(t.resolvedType)
      case t => _asLong(t)
    }
  }

  override def equals(obj: Any) = obj match {
    case t: Constant => super.equals(t) && t.value.equals(value) && t.constType.equals(constType) && t.parserString.equals(parserString)
    case _ => false
  }
}


case class Cast(
                 private var _e: Expression,
                 to: Type)
  extends PrefixOp(9) {
  override def isConstant = e.isConstant

  override def clone = Cast(e.clone().asInstanceOf[Expression], to)

  override def children: List[AstNode] = List(e)

  override def isLvalue: Boolean = false

  override def baseExpression: Expression = e

  override def operator: String = "(" + to.printValType(0, head = false).stripSuffix(" ") + ")"

  override def computeType: Type = {
    val ty = to.resolvedType
    if (ty.isInstanceOf[primitiveType] && !ty.isInstanceOf[PointerType]) to
    else null // only basic type supported
  }

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == e) {
      e = by.asInstanceOf[Expression]
    }
    else throw ASTException(" AST " + child + " is not a subtree of " + this)
  }

  def e = _e

  def e_=(a: Expression) {
    if (e.father == this) e.father = null
    _e = a
    e.father = this
  }

  _e.father = this
}

//TODO: call could be done on pointer, so need to support expression as Target.
case class Call(
                 var target: FunDecl,
                 var params: List[Expression])
  extends Expression(2) with ASTList[Expression] with Observer {
  override def isConstant = false

  override def computeType: Type = target.funType.returnType

  override def isLvalue: Boolean = false

  override def clone = Call(target, params.map(t => t.clone().asInstanceOf[Expression]))

  override protected def _listen(sender: Subject, e: Event) = check()

  override def equals(obj: Any) = obj match {
    case t: Call => super.equals(t) && t.params.equals(params) && t.target.equals(target)
    case _ => false
  }

  override def printMe(): String = target.name + "(" + params.map(_.printMe()).mkString(",") + ")"

  override def list: List[Expression] = params

  override def list_=(a: List[Expression]) {
    params = a
  }

  def check() = {
    for ((to, from) <- target.funType.parameters.zip(params)) {
      if (!from.outputType.canBeConvertedTo(to.declaredType)) {
        throw ASTException(from + "(type=" + from.outputType + ") cannot be converted to " + to + "(type=" + to.declaredType + ")")
      }
    }
  }

  def extractComplexArgs(tvg: VariableGenerator) {
    def extract(param: Expression, paramType: Type) {
      val repl = tvg.next("tmp", paramType)
      param.replaceBy(repl.asVariable)
      repl.init = Some(param)
      this.enclosing[Instruction].get.insertBefore(repl)
    }

    (this.target.arguments.map(_.declaredType), params).zipped foreach { (y, x) => if (x.isComplex) extract(x, y) }
  }

  notifyAllInserts()
  check()
  target.registerCall(this)
}
