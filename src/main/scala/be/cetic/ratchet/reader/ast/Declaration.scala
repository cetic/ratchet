package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.helpers.{ASTException, VariableGenerator, Visitable}
import be.cetic.ratchet.utils.{Event, Observer, Subject}

import scala.collection.immutable.SortedMap


case class NoDeclarationScopeException(decl: Declaration, details: String, cause: Throwable = null)
  extends Exception("No declaration scope for declaration : " + decl.name + " as type " + decl.getClass.getName + " " + details, cause)

//a decl is an instruction, so they can be set together in block.
abstract class Declaration(val extern: Boolean) extends Instruction() {

  def name: String

  override def toString: String = printInstr(0, head = false)

  def firstInstruction: List[Instruction] = List(this)

  def lastInstruction: List[Instruction] = List(this)

  override def equals(obj: Any) = obj match {
    case t: Declaration => super.equals(t) && t.name.equals(name) && t.extern == extern
    case _ => false
  }

  def canbeMultipleDeclared: Boolean = false

  def declarationscope = this.enclosingOrDie[DeclarationScope]

}

case class StructPart(decl: VarDecl) extends Visitable {
  if (decl == null) throw ASTException("Declaration must be not null")

  def name = decl.name

  def declType = decl.declaredType

  def printInstr(indent: Int, head: Boolean): String = decl.printInstr(indent, head)
}

case class StructDecl(var struct: Struct) extends Declaration(false) {

  override def name = struct.name

  override def children = Nil //List(Struct).asInstanceOf[List[AstNode]]

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = struct.printVarType(indent, head = true, "") + ";"


  override def canbeMultipleDeclared = true
}

case class UnionDecl(var union: Union) extends Declaration(false) {

  override def name = union.name

  override def children = List(Union).asInstanceOf[List[AstNode]]

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = union.printVarType(indent, head = true, "") + ";"

  override def canbeMultipleDeclared = true
}

case class TypeDecl(override val name: String, var declaredType: Type) extends Declaration(false) with NestedBaseType {
  override def children = Nil //List(declaredType).asInstanceOf[List[AstNode]]

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = "typedef " + declaredType.printVarType(indent, head, name) + ';'


  override def equals(n: Any) = {
    super.equals(n) && declaredType.equals(n.asInstanceOf[TypeDecl].declaredType)
  }

  override def baseType: Type = declaredType match {
    case t: NestedBaseType => t.baseType
    case _ => declaredType
  }


  override def canbeMultipleDeclared = true


  private def lookForUsage(t: Type): Option[TypeDefReference] = t match {
    case t: TypeDefReference if t.name == this.name => Some(t)
    case PointerType(to) => lookForUsage(to)
    case TypeDefReference(to) => lookForUsage(to.baseType)
    case _ => None
  }

  def usages: List[TypeDefReference] =
    this.declarationscope.descendantsOfType[Declaration]()
      .flatMap {
        case d: StructDecl => d.struct.fields.map(_.decl)
        case x: FunDef => Nil
        case x => List(x)
      }
      .map {
        case f: FunDecl => lookForUsage(f.funType.returnType)
        case d: VarDecl => lookForUsage(d.declaredType)
        case t: TypeDecl => lookForUsage(t.declaredType)
      }
      .filter(_.isDefined)
      .map(_.get)

  def replaceByWithUsages(by: TypeDecl) {
    usages.foreach(t => t.to = by)
    checkFather
    father.replaceChild(this, by)
  }
}

case class EnumDecl(override val name: String, declaredEnum: Enum) extends Declaration(false) {
  //override def children = List(declaredEnum).asInstanceOf[List[AstNode]]
  override def children = Nil

  def this(e: Enum) = this(e.name, e)

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = declaredEnum.printVarType(indent, head, "") + ";"
}


case class EnumElement(override val name: String, value: Option[Int] = None) extends Declaration(false) {

  var _enumType: Enum = null

  def enumType = _enumType

  def enumType_=(t: Enum) {
    _enumType = t
  }

  override def toString: String = name + (value match {
    case None => "";
    case Some(x) => " = " + x
  })

  override def children: List[AstNode] = Nil

  /**
    * builds the string representation of an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = throw new Exception("illegal use")
}


case class VarDecl(var name: String, var static: Boolean = false, override val extern: Boolean = false, var declaredType: Type = IntType(), var initializer: Option[Expression] = None)
  extends Declaration(extern) {

  def init = initializer

  def init_=(a: Option[Expression]) {
    init match {
      case Some(e) => e.father = null
      case _ =>
    }
    initializer = a
    a match {
      case Some(e) => e.father = this
      case _ =>
    }
  }

  init = initializer

  override def clone = init match {
    case Some(i) => VarDecl(name, static, extern, declaredType, Some(i.clone))
    case None => VarDecl(name, static, extern, declaredType, None)
  }

  var brothers = SortedMap[String, VarDecl]()

  override def equals(n: Any): Boolean = {
    super.equals(n) && (extern == n.asInstanceOf[VarDecl].extern) && (static == n.asInstanceOf[VarDecl].static) && declaredType.equals(n.asInstanceOf[VarDecl].declaredType)
  }

  private var mActualDeclaration: VarDecl = null

  def actualDeclaration: VarDecl = mActualDeclaration

  def actualDeclaration_=(d: VarDecl) {
    assert(this.name == d.name)
    assert(this.declaredType equals d.declaredType)
    mActualDeclaration = d
  }

  def asVariable = Variable(this)

  def duplicateWithPrefix(prefix: String) = {
    insertAfter(generateBrothers(prefix))
  }

  def isAnonymous = name.isEmpty

  def duplicateWithPrefix(prefix: String, f: (Type) => Type) = {
    generateBrothers(prefix, f)
    if (Logger.getLogger("Hardener").isLoggable(Level.INFO)) {
      Logger.getLogger("Hardener").warning("Duplicating with prefix : " + this +
        " by inserting " + brothers(prefix)
      )
    }
  }


  def generateBrothers(prefix: String, f: (Type) => Type) = init match {
    case Some(v@Constant(_, y, _)) =>
      brothers = brothers + ((prefix, VarDecl(prefix + name, static, extern, f(declaredType),
        Some(Constant(~v.asLong, y, (~v.asLong).toString)))))
      insertAfter(brothers.get(prefix).get)
    case Some(x) =>
      brothers = brothers + ((prefix, VarDecl(prefix + name, static, extern, f(declaredType))))
      insertAfter(brothers.get(prefix).get)
    case None =>
      brothers = brothers + ((prefix, VarDecl(prefix + name, static, extern, f(declaredType))))
      insertAfter(brothers.get(prefix).get)
  }

  def generateBrothers(prefix: String) = {
    val brother = prefix -> VarDecl(prefix + name, static, extern, declaredType)
    brothers = brothers + brother
    brother._2
  }

  override def children = init match {
    case Some(x) => List(x)
    case None => List()
  }

  def resolvedType = declaredType.resolvedType

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String =
    SpacesAsHead(indent, head) + (if (extern) "extern " else "") + (if (static) "static " else "") + declaredType.printVarType(indent, head = false, name) + (if (init.isDefined) " = " + init.get else "") + (if (head) ";" else "")

  override def replaceChild(child: AstNode, by: AstNode) = {
    try {
      this.init match {
        case Some(e) => e.father = null
        case _ =>
      }
      this.init = Some(by.asInstanceOf[Expression])

      by.father = this
    }
    catch {
      case e: ClassCastException => throw ASTException("The replacement of child of a variable declaration must be an expression")
    }
  }


  def simplifyInit(tvg: VariableGenerator) = {
    Logger.getLogger("Instruction").log(Level.INFO, "Simplifying vardecl init " + this)
    init match {
      case Some(expr) => expr.children.foreach(e => e.asInstanceOf[Expression].extractFrom(this, tvg))
      case _ =>
    }
  }
}


case class FunDecl(
                    var name: String,
                    var mtype: Type,
                    override val extern: Boolean = false
                  )
  extends Declaration(extern) with Subject with Observer {

  override def children = List(funType).asInstanceOf[List[AstNode]]

  funType.father = this

  def funType = mtype match {
    case x: FunctionType => x
    case x: PointerType => x.baseType match {
      case y: FunctionType => y
      case _ => throw new Exception("Type for function declaration must be function ")
    }
    case _ => throw new Exception("Type for function declaration must be function ")
  }

  def funType_=(t: Type) {
    mtype match {
      case x: FunctionType => mtype = t
      case x: PointerType => mtype = TypeUtils.generateNestedPointer(x.depth, t)
      case _ => throw new Exception("Type for function declaration must be function ")
    }
  }

  def rename(newName: String) = name = newName

  override def equals(n: Any) = {
    super.equals(n) && (extern == n.asInstanceOf[FunDecl].extern) && funType.equals(n.asInstanceOf[FunDecl].funType)
  }

  //here is the binding performed, to ensure that we are able to link funcion declaration to definition
  private var mfunDef: FunDef = null
  private var mcalls: List[Call] = Nil

  def funDefOption: Option[FunDef] = if (mfunDef == null) None else Some(mfunDef)

  def funDef: FunDef = mfunDef

  def funDef_=(a: FunDef) {
    mfunDef = a
    funType.funDef = a
    //TODO: check type of a
  }

  def arguments = funType.list

  def isPointerToFunction = mtype.isInstanceOf[PointerType]


  def calls = mcalls

  def registerCall(call: Call) = {
    mcalls = mcalls ::: call :: Nil
    this.register(call)
  }

  override def replaceBy(by: AstNode) = {
    father match {
      case funDef: FunDef => by match {
        case decl: FunDecl => funDef.replaceBy(FunDef(decl, funDef.instructions))
        case _ => throw ASTException("Only function decl in a function definition")
      }
      case _ => super.replaceBy(by)
    }
  }

  def insertNewArgs(decls: VarDecl*) = funType.insertNewArgs(decls: _*)

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = father match {
    case x: FunDef => funType.printVarType(indent, head, name)
    case _ => mtype.printVarType(indent, head, name) + ";" // just function decl
  }

  override final def _listen(sender: Subject, e: Event) = notifyObservers(e)

  /**
    *
    * @param varName name of the return variable
    * @param tvg     temporary variable for call
    */
  def convertToProcedure(varName: String, tvg: VariableGenerator) {
    try {
      val returnType = this.funType.returnType
      if (returnType.isInstanceOf[Void]) return

      val returnVar = VarDecl(varName, false, false, PointerType(returnType))
      funType.insertNewArgs(returnVar)
      funType.returnType = Void()

      if (funDef != null) {
        funDef.instructions.descendantsOfType[Return]().foreach(ret => {
          ret.value match {
            case Some(x) =>
              ret.value = None
              ret.insertBefore(ExpressionInstr(Affect(Ptr(returnVar.asVariable), x)))
            case None =>
          }
        })

        funDef.ref match {
          case Some(x) =>
            x.funType.insertNewArgs(VarDecl(varName, false, false, PointerType(returnType)))
            x.funType.returnType = Void()
          case None =>
        }
      }
    }
    catch {
      case e: Throwable => throw new ASTException("Cannot transform function to procedure " + this + " with calls " + calls.map(_.printMe()).mkString(","), e)
    }

  }
}

trait NoDoubleDeclare extends DeclarationScope {
  override def addDeclaration(d: Declaration) {
    assert(!this.declared.isDefinedAt(d.name), d.name + " already declared")
    declared = declared + ((d.name, d))
  }
}

/** this declares a scope of declaration; can be used for procedure parameters, global vars, and blocks */
trait DeclarationScope extends AstNode {
  final var declared: SortedMap[String, Declaration] = SortedMap.empty

  def cleanDecls() = declared = SortedMap.empty

  def addDeclaration(d: Declaration) {
    if (d == null) {
      throw new ASTException("Attempt to add null declaration")
    }
    if (d.name == null) {
      throw new ASTException("Attempt to add declaration " + d + " with null name")
    }
    declared = declared + ((d.name, d))
  }

  final def deleteDeclaration(name: String) {
    declared = declared - name
  }

  final def get(name: String): Declaration = declared(name)

  final def isDefined(name: String): Boolean = declared.isDefinedAt(name)

  final def find(name: String): Option[Declaration] = {
    if (isDefined(name)) {
      Some(get(name))
    } else {
      //This also works for FunDEf, ad FunDef are DeclarationScope.
      this.enclosing[DeclarationScope].map(x => x.findOrDie(name))
    }
  }

  final def findOrDie(name: String): Declaration = this.find(name) match {
    case Some(x) => x
    case None => throw new Exception(name + " not declared in "
      + this.allDeclarations.mkString(", "))
  }

  def allDeclarations: List[String] = declared.keys.toList ::: (
    this.enclosing[DeclarationScope] match {
      case None => Nil.asInstanceOf[List[String]]
      case Some(declScope: DeclarationScope) => declScope.allDeclarations
    })

  def declarations: Iterable[Declaration] = declared.values
}
