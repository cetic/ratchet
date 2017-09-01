package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.helpers.ASTException

import scala.collection.SortedMap

trait TypePart

trait TypeSpecifier extends TypePart

trait NewTypeSpecifier extends TypeSpecifier

trait Type extends printHelpers with AstElement with TypeSpecifier with Cloneable {
  /** returns the real type of the thing, resolving typedefs, struct and enum dereference
    * this type cannot be modified */
  def resolvedType: primitiveType

  def canBeConvertedTo(t: Type): Boolean = true

  // All convertion is acceptable in C, type is just a canvas
  //this.resolvedType == t.resolvedType //TODO

  def printVarType(indent: Int, head: Boolean, varname: String): String

  def printValType(indent: Int, head: Boolean): String = printVarType(indent, head, "")

  override def clone: Object = {
    null
  }

  def name: String

  def toSigned: Type = this match {
    case t: Integral => t.toSigned
    case _ => this
  }

  def toUnsigned: Type = this match {
    case t: Integral => t.toUnsigned
    case _ => this
  }
}

/** integral is everything that is represented by an integral, including pointers */
trait Integral {
  def getNbBytes: Int

  def getSigned: Boolean

  def toSigned: Type

  def toUnsigned: Type
}


trait NestedBaseType {
  def baseType: Type
}

/** primitive types are the ones that are not typedefs */
trait primitiveType extends Type {
  override def resolvedType = this
}


object TypeUtils {
  def generateNestedPointer(nb: Int, toType: Type): Type = {
    if (nb == 0) toType
    else PointerType(generateNestedPointer(nb - 1, toType))
  }
}


case class Void() extends primitiveType {
  def printVarType(indent: Int, head: Boolean, varname: String): String = {
    "void " + varname
  }

  override def toString = name

  override def clone = Void()

  override def name = "void"
}


case class IntType(nbByte: Int = 32, signed: Boolean = false) extends primitiveType with Integral {
  override def toString: String = (if (!signed) "unsigned " else "signed ") + (nbByte match {
    case 15 | 16 => "short int"
    case 31 | 32 => "long int"
    case 63 | 64 => "long long int"
  })

  def getNbBytes = nbByte

  def getSigned = signed

  override def equals(other: Any) = other match {
    case IntType(n, s) => n == this.nbByte && s == this.signed
    case _ => false
  }

  override def name = toString

  override def clone = IntType(nbByte, signed)

  def printVarType(indent: Int, head: Boolean, varname: String): String = toString + " " + varname

  override def toSigned = IntType(nbByte, true)

  override def toUnsigned = IntType(nbByte, false)
}

case class BoolType() extends primitiveType with Integral {
  override def toString: String = "_Bool"

  override def getNbBytes = 32

  override def getSigned = true

  override def name = toString

  override def equals(other: Any) = other match {
    case BoolType() => true
    case _ => false
  }

  override def clone = BoolType()

  override def toSigned = BoolType()

  override def toUnsigned = BoolType()

  def printVarType(indent: Int, head: Boolean, varname: String): String = toString + " " + varname
}

case class CharType(signed: Boolean = false) extends primitiveType with Integral {
  override def toString: String = (if (!signed) "unsigned " else "signed ") + "char"

  override def getNbBytes = 8

  override def getSigned = signed

  override def equals(other: Any) = other match {
    case CharType(s) => s == this.signed
    case _ => false
  }

  override def clone = CharType(signed)

  override def toSigned = CharType(signed = true)

  override def toUnsigned = CharType(signed = false)

  override def name = toString

  def printVarType(indent: Int, head: Boolean, varname: String): String = toString + " " + varname
}


case class PointerType(to: Type) extends primitiveType with Integral with NestedBaseType {
  override def name = to.toString + "*"

  override def getNbBytes = 32

  override def getSigned = false

  override def printValType(indent: Int, head: Boolean): String = "*" + to.printValType(indent, head)

  override def clone = PointerType(to)

  override def baseType: Type = to match {
    case x: PointerType => x.baseType
    case _ => to
  }

  override def equals(other: Any) = other match {
    case o: PointerType => to.equals(o.to)
    case _ => false
  }

  override def toSigned: Type = throw new ASTException("Pointer cannot by signed")

  override def toUnsigned: Type = this

  def depth: Int = to match {
    case x: PointerType => 1 + x.depth
    case _ => 1
  }

  def printVarType(indent: Int, head: Boolean, varname: String): String = to.printVarType(indent, false, "*" + varname)
}

/** when you use a typedef, this is the type that is actually stored
  * to geet the resolved ttype, you must get the resolved Type */
case class TypeDefReference(var to: TypeDecl) extends Type with NewTypeSpecifier with NestedBaseType {
  override def resolvedType: primitiveType = to.declaredType.resolvedType

  override def equals(other: Any) = other match {
    case o: TypeDefReference => to.name.equals(o.to.name)
    case _ => false
  }

  override def clone = TypeDefReference(to)

  override def baseType: Type = to.baseType

  override def name = to.name

  def printVarType(indent: Int, head: Boolean, varname: String): String = name + " " + varname
}

case class StructReference(to: StructDecl) extends Type with NewTypeSpecifier {
  override def resolvedType: primitiveType = to.struct

  override def clone = StructReference(to)

  override def name = "struct " + to.name

  def printVarType(indent: Int, head: Boolean, varname: String): String = "struct " + to.name + " " + varname
}

case class UnionReference(to: UnionDecl) extends Type with NewTypeSpecifier {
  override def resolvedType: primitiveType = to.union

  override def clone = UnionReference(to)

  override def name = "union " + to.name

  def printVarType(indent: Int, head: Boolean, varname: String): String = "union " + to.name + " " + varname
}

case class EnumReference(to: EnumDecl) extends Type with NewTypeSpecifier {
  override def resolvedType: primitiveType = to.declaredEnum

  override def clone = EnumReference(to)

  override def name = "enum " + to.name

  def printVarType(indent: Int, head: Boolean, varname: String): String = "enum " + to.name + " " + varname
}

case class ArrayType(of: Type, size: Option[Int]) extends primitiveType with NestedBaseType {

  size match {
    case Some(n) => assert(n > 0)
    case None =>
  }

  override def name = of.name + suffix

  override def clone = ArrayType(of, size)


  override def baseType: Type = of match {
    case t: NestedBaseType => t.baseType
    case _ => of
  }

  def suffix: String = size match {
    case Some(n) => "[" + n + "]"
    case None => "[]"
  }

  def printVarType(indent: Int, head: Boolean, varname: String): String = of.printVarType(indent, false, varname + suffix)
}

//int a[i][j]
//le premier index donné est i, le second est j.
//on a donc un tableau de taille i de tableaux de taille j d'entiers

case class Enum(
                 private var _elements: List[EnumElement],
                 private var _name: String)
  extends primitiveType with Integral {
  override def toString: String = "enum" + (if (!name.isEmpty) " " + name else "")

  override def getNbBytes = 32

  override def getSigned = false

  override def clone = Enum(elements, name)

  override def toSigned: Type = throw new ASTException("Enum cannot by signed")

  override def toUnsigned: Type = this

  def name = _name

  def name_=(n: String) {
    name = n
  }

  def elements = _elements

  def elements_=(es: List[EnumElement]) {
    _elements = es
    elements.foreach(ee => ee.enumType = this)
  }

  def addElement(label: String, value: Option[Int]) {
    val ee = EnumElement(label, value)
    ee.enumType = this
    elements = elements ::: List(ee)
  }

  def instanciateValues(offset: Int = 0, inc: Int = 1) {
    var i = offset - inc
    this.elements = this.elements.map(ee => ee.value match {
      case Some(x) => ee
      case None => i = i + inc; EnumElement(ee.name, Some(i))
    })
  }

  def printVarType(indent: Int, head: Boolean, varname: String): String =
    "enum" + (if (!name.isEmpty) " " + name else "") + " {" + elements.mkString(",") + "}" + (if (!varname.isEmpty) " " + varname else "")
}


abstract class StructOrUnion extends primitiveType {
  private var _fields = SortedMap.empty[String, StructPart]
  declarations.foreach(addDeclaration)

  def name: String

  def name_=(n: String)

  def memAllocation: String

  def declarations: List[StructPart]

  def addDeclaration(declaration: StructPart) = {
    _fields = _fields + (declaration.name -> declaration)
  }

  def typeOf(name: String) = {
    if (_fields.contains(name)) {
      _fields(name).declType
    }
    else {
      throw ASTException("Field of name " + name + " does not exists in " + this)
    }
  }

  def fieldNames = _fields.keySet

  def fields = declarations

  def prefix = {
    val n = if (name != null && !name.isEmpty) " " + name else ""
    memAllocation + n
  }

  def printVarType(indent: Int, head: Boolean, varname: String): String =
    prefix + "\n" + Spaces(indent) + "{\n" +
      declarations.map(st => st.printInstr(indent + 2, head = true)).mkString("\n") +
      "\n" + Spaces(indent) + "}" + (if (varname.isEmpty) "" else " " + varname)
}

case class Struct(
                   private val _declarations: List[StructPart],
                   private var _name: String)
  extends StructOrUnion with NewTypeSpecifier {
  override def name: String = _name

  override def name_=(n: String) = {
    _name = n
  }

  override def memAllocation: String = "struct"

  override def declarations: List[StructPart] = _declarations

  override def clone = Struct(declarations, name)
}

case class Union(
                  private val _declarations: List[StructPart],
                  private var _name: String)
  extends StructOrUnion with NewTypeSpecifier {
  override def name: String = _name

  override def name_=(n: String) = {
    _name = n
  }

  override def memAllocation: String = "union"

  override def declarations: List[StructPart] = _declarations

  override def clone = Union(declarations, name)
}

case class FunctionType(
                         private var _returnType: Type,
                         private var _parameters: List[VarDecl] = List.empty
                       )
  extends primitiveType with ASTList[VarDecl] with NestedBaseType {
  private var void = false
  private var mfunDef: FunDef = null

  if (_parameters == null) {
    _parameters = List.empty
  }
  for (p <- parameters) {
    check(p)
  }
  notifyAllInserts()

  override def name = "(" + _parameters.mkString(",") + "):" + returnType.toString

  def parameters = _parameters

  def returnType = _returnType

  def returnType_=(t: Type) {
    _returnType = t
  }

  def funDef: FunDef = mfunDef

  def funDef_=(a: FunDef) { //fundef must register here to be notified about changes
    mfunDef = a
    notifyAllInserts()
  }

  def insertNewArgs(args: VarDecl*) {
    if (void) _parameters = Nil
    _parameters = _parameters ::: args.toList
    notifyAllInserts()
  }

  override def equals(other: Any) = other match {
    case x: FunctionType =>
      x.returnType.equals(this.returnType) &&
        parameters.size == x.parameters.size //&&
    //parameters.zip(x.parameters).filterNot(x=>x._1.equals(x._2)).size==0
    case _ => false
  }

  def lookForParameterDecl(name: String) = parameters.find(p => p.name == name)

  override def list: List[VarDecl] = parameters

  override def list_=(a: List[VarDecl]) {
    _parameters = a
  }

  override def clone = FunctionType(returnType, parameters)

  def check(arg: VarDecl) {
    if (arg.declaredType.isInstanceOf[Void]) {
      if (parameters.size > 1) throw ASTException("if args contains void, only this argument is authorized");
    }
  }

  override def inserted(listBefore: List[VarDecl], node: VarDecl, listAfter: List[VarDecl]) {
    if (void && !node.declaredType.isInstanceOf[Void]) {
      throw ASTException("if args contains void, only this argument is authorized : " +
        node + " cannot be inserted");
    }
    else {
      check(node)
      void = node.declaredType.isInstanceOf[Void] || void
      if (!node.declaredType.isInstanceOf[Void] && funDef != null) {
        funDef.addDeclaration(node)
      }
    }
  }

  def isVoid = void

  def unsetVoid {
    if (void) {
      void = false
      list.foreach(p => p.delete())
    }
  }

  override def deleted(listBefore: List[VarDecl], node: VarDecl, listAfter: List[VarDecl]) {
    if (funDef != null) funDef.deleteDeclaration(node.name)
    node.father = null
  }

  override def baseType: Type = returnType

  def printVarType(indent: Int, head: Boolean, varname: String): String = {
    val tmpName = if (varname.startsWith("*")) "(" + varname + ")" else varname
    if (void) {
      returnType.printValType(indent, head) + " " + tmpName + " (void)"
    }
    else {
      returnType.printValType(indent, head) + " " + tmpName + " (" + parameters.mkString(", ") + ")"
    }
  }

  override def toString: String = printValType(0, head = false)
}



