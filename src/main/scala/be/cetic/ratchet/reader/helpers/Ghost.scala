package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast.{ArrayType, VarDecl, _}

/**
  * This file contains tools to be used by the parser to compute the type or a declaration
  *
  * An AST contains these entities only during parsing. It must no contain one in the AST return by the parser.
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */


/**
  * Used during parsing and for debugging purpose
  *
  * It qualifies a variable or a pointer to be a ghost of a real variable
  * of pointer.
  *
  * A ghost variable or pointer has a name or a type that can be
  * determined at the moment of the declaration.
  *
  */
case class Ghost() extends primitiveType with Integral {
  def printVarType(indent: Int, head: Boolean, varname: String): String = {
    if (!varname.isEmpty) throw ASTException("ghost variable name must be empty")
    "ghost"
  }

  def getNbBytes = 0

  def getSigned = false

  override def name = "__Ghost__"

  override def toSigned: Type = this

  override def toUnsigned: Type = this
}


/**
  * Temporary array definition.
  *
  * @param e
  */
case class ArrayDef(e: Expression) extends Expression(1) {
  override def printMe(): String = "ghost"

  override def isLvalue: Boolean = true

  /** This computes the output type of the expression.
    * it might throw an exception in case parameter types are unsupported.
    * if returns false, an exception is thrown by the system
    */
  override def computeType: Type = ArrayType(Ghost(), None)

  override def children: List[AstNode] = List(e)

  override def isConstant = false

}

/**
  * Untyped nested pointer.
  *
  * @param depth
  */
case class PointerTemplate(depth: Int)

/**
  * Template of a declarator before its type computation
  *
  *
  */
trait DeclaratorTemplate {
  def completeDeclaration(declSpec: List[TypePart]): Declaration

  def name: String
}

case class Identifier(str: String) extends DeclaratorTemplate {

  override def completeDeclaration(declSpec: List[TypePart]): Declaration = {
    DeclarationFactory.createDeclaration(declSpec, None, None)
  }

  def name = str
}

case class Anonymous() extends DeclaratorTemplate {

  override def completeDeclaration(declSpec: List[TypePart]): Declaration = {
    DeclarationFactory.createDeclaration(declSpec, None, None)
  }

  def name = "" // throw new Exception("Anonymous has no name !!")
}

case class SuffixedDeclarator(
                               declarator: DeclaratorTemplate,
                               suffixes: List[DeclaratorSuffix],
                               pointer: Option[PointerTemplate]
                             ) extends DeclaratorTemplate {

  if (declarator == null) {
    throw new ASTException("Declarator cannot be null for suffixed declarator, used Anonymous class instance instead")
  }

  def completeDeclaration(declSpec: List[TypePart]): Declaration = {
    DeclarationFactory.createDeclaration(declSpec, Some(this), None)
  }

  def name = declarator.name

}

case class InitDeclarator(decl: DeclaratorTemplate, init: Option[Expression]) extends DeclaratorTemplate {
  def completeDeclaration(declSpec: List[TypePart]): Declaration = {
    DeclarationFactory.createDeclaration(declSpec, Some(this), init)
  }

  def name = decl.name
}

trait DeclaratorSuffix

case class FunctionDeclaration(private var _args: List[VarDecl]) extends DeclaratorSuffix {

  //_args=_args.filterNot(p => p.declaredType.isInstanceOf[Void])

  def args = _args
}

case class ArrayDeclaration(size: Option[Expression]) extends DeclaratorSuffix {
  def intSize: Option[Int] = size match {
    case None => None
    case Some(x) => x match {
      case c: Constant => Some(c.asLong.toInt)
      case other => throw new Exception("Not supported array size " + other)
    }
  }
}


case class PostFixOp(opstr: String) extends DeclaratorSuffix


case class TypeQualifier(name: String) extends TypePart {
  override def toString = name
}

case class TypeScope(name: String) extends TypePart {
  override def toString = name
}

case class SignedSpecifier(signed: Boolean) extends TypeSpecifier

case class TypeDefSpecifier() extends TypeSpecifier


case class StringSpecifier(part: String) extends TypeSpecifier {
  val auth = List("void", "char", "int", "float", "double", "long", "short", "_Bool")

  if (!auth.contains(part)) throw new Exception(part + " not authorized")
}

