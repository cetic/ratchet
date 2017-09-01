package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.ast.{EnumDecl, TypeDecl, _}

class ParsingStack(logger: Logger = Logger.getLogger("be.cetic.ratchet.be.cetic.ratchet.reader.helpers.ParsingStack")) {
  var stack: List[DeclarationScope] = List.empty

  override def toString = stack.last.declarations.toString

  /** declarations are to be added to the topmos gtASTNode, not via this class. */
  def push(d: DeclarationScope) {
    stack = d :: stack
  }

  def addDeclaration(decl: Declaration) = {
    if (decl.name != null && !decl.name.isEmpty) {
      /*if(isDeclared(decl.name)){
        throw new Exception("Already declared " + decl.name + " in this scope")
      }*/
      logger.log(Level.FINEST, "Adding decl of (" + decl + ") into parsing stack")
      decl match {
        case e: EnumDecl =>
          stack.head.addDeclaration(decl)
          e.declaredEnum.elements.foreach(el => stack.head.addDeclaration(el))
        case _ => stack.head.addDeclaration(decl)
      }
    }
  }

  def pop() = {
    stack = stack.tail
  }

  def get(name: String): Option[Declaration] = {
    val result = _get(name, stack)
    logger.log(Level.FINEST, "Get decl of (" + name + ") as (" + result + ")");
    result
  }

  private def _get(name: String, rstack: List[DeclarationScope]): Option[Declaration] = {
    if (rstack.isEmpty) None
    else {
      val decl = rstack.head.find(name)
      if (decl.isDefined) {
        decl
      }
      else {
        _get(name, rstack.tail)
      }
    }
  }

  def getStructure(name: String): Option[StructDecl] = get(name) match {
    case None => None
    case Some(x) => x match {
      case s: StructDecl => Some(s)
      case _ => None
    }
  }


  def getUnion(name: String): Option[UnionDecl] = get(name) match {
    case None => None
    case Some(x) => x match {
      case s: UnionDecl => Some(s)
      case _ => None
    }
  }

  def getEnum(name: String): Option[EnumDecl] = get(name) match {
    case None => None
    case Some(x) => x match {
      case s: EnumDecl => Some(s)
      case _ => None
    }
  }

  def getEnumElement(name: String): Option[EnumElement] = get(name) match {
    case None => None
    case Some(x) => x match {
      case s: EnumElement => Some(s)
      case _ => None
    }
  }

  def isEnumElement(name: String): Boolean = {
    getEnumElement(name) match {
      case Some(x) => true
      case None => false
    }
  }

  def isTypeName(name: String): Boolean = {
    val d = get(name)
    d match {
      case None => false
      case Some(x) =>
        x match {
          case e: EnumDecl => true
          case t: TypeDecl => true
          case s: StructDecl => true
          case s: Struct => true
          case s: StructReference => true
          case _ => false
        }
      case _ => throw new Exception("Not declared : " + name);
    }
  }

  def toReference(name: String): NewTypeSpecifier = {
    get(name) match {
      case None => throw new Exception("type not exists : " + name)
      case Some(x) =>

        x match {
          case e: EnumDecl => logger.log(Level.FINEST, "Get reference of (" + name + ") to enum"); EnumReference(e)
          case t: TypeDecl => logger.log(Level.FINEST, "Get reference of (" + name + ") to typedef"); TypeDefReference(t)
          case s: StructDecl => logger.log(Level.FINEST, "Get reference of (" + name + ") to struct"); StructReference(s)
          case s: StructReference => logger.log(Level.FINEST, "Get reference of (" + name + ") to struct"); s
          case s: EnumReference => logger.log(Level.FINEST, "Get reference of (" + name + ") to struct"); s
          case _ => throw new Exception("Not supported")
        }
    }
  }

  def isDeclared(name: String) = get(name).isDefined
}
