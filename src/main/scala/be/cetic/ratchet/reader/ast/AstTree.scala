package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.helpers.{ASTException, Visitable}


trait TypeInjector {
  def map(aType: Type): Type
}

class DefaultTypeInjector extends TypeInjector {

  def map(aType: Type): Type = aType match {
    case x => x
  }

}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class AstTree(aRoot: AstNode) extends Visitable {

  private var _root: AstNode = null
  private var _nodes: List[AstNode] = Nil
  private var _vardecls: List[VarDecl] = Nil
  private var _fundecls: List[FunDecl] = Nil
  private var _fundefs: List[FunDef] = Nil
  private var _conditionals: List[Instruction] = Nil
  private var _typedecls: List[TypeDecl] = Nil
  private var _structdecls: List[StructDecl] = Nil
  private var _calls: List[Call] = Nil
  private var _translationUnits: List[TranslationUnit] = Nil

  def nodes = _nodes

  def vardecls = _vardecls

  def fundecls = _fundecls

  def fundefs = _fundefs

  def conditionals = _conditionals

  def typedecls = _typedecls

  def structdecls = _structdecls

  def translationUnits = _translationUnits

  def calls = _calls

  def root = _root

  def root_=(r: AstNode) {
    _root = r
    link(root)
  }

  def check() = {
    def doit(parent: AstNode) {
      for (child <- parent.children) {
        if (child.father == null) throw ASTException("Father is null for node (child=" + child + ", expected father" + parent + ")")
        if (!child.father.eq(parent)) throw ASTException("Child have not the good father (child=" + child + ", expected father="
          + parent + ", actual=" + child.father + ")")
        if (!(_nodes contains child)) throw ASTException("Child is not contained into the father's tree (child=" + child
          + ", TreeRoot=" + this.root)
        doit(child)
      }
    }

    doit(root)
  }

  root = aRoot

  private def link(node: AstNode) = {
    def doit(node: AstNode) {
      if (node != null) {
        /*if(nodes.contains(node)) {
          throw new Exception("Adding existing nodes : " + node)
        }*/
        Logger.getLogger("AST").log(Level.FINEST, "Adding node of type " + node.getClass.getCanonicalName + " to tree value : \n" + node)
        _nodes = _nodes ::: node :: Nil
        node match {
          case v: VarDecl => _vardecls = v :: _vardecls
          case f: FunDecl => _fundecls = f :: _fundecls
          case f: FunDef => _fundefs = f :: _fundefs
          case i: IfThenElse => _conditionals = i :: _conditionals
          case w: While => _conditionals = w :: _conditionals
          case w: DoWhile => _conditionals = w :: _conditionals
          case s: SwitchCase => _conditionals = s :: _conditionals
          case f: For => _conditionals = f :: _conditionals
          case t: TypeDecl => _typedecls = t :: _typedecls
          case t: StructDecl => _structdecls = t :: _structdecls
          case c: Call => _calls = c :: _calls
          case t: TranslationUnit => _translationUnits = t :: _translationUnits
          case _ =>
        }

        node.tree = this
        node.children.foreach(doit)
      }
    }

    doit(node)
  }

  private def unlink(node: AstNode) = {
    def doit(node: AstNode): List[AstNode] = {
      if (node != null) {
        /*if(!nodes.contains(node)){
          throw new Exception("removing unexisting nodes" + node)
        }*/
        node match {
          case v: VarDecl => _vardecls = _vardecls diff List(v)
          case f: FunDecl => _fundecls = _fundecls diff List(f)
          case f: FunDef => _fundefs = _fundefs diff List(f)
          case i: IfThenElse => _conditionals = _conditionals diff List(i)
          case w: While => _conditionals = _conditionals diff List(w)
          case w: DoWhile => _conditionals = _conditionals diff List(w)
          case s: SwitchCase => _conditionals = _conditionals diff List(s)
          case f: For => _conditionals = _conditionals diff List(f)
          case t: TypeDecl => _typedecls = _typedecls diff List(t)
          case t: StructDecl => _structdecls = _structdecls diff List(t)
          case c: Call => _calls = _calls diff List(c)
          case t: TranslationUnit => _translationUnits = _translationUnits diff List(t)
          case _ =>
        }

        node.tree = null
      }
      node :: node.children.foldLeft(List[AstNode]())((x, y) => doit(y) ::: x)
    }

    _nodes = _nodes diff doit(node)
  }

  def insertMe(node: AstNode) = {
    link(node)
  }

  def removeMe(node: AstNode) = {
    unlink(node)
  }


  def visit(f: (AstNode) => Unit) {

    def doit(node: AstNode) {
      if (node != null) {
        f(node)
        node.children.foreach(doit)
      }
    }

    doit(root)
  }

}
