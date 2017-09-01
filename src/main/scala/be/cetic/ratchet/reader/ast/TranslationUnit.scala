package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.helpers.ASTException
import be.cetic.ratchet.transformer.Transformable
import be.cetic.ratchet.utils.Subject


case class FullSoftwareCode(var _list: List[TranslationUnit])
  extends ASTList[TranslationUnit] with DeclarationScope with NoDoubleDeclare with NoDelete[TranslationUnit] with Transformable
    with StorageUtilityManager {
  //declaration scope is for funDef, not for funDecl. neither for external declarations.
  //these are kept on a separate list of unresolved externals.

  notifyAllInserts()

  def list = _list

  def list_=(a: List[TranslationUnit]) {
    cleanDecls
    _list = a
    notifyAllInserts()
  }


  var externalDecl: List[Declaration] = List.empty //the unresolved external declarations
  override def inserted(listBefore: List[TranslationUnit], node: TranslationUnit, listAfter: List[TranslationUnit]) {
    //binds existing external declarations
    //imports declarations into the bazaar
    for (d: Declaration <- node.declarations) {
      if (d.extern) {
        externalDecl = d :: externalDecl
      } else {
        //pas extern, donc c'est résolu
        if (!(d.canbeMultipleDeclared && this.declared.isDefinedAt(d.name))) {
          addDeclaration(d)
        }
      }
    }
  }

  def functionDeclarations = descendantsOfType[FunDecl](4).asInstanceOf[List[FunDecl]]

  /** checks that everything was bound, all external declarations could be resolved */
  def checkComplete() {
    for (ud <- externalDecl) {
      ud match {
        case v: VarDecl => v.actualDeclaration = get(v.name).asInstanceOf[VarDecl]
        case f: FunDecl => f.funDef = get(f.name).asInstanceOf[FunDef]
        case _ => throw ASTException(ud + " not resolved")
      }
    }

    for (file <- list; declarations <- file.declarations)
      if (declarations.extern) throw ASTException("only extern") //to check
  }
}

case class TranslationUnit(var list: List[Declaration], var fileName: String = null) extends ASTList[Declaration]
  with DeclarationScope {
  //les déclarations sont déjà résolues en interne si possible.
  //faut faire gaffe àce qui est dans la liste et dans le scope...
  //une fct peut être déclarée après avoir été définie?

  override def inserted(listBefore: List[Declaration], node: Declaration, listAfter: List[Declaration]) {
    //faire gaffe si on a une déclaration / définition de fct.
    //dans le scope, on met le premier truc qu'on trouve.

    //on ne stoque dans le declSclpe que le premier qui vient.
    Logger.getLogger("AST").log(Level.INFO, "To " + this.fileName + " Adding declaration " + node)
    addDeclaration(node)
    node.father = this

  }

  notifyAllInserts()

  //"File "+fileName+":\r\n"+
  override def toString: String = list.map((i: Declaration) => i.printInstr(0, true)).mkString("\n\n") + "\n"

  def lookForLink(c: Call) = this.descendantsOfType[FunDef](2).filter(
    fd => fd.name.equals(c.target.name)).headOption match {
    case Some(x) => x
    case _ => c.target
  }

  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[TranslationUnit]) {
      var _obj = obj.asInstanceOf[TranslationUnit]
      return _obj.fileName.eq(this.fileName)
    }
    else false
  }
}

case class FunDef(funDecl: FunDecl, instructions: Block, ref: Option[FunDecl] = None) extends Declaration(false)
  with DeclarationScope with Subject {

  def name: String = funDecl.name

  def calls = ref match {
    case Some(d) => d.calls ::: funDecl.calls
    case None => funDecl.calls
  }

  this.register(funDecl)
  ref match {
    case Some(d) => this.register(d); d.funDef = this
    case None =>
  }

  def firstDecl = ref match {
    case Some(x) => x
    case _ => funDecl
  }

  //assert(funDecl.funType.funDef == null,"At most one fun def per fun type")

  funDecl.funDef = this
  funDecl.father = this
  instructions.father = this
  funDecl.arguments.foreach(addDeclaration(_))


  //this is immutable because we did not implement child assignment operators.
  def children: List[AstNode] = List(funDecl, instructions)

  def printInstr(indent: Int, head: Boolean): String =
    funDecl.printInstr(indent, true) + "\n" + instructions.printInstr(indent, false)

  override def toString: String = printInstr(0, false)


}

