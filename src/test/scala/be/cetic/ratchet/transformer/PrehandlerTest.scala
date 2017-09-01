package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.TypeMgr
import be.cetic.ratchet.reader.ast._

import be.cetic.ratchet.reader.ast.FunDecl
import be.cetic.ratchet.reader.ast.Block
import be.cetic.ratchet.reader.ast.FunDef
import be.cetic.ratchet.utils.DefaultVariableNameGenerator
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.Assert._

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
class PrehandlerTest extends FunSuite with BeforeAndAfter {
  var ast:AstTree=null

  before {
    val argvDecl=VarDecl("argv",false,false,IntType())
    val argcDecl=VarDecl("argc",false,false,IntType())

    val iDecl=VarDecl("i",false,false,IntType(),Some(Constant(1,IntType(),"1")))
    val jDecl=VarDecl("j",false,false,IntType(),Some(Constant(4,IntType(),"4")))
    val aDecl=VarDecl("a",false,false,IntType(),Some(Constant(0,IntType(),"0")))

    val cond=Equ(iDecl.asVariable,jDecl.asVariable)

    val addition=Plus(Variable(iDecl),Variable(jDecl))

    val ifb=Block(List(addition))


    var ifte=IfThenElse(cond,ifb)

    val mainImpl = Block(List(iDecl,jDecl,aDecl,ifte));
    val mainl=List(argcDecl,argvDecl)
    val mainDecl = FunDecl("f1",FunctionType(Void(),mainl),false)
    val mainDef = FunDef(mainDecl,mainImpl)
    val unit = TranslationUnit(List(mainDef),"main.c")
    val bast = AstTree(FullSoftwareCode(List(unit)))

    ast = Simplification(new DefaultVariableNameGenerator(),"butterfly_","").transform(bast)
  }

  test("yaccaidIfThen"){

    val ifStmt=ast.root.descendantsOfType[IfThenElse]().head
    println(ast.toString)

    assertTrue(ifStmt.cond.isInstanceOf[Variable])

    val variable=ifStmt.cond.asInstanceOf[Variable];

    assertTrue(variable.name.contains("tmp"))
    assertTrue(ifStmt.previousSiblingOfType[ExpressionInstr].isDefined)

    val oldCond=ifStmt.previousSiblingOfType[ExpressionInstr].get.e

    println("old  :"+ oldCond)

    assertTrue(oldCond match {
      case Affect(
        Variable(VarDecl("butterfly_0_tmp",false,false,TypeMgr.BOOL,None)),
          Nequ(Variable(VarDecl("butterfly_1_tmp",false,false,TypeMgr.BOOL,None)),Constant(0,IntType(32,true),"0"))) => true
      case _ => false
    })

  }
}
