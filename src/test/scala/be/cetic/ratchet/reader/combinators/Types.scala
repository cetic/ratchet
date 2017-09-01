package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import be.cetic.ratchet.reader.ast._
import be.cetic.ratchet.reader.ast.Plus
import be.cetic.ratchet.reader.ast.Constant
import be.cetic.ratchet.reader.ast.TypeDecl
import be.cetic.ratchet.reader.ast.IntType
import org.junit.Assert._
import be.cetic.ratchet.reader.ast.Plus
import be.cetic.ratchet.reader.ast.Constant
import be.cetic.ratchet.reader.ast.TypeDecl
import be.cetic.ratchet.reader.ast.TypeDefReference
import be.cetic.ratchet.reader.ast.IntType
import be.cetic.ratchet.utils.TestUtils

/**
 *
 * Test suite to verify that the following standard type are supported by default
 * by the parser
 *
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class Types extends FunSuite {


  test("The Parser should manage int type") {
    val input =
      """
        | signed long int i;
      """.stripMargin

    val unit = (new CParser).apply(input)
    TestUtils.compare(input,unit.toString)
  }

  def gen(op:String,tA:Type,tB:Type):Expression = {
    val a = VarDecl("a",false,false,tA);val b = VarDecl("b",false,false,tB)
    val in = "a"+op+"b"
    val parser = new CParser
    parser.init(List(a,b))
    parser.parseAll(parser.conditional_expression,in).get
  }


  test("Basic arithmetics rules") {
    val tA = TypeDefReference(TypeDecl("UINT32",IntType(32,false)))
    val tB = TypeDefReference(TypeDecl("UINT16",IntType(32,false)))
    val ops = List("+","-","*","/","%",">>","<<","&", "|")
    ops.foreach(op => {
      val input = gen(op,tA,tB)
      val output = input.computeType
      assertEquals(tA,output)
    })
  }
}
