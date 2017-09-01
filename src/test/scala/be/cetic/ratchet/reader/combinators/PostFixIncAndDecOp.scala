package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import be.cetic.ratchet.reader.ast._
import be.cetic.ratchet.reader.ast.Variable
import be.cetic.ratchet.reader.ast.PostFixPlus
import be.cetic.ratchet.utils
import be.cetic.ratchet.utils.{ParserFixtureGenerator, ParserTestSuite, TestUtils}

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class PostFixIncAndDecOp extends ParserTestSuite with ParserFixtureGenerator {

  "The Parser" should "Manage postfix plus " in {
    val pattern =
      """
        |void f(signed long int a){
        | a++;
        |}
      """.stripMargin
    val unit = (new CParser).apply(pattern)
    TestUtils.compare(pattern, unit.toString)
    val postfixeds = unit.descendantsOfType[PostFixPlus]()
    assert(postfixeds.size == 1)
    val postfixed = postfixeds.head
    postfixed.expr match {
      case Variable(VarDecl("a",false,false,IntType(32,true),None)) => assert(true)
      case _ => assert(false)
    }
    assert(postfixed.computeType equals IntType(32,true))
  }

  it should "Manage postfix minus " in {
    val pattern =
      """
        |void f(signed long int a){
        | a--;
        |}
      """.stripMargin
    val unit = (new CParser).apply(pattern)
    utils.TestUtils.compare(pattern, unit.toString)
    val postfixeds = unit.descendantsOfType[PostFixMinus]()
    assert(postfixeds.size == 1)
    val postfixed = postfixeds.head
    postfixed.expr match {
      case Variable(VarDecl("a",false,false,IntType(32,true),None)) => assert(true)
      case _ => assert(false)
    }
    assert(postfixed.computeType equals IntType(32,true))
  }
}
