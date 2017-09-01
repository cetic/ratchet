package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert
import be.cetic.ratchet.reader.ast.{Affect, IntType}
import be.cetic.ratchet.reader.combinators.CParser
import be.cetic.ratchet.utils.{ParserTestSuite, TestUtils}

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class CastTest extends ParserTestSuite {

  "The parser" should " parse cast and change type as expected" in {
    val input =
      """
        |typedef signed long int INT32;
        |void f1 (signed long int* res){
        | INT32 z;
        | signed long int i;
        | i = (signed long int) z;
        |}
      """.stripMargin

    val unit = (new CParser).apply(input)
    TestUtils.compare(input, unit.toString)
    val affects = unit.descendantsOfType[Affect]();
    assert(affects.size == 1)
    val affect = affects.head
    assert(affect.right.computeType equals IntType(32,true))
  }


}
