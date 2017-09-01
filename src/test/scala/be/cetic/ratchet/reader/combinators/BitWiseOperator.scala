package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import be.cetic.ratchet.reader.combinators.CParser
import be.cetic.ratchet.utils
import be.cetic.ratchet.utils.TestUtils

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class BitWiseOperator extends FunSuite {
  test("The Parser should Manage the bitwise and operator") {
    val input =
      """
        |void f1 (signed long int* res){
        | signed long int a;
        | signed long int b;
        | signed long int c;
        | c = a & b ;
        |}
      """.stripMargin


    val unit = (new CParser).apply(input)
    TestUtils.compare(input,unit.toString)
  }

  test("The Parser should Manage the bitwise or operator") {
    val input =
      """
        |void f1 (signed long int* res){
        | signed long int a;
        | signed long int b;
        | signed long int c;
        | c = a | b ;
        |}
      """.stripMargin


    val unit = (new CParser).apply(input)
    utils.TestUtils.compare(input,unit.toString)
  }
}


