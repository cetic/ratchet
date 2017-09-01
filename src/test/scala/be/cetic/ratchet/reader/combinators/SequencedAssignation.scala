package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.utils
import be.cetic.ratchet.utils.TestUtils
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class SequencedAssignation extends FunSuite {

  test("The Parser should manage sequenced assignation in statement") {
    val pattern =
      """
        |void f(signed long int a){
        | signed long int c;
        | signed long int b;
        | b = (c = a);
        |}
      """.stripMargin

    val lines = pattern
    val unit = (new CParser).apply(lines)
    TestUtils.compare(lines, unit.toString)
  }

  test("The Parser should manage sequenced assignation in statement with operation") {
    val pattern =
      """
        |void f(signed long int a){
        | signed long int c;
        | signed long int b;
        | b = (c = a + 3);
        |}
      """.stripMargin

    val lines = pattern
    val unit = (new CParser).apply(lines)
    utils.TestUtils.compare(lines, unit.toString)
  }
}
