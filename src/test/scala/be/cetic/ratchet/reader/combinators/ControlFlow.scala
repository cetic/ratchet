package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import be.cetic.ratchet.reader.ast.{Affect, IntType}
import be.cetic.ratchet.reader.combinators.CParser
import be.cetic.ratchet.utils.TestUtils

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class ControlFlow extends FunSuite {

    test("The Parser should Manage if then else instruction") {
      val input =
        """
          |void f1 (signed long int* res){
          | signed long int i;
          | signed long int j;
          | signed long int z;
          |
          | i=0;
          | j=0;
          |
          | if(i){
          |  z = j;
          | }
          | else {
          |  z = i;
          | }
          |
          | (*res)=z;
          |}
          |"""
          .stripMargin

      val unit = (new CParser).apply(input)
      TestUtils.compare(input, unit.toString)

    }
}
