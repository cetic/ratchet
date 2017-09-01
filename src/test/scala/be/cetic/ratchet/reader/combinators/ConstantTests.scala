package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import be.cetic.ratchet.reader.ast._
import be.cetic.ratchet.utils.{ParserFixtureGenerator, ParserTestSuite}

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class ConstantTests extends ParserTestSuite with ParserFixtureGenerator {

  def const(cont:Long) = Constant(cont,IntType(32,signed = true),cont.toString)

  "Constant 0" should " be constant " in {
    val expr = const(0L)
    assert(expr.isConstant)
  }

  "0 + 2" should " be constant " in {
    val expr = Plus(const(0L),const(2L))
    assert(expr.isConstant)
  }

  "(0 + 2) + 3" should " be constant " in {
    val expr = Plus(Parenthesis(Plus(const(0L),const(2L))),const(3L))
    assert(expr.isConstant)
  }

}

