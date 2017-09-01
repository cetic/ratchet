package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.Assert._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class TestConv extends FunSuite {

  test("32 bit integer must be converted correctly") {

    var input = -2
    var expected = (1L << 32) - 2

    val result = input.toLong & ((1L << 32) -1)

    assertEquals(expected,result)
  }

  /*
  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()

    t1 - t0
  }*/


  /*test("List perf pre") {

    var list = List(0)
    var total = 0L
    for(i <- 1 to 500000) total += time{ list = i::list }
    println("Elapsed time: " + total/1000000000. + "s")
  }

  test("List perf buffer") {

    var list = List(0)
    var total = 0L
    for(i <- 1 to 500000) total += time{ val lb = ListBuffer(list:_*); lb.append(i); list = lb.result() }
    println("Elapsed time: " + total/1000000000. + "s")
  }

  test("List perf reverse") {

    var list = List(0)
    var total = 0L
    for(i <- 1 to 500000) total += time{ list = (i::list.reverse).reverse }
    println("Elapsed time: " + total/1000000000. + "s")
  }

  test("List perf post") {

    var list = List(0)
    var total = 0L
    for(i <- 1 to 500000) total += time{ list = list ::: i :: Nil }
    println("Elapsed time: " + total/1000000000. + "s")
  }*/

}
