package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
trait ParserFixtureGenerator {


  def genRelBinOpFun(op:String): String = {
    """
      |
      | _Bool f(signed long int a, signed long int b){
      |   return a ?? b;
      | }
    """.stripMargin.replace("??",op)
  }

  def genBinOpFun(op:String): String = {
    """
      |
      | signed long int f(signed long int a, signed long int b)
      | {
      |   return a ?? b;
      | }
    """.stripMargin.replace("??",op)
  }

  def genAssignOpFun(op:String): String = {
    """
      | signed long long int f(signed long long int a, signed long int b)
      | {
      |   a ?? b;
      |   return a;
      | }
    """.stripMargin.replace("??",op)
  }

  def genUnOpFun(op:String):String = {
    """
      | signed long int f(signed long int a)
      | {
      |   return ??a;
      | }
    """.stripMargin.replace("??",op)
  }

}
