package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import scala.util.parsing.combinator.RegexParsers

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
object ParserSandbox extends RegexParsers{

  val types=List("int","char")

  def IDENTIFIER:Parser[String] = ("""[$A-Za-z_][$A-Za-z_0-9]*""".r)


  def TYPE:Parser[String] = ("""[$A-Za-z_][$A-Za-z_0-9]*""".r)

  def decl:Parser[Any] = IDENTIFIER ~ ":" ~ TYPE
  def use:Parser[Any] =  TYPE ~ ":" ~ IDENTIFIER

  def soft: Parser[Any] = decl | use

  def main(args: Array[String]){
    val str= "i:char"

    this.parseAll(decl,str) match {
      case Success(result, _) => print("success")
      case failure : NoSuccess => scala.sys.error("At position "+failure.next.pos.line + ":" +failure.next.pos.column + " : " +failure.msg)
      case _ => throw new Exception("Unknown error during parsing");
    }

  }
}
