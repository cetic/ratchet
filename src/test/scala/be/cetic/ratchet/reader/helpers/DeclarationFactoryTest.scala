package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, FlatSpec}
import be.cetic.ratchet.reader.ast.VarDecl
import scala.Some

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class DeclarationFactoryTest extends FlatSpec with Matchers {

  "INT32" should "correctly create simple int i declaration" in {

    val expected = VarDecl("i",false,false,IntType(32,true),None)
    val output = DeclarationFactory.createDeclaration(
      List(StringSpecifier("int")),
      Some(Identifier("i")),
      None
    )
    assert(expected.toString == output.toString)
  }

  it should "correctly create extern int i declaration" in {

    val expected = VarDecl("i",false,extern=true,IntType(32,true),None)
    val output = DeclarationFactory.createDeclaration(
      List(StringSpecifier("int"),TypeScope("extern")),
      Some(Identifier("i")),
      None
    )
    assert(expected.toString == output.toString)

  }


  it should "correctly create static int i declaration" in {

    val expected = VarDecl("i",static=true,extern=false,IntType(32,true),None)
    val output = DeclarationFactory.createDeclaration(
      List(StringSpecifier("int"),TypeScope("static")),
      Some(Identifier("i")),
      None
    )
    assert(expected.toString == output.toString)

  }

  "CHAR" should "correctly create simple char i declaration" in {

    val expected = VarDecl("i",false,false,CharType(true),None)
    val output = DeclarationFactory.createDeclaration(
      List(StringSpecifier("char")),
      Some(Identifier("i")),
      None
    )
    assert(expected.toString == output.toString)

  }

  it should "correctly create extern char i declaration" in {

    val expected = VarDecl("i",false,extern=true,CharType(true),None)
    val output = DeclarationFactory.createDeclaration(
      List(StringSpecifier("char"),TypeScope("extern")),
      Some(Identifier("i")),
      None
    )
    assert(expected.toString == output.toString)

  }


  it should "correctly create static char i declaration" in {

    val expected = VarDecl("i",static=true,extern=false,CharType(true),None)
    val output = DeclarationFactory.createDeclaration(
      List(StringSpecifier("char"),TypeScope("static")),
      Some(Identifier("i")),
      None
    )
    assert(expected.toString == output.toString)

  }

}
