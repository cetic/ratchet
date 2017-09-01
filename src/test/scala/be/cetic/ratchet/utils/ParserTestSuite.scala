package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast.{IntType, Variable, _}
import be.cetic.ratchet.reader.combinators.CParser
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
class ParserTestSuite extends FlatSpec with Matchers with TableDrivenPropertyChecks  {


  def testRelBinOp[T <: BinOp](input:String,signed:Boolean=false)(implicit m: Manifest[T]) = {
    (new CParser).init
    val unit = (new CParser).apply(input)
    TestUtils.compare(input,unit.toString)
    val ops = unit.descendantsOfType[T]()
    assert (ops.size == 1)
    val op = ops.head
    var tle = op.internLeft
    var tre = op.internRight
    assert (tle.isInstanceOf[Variable])
    assert (tre.isInstanceOf[Variable])
    var le = op.internLeft.asInstanceOf[Variable]
    var re = op.internRight.asInstanceOf[Variable]
    assert (le.declaration.name == "a")
    assert (re.declaration.name == "b")
    le.computeType should equal (IntType(32,true))
    re.computeType should equal (IntType(32,true))
    op.computeType should equal (BoolType())
    val opclone = op.clone()
    assert (opclone sameTree op)
  }


  def testBinOp[T <: BinOp](input:String,signed:Boolean=false)(implicit m: Manifest[T]) = {
    (new CParser).init
    val unit = (new CParser).apply(input)
    TestUtils.compare(input,unit.toString)
    val ops = unit.descendantsOfType[T]()
    assert (ops.size == 1)
    val op = ops.head
    var tle = op.internLeft
    var tre = op.internRight
    assert (tle.isInstanceOf[Variable])
    assert (tre.isInstanceOf[Variable])
    var le = op.internLeft.asInstanceOf[Variable]
    var re = op.internRight.asInstanceOf[Variable]
    assert (le.declaration.name == "a")
    assert (re.declaration.name == "b")
    le.computeType should equal (IntType(32,true))
    re.computeType should equal (IntType(32,true))
    op.computeType should equal (IntType(32,signed))
    val opclone = op.clone()
    assert (opclone sameTree op)
  }

  def testAssignationOp[T <: BinOp](input:String)(implicit m: Manifest[T]) = {
    (new CParser).init
    val unit = (new CParser).apply(input)
    TestUtils.compare(input,unit.toString)
    val ops = unit.descendantsOfType[T]()
    assert (ops.size == 1)
    val op = ops.head
    var tle = op.internLeft
    var tre = op.internRight
    assert (tle.isInstanceOf[Variable])
    assert (tre.isInstanceOf[Variable])
    var le = op.internLeft.asInstanceOf[Variable]
    var re = op.internRight.asInstanceOf[Variable]
    assert (le.declaration.name == "a")
    assert (re.declaration.name == "b")
    le.computeType should equal (IntType(64,signed = true))
    re.computeType should equal (IntType(32,signed = true))
    op.computeType should equal (IntType(64,signed = true))
    val opclone = op.clone()
    assert(opclone sameTree op)
  }

  def testUnOp[T <: UnaryOp](input:String,opType:Type)(implicit m: Manifest[T]) = {
    val parser = new CParser()
    parser.init()
    val unit = parser.apply(input)
    TestUtils.compare(input,unit.toString)
    val ops = unit.descendantsOfType[T]()
    assert (ops.size == 1)
    val op = ops.head
    var te = op.e
    assert (te.isInstanceOf[Variable])
    var e = op.e.asInstanceOf[Variable]
    assert (e.declaration.name == "a")
    e.computeType should equal (IntType(32,signed = true))
    op.computeType should equal (opType)
  }
}
