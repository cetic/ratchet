package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.scalatest.{FlatSpec, Matchers, Suites}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import be.cetic.ratchet.reader.ast.{IntType, PointerType, TypeUtils, VarDecl}
import be.cetic.ratchet.reader.combinators.CParser
import be.cetic.ratchet.utils.ParseTreeDumper

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class HelperTest extends FlatSpec with Matchers {


  "Dumper" should "correctly dump" in {
    val input =
      """
        |  typedef signed long int INT8;
        |  typedef unsigned long int UINT32;
        |  typedef signed long int INT32;
        |
        |  void test_addition(INT8 a, INT8 b, INT8 *result)
        |  {
        |    *result = a + b;
        |  }
      """.stripMargin
    val unit = (new CParser).apply(input)

    val dumper = new ParseTreeDumper()

    dumper.visit(unit)

    print(dumper.get)

    //utils.TestUtils.compare(input,unit.toString)
  }


  "TypeUtils.generateNestedPointer" should "return the basetype if the pointer depht is 0" in {
    assert( TypeUtils.generateNestedPointer(0,IntType()) match{
      case IntType(32,false) => true
      case _ => false
    })
  }

  "TypeUtils.generateNestedPointer" should "return a pointer to a basetype if the pointer depht is 1" in {
    assert( TypeUtils.generateNestedPointer(1,IntType()) match{
      case PointerType(IntType(32,false)) => true
      case _ => false
    })
  }

  "TypeUtils.generateNestedPointer" should "return a triple pointer to a basetype if the pointer depht is 3" in {
    assert( TypeUtils.generateNestedPointer(3,IntType()) match{
      case PointerType(PointerType(PointerType(IntType(32,false)))) => true
      case _ => false
    })
  }


}
