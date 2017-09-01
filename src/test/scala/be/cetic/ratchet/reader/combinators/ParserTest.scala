package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.scalatest.Suites
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class ParserTest extends Suites(
  new ArithmeticOperators,
  new Array,
  new BitWiseOperator,
  new CastTest,
  new CompoundAssignementOperators,
  new ControlFlow,
  new EnumSpecifier,
  new ForStatement,
  new FunctionAndDeclaration,
  new GotoTest,
  new LogicalOperators,
  new OctalandHexadecimal,
  new OtherOperators,
  new PostFixIncAndDecOp,
  new RelationalOperator,
  new SequencedAssignation,
  new StructTest,
  new SwitchStatement,
  new TypeDefTest,
  new Types,
  new WhileStatement
  ) {

}
