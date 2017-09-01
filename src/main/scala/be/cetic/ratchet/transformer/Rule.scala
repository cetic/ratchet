package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast._
import be.cetic.ratchet.reader.helpers.VariableGenerator

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
abstract class Rule {
  def transform(e: Expression, tvg: VariableGenerator): List[Instruction]


  def handle(e: Affect, tvg: VariableGenerator): List[Instruction] = {
    transform(e, tvg)
  }

}


