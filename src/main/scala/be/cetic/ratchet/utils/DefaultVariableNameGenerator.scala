package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast.IntType
import be.cetic.ratchet.reader.helpers.{DefaultIncrementator, VariableGenerator}

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
class DefaultVariableNameGenerator() extends VariableGenerator("butterfly_",DefaultIncrementator(),true,IntType()) {

}
