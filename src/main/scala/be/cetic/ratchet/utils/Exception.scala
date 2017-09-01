package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */

case class VariableUndeclaredException(varname: String, cause: Throwable = null)
  extends Exception("Variable undeclared : " + varname, cause);

case class UnsupportedTypeException(message: String, cause: Throwable = null)
  extends Exception(message, cause);

case class TransformationException(message: String, cause: Throwable = null)
  extends Exception(message, cause);