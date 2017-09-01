package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class ParserException(message: String, cause: Throwable = null) extends Exception(message, cause);


object ASTException {
  def apply(message: String, cause: Throwable = null): ASTException = {
    new ASTException(message, cause)
  }
}

class ASTException(message: String, cause: Throwable = null) extends Exception(message, cause);

case class NoFatherException(message: String, cause: Throwable = null) extends ASTException(message, cause);
