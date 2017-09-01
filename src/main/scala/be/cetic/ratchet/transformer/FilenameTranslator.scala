package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.io.File

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
trait FilenameTranslator {
  def translate(filename: String): String
}

object DirectoryCleaner extends FilenameTranslator {
  def translate(filename: String): String = new File(filename).getName
}
