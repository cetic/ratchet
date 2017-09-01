package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.TypeMgr
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class TestFormat extends FunSuite{


  test("Yacca ID must respect the %016xULL format") {

    var input = TypeMgr.format( ((15L << 32) | 129L).toLong )
    var expected = "0x0000000f00000081ULL"

    print(input)
    assert (input == expected)
  }


}
