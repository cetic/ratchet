package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.Assert.assertEquals


/**
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */

object TestUtils {

  def compare(expected:String,current:String){
    val _current = current.replaceAll("UINT(32|64)","unsigned int").replaceAll("U?INT(32|64)","int")
    val _expected = expected.replaceAll("UINT(32|64)","unsigned int").replaceAll("U?INT(32|64)","int")
    println("current\n"+_current)
    println("expected\n"+_expected)
    assertEquals(("""([ \r\t\u000C\n]|(/*.*\*/)|(//[^\n|\r]*\r?\n))+""".r).replaceAllIn(_expected,"").trim,(("""([ \r\t\u000C\n]|(/*.*\*/)|(//[^\n|\r]*\r?\n))+""".r).replaceAllIn(_current,"").trim))
  }

}