package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import scala.util.Random

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
trait IDGenerator {

  def nextCallId: Int

  def nextCheckPointId: Int

  def reset
}


case class LinearGenerator(offset: Long = 0) extends IDGenerator {
  private var callId = offset - 1
  private var checkpointId = -1

  def reset = {
    callId = offset - 1
    checkpointId = -1
  }

  def nextCallId = {
    callId = callId + 1; callId.toInt
  }

  def nextCheckPointId = {
    checkpointId = checkpointId + 1; checkpointId
  }
}

case class RandomGenerator(offset: Long = 0) extends IDGenerator {

  private var generator = new Random(offset)

  def reset = {
    generator = new Random(offset)
  }

  def nextCallId = generator.nextInt()

  def nextCheckPointId = generator.nextInt()

}

case class IDGeneratorNotFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

object IDGeneratorFactory {

  def create(name: String, offset: Long) = name match {
    case "linear" => LinearGenerator(offset)
    case "random" => RandomGenerator(offset)
    case _ => throw IDGeneratorNotFoundException("ID idGenerator not found with the name : " + name)
  }

}