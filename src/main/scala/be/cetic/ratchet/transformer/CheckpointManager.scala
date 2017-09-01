package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.ast.{Expression, FunDecl, FunDef}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
class Clusterizer {


  def getClusters(function: FunDef): List[List[IndexedCheckPoint]] = {
    Logger.getLogger("CheckPointManager").log(Level.INFO, "Creating cluster for " + function.name)
    var consideredCheckpoint = List[CheckPoint]()

    def allPredecessorHandled(cps: List[IndexedCheckPoint]): Boolean = {
      if (cps.nonEmpty) {
        val prev = cps.head.previousIndexedCheckPoints
        prev.size == 1 || prev.filterNot(consideredCheckpoint.contains(_)).size == 0
      }
      else {
        false
      }
    }

    // We get the first checkpoint in the function
    val c = function.instructions.childrenOfType[FirstCheckPoint]
      .head.asInstanceOf[IndexedCheckPoint]

    var clusters = List[List[IndexedCheckPoint]](List(c))

    // We create a pipe
    var pipe = List(c.nextIndexedCheckPoints)

    // We performing a BFS search expanding using nextIndexedCheckPoints function
    while (pipe.nonEmpty) {
      val current = pipe.head;
      pipe = pipe.tail
      if (current.nonEmpty && !consideredCheckpoint.contains(current.head)) {
        val head = clusters.filter(c => c.size >= current.size)
        clusters = head ::: List(current) ::: (clusters diff head)
        consideredCheckpoint :::= current

        pipe = current.map(c => c.nextIndexedCheckPoints).filter(allPredecessorHandled) ::: pipe
      }
    }

    //
    // sort(clusters.sortBy(_.size).distinct)

    clusters
  }

  private def sort(clusters: List[List[IndexedCheckPoint]]): List[List[IndexedCheckPoint]] =
    if (clusters.size == 0) Nil
    else {
      val largest = clusters.filter(c => c.size == clusters.last.size)
      largest ::: sort(clusters diff largest)
    }


}


class InadmissibleIDException extends Exception {}

class YetAllocatedException extends Exception {}



class ProcIdGenerator(generator: IDGenerator) {
  private val MAX_ATTEMPT = 250
  private val _procId = collection.mutable.Map[String, Int]()

  private def isCallAdmissible(value: Int): Boolean = _procId.filter(_._2 == value).isEmpty

  def computeProcIds(funDecls: List[FunDecl]) {
    _procId.clear()
    funDecls.map(f => f.name -> computeCallId(f)).toMap
  }

  private def computeCallId(function: FunDecl) {
    var attempt_number: Int = 0
    var index: Int = generator.nextCallId

    Logger.getLogger("CheckPointManager").log(Level.INFO, "Allocation tentative of proc id for " + function.name)
    while (attempt_number <= MAX_ATTEMPT) {
      if (_procId.contains(function.name)) {
        // to manage reference to function
        function.addProperties("allocation.procId", _procId.get(function.name).getOrElse(0))
        return
      }
      else if (isCallAdmissible(index)) {
        Logger.getLogger("CheckPointManager").log(Level.INFO, "Allocation of proc " + function.name + " with id " + index)
        _procId.put(function.name, index)
        function.addProperties("allocation.procId", index)
        return
      }
      else {
        index = generator.nextCallId
        attempt_number = attempt_number + 1
      }
    }
    throw new Error("Max attempt exceeded during allocation of proc id")
  }
}

/**
  * Created by ddu on 28/08/17.
  */
trait CheckpointManager {

  def createSiedCheckPoint: CheckPoint

  def createErrorCheckPoint(left: Expression, right: Expression): CheckPoint

}
