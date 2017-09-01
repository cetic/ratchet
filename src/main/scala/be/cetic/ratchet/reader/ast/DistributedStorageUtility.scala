package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import scala.collection.immutable.SortedMap

/**
  *
  * @author : Renaud De Landtsheer <renaud.delandtsheer@cetic.be>
  *
  */
trait DistributedStorageUtility {

  var storage: SortedMap[Int, AnyRef] = SortedMap.empty

  /** returns null if nothing was stored */
  final def getStorageAt[T](index: Int, default: T = null) =
    storage.getOrElse(index, default).asInstanceOf[T]

  final def storeAt(index: Int, value: AnyRef) {
    if (!storage.isDefinedAt(index))
      getStorageManager.registerStorageUtility(index, this)
    storage = storage + ((index, value))
  }

  def deleteStorage(key: Int) {
    storage -= key
  }

  def getStorageManager: StorageUtilityManager
}

trait StorageUtilityManager {
  var storageUtilities: SortedMap[Int, List[DistributedStorageUtility]] = SortedMap.empty
  var NextStoragePlace: Int = 0

  def getStorageIndex: Int = {
    val toreturn = NextStoragePlace
    NextStoragePlace += 1
    toreturn
  }

  def registerStorageUtility(key: Int, x: DistributedStorageUtility) {
    val oldSet = storageUtilities.getOrElse(key, List())
    storageUtilities += ((key, x :: oldSet))
  }

  def cleanKey(key: Int) {
    for (d <- storageUtilities.getOrElse(key, List.empty)) {
      d.deleteStorage(key)
    }
    storageUtilities -= key
  }
}

