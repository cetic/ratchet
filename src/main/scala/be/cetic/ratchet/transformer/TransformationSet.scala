package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.ast.AstTree


trait Transformable

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
class TransformationSet(prehandlers: List[Prehandler], var operations: List[Transformation], posthandlers: List[TransformationSet]) {
  private var _onlyPrehandlers = false;

  def onlyPrehandlers = _onlyPrehandlers

  def onlyPrehandlers_=(a: Boolean) {
    _onlyPrehandlers = a
  }

  def transform(software: AstTree): AstTree = {
    var code = software
    code = apply(code, prehandlers)
    if (onlyPrehandlers) {
      code
    }
    else {
      operations.foreach(operation => {
        try {
          Logger.getLogger("Transformation").log(Level.INFO, "Applying transformation " + operation.name + "...")
          code = operation.apply(code)
        }
        catch {
          case e: Throwable => throw new Exception("Cannot apply transformation " + operation.name + "(" +
            operation.getClass.getCanonicalName + ")", e)
        }
      })
      code = apply(code, posthandlers)
      code
    }

  }

  def apply(software: AstTree, transformations: List[TransformationSet]) = {
    var aSoftware = software
    transformations.foldLeft(aSoftware)((currentSoftwareState, prehandler) =>
      prehandler.transform(currentSoftwareState))
  }
}

abstract class Prehandler(_operations: List[Transformation]) extends TransformationSet(Nil, _operations, Nil);

abstract class Posthandler(_operations: List[Transformation]) extends TransformationSet(Nil, _operations, Nil);

abstract class Transformation(val name: String) {

  def apply(software: AstTree): AstTree
}

