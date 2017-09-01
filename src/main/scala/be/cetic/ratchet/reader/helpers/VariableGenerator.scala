package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast.{VarDecl, _}

import scala.collection.mutable.Map


abstract class VariableNameIncrementator {
  def next(name: String): String

  def reset
}


case class DefaultIncrementator(prefix: String = "", suffix: String = "_") extends VariableNameIncrementator {
  final private var state = Map[String, Int]().withDefaultValue(0)

  def reset = state = Map[String, Int]().withDefaultValue(0)

  override def next(name: String): String = prefix + increment(name) + suffix

  def increment(name: String) = {
    state(name) = lastIndexOf(name) + 1; lastIndexOf(name) - 1
  }

  def lastIndexOf(name: String) = state(name)
}

case class VariableGenerator(
                              tmpprefix: String,
                              inc: VariableNameIncrementator,
                              withIndex: Boolean,
                              typeDef: Type) {

  case class NoIncrementation() extends VariableNameIncrementator {
    final private var state = Map[String, Boolean]().withDefaultValue(false)

    def reset = state = Map[String, Boolean]().withDefaultValue(false)

    override def next(name: String): String =
      if (state(name)) throw new Exception("No incrementation for variable idGenerator")
      else {
        state(name) = true; ""
      }
  }

  var _inc: VariableNameIncrementator = null
  if (!withIndex) {
    _inc = NoIncrementation()
  }
  else {
    _inc = inc
  }

  def reset() = _inc.reset

  //def next(name:String, suffix:String) = VarDecl(tmpprefix+_inc.next(name)+name+suffix,false,typeDef,Some(Constant(0,IntType(),0.toString)))
  def next(name: String, ctype: Type, suffix: String) = VarDecl(tmpprefix + _inc.next(name) + name + suffix, false, false, ctype)

  //def next(name:String) = VarDecl(tmpprefix+_inc.next(name)+name,false,typeDef,Some(Constant(0,IntType(),0.toString)))
  def next(name: String, ctype: Type) = VarDecl(tmpprefix + _inc.next(name) + name, false, false, ctype)

  /*def next(name:String,nb:Int):List[VarDecl] =
    if(nb==1) List(next(name))
    else (0 until nb).map(x=>next(name, ""+(x+1))).toList
  */
  def next(name: String, ctype: Type, nb: Int): List[VarDecl] =
    if (nb == 1) List(next(name, ctype))
    else (0 until nb).map(x => next(name, ctype, "" + (x + 1))).toList

}
