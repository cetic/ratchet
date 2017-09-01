package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

trait Event

case class SimpleMessageEvent(message: String) extends Event


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
trait Observer {
  private var observe: Boolean = true

  def setObserve(o: Boolean) = observe = o

  final def listen(sender: Subject, e: Event) = if (observe) _listen(sender, e)

  protected def _listen(sender: Subject, e: Event)
}

trait Subject {
  private var _observers: List[Observer] = Nil

  def observers = _observers

  def register(o: Observer) = _observers = o :: _observers

  def unregister(o: Observer) = _observers = _observers diff List(o)

  def notifyObservers(e: Event) = _observers.foreach(_.listen(this, e))
}


