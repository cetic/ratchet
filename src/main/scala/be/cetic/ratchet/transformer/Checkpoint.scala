package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.TypeMgr
import be.cetic.ratchet.reader.ast._

object UniqueIDGenerator {
  var id = 0L

  def next: Long = {
    id = id + 1; id
  }
}



/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
class IndexedCheckPoint(_name: String, var ID: Long) extends CheckPoint(_name) {
  def allocate(value: Long) = ID = value

  override def printInstr(indent: Int, head: Boolean): String = "// checkpoint " + ID + " type " + _name

  def ID_Ex = constant64(ID)

  def id: Long = ID

  def const_checkpointEx = constant(const_checkpoint)

  def const_checkpoint = nextIndexedCheckPoint match {
    case Some(next) => ID ^ next.ID
    case None => -1L
  }

}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
abstract class CheckPoint(name: String) extends Instruction {
  val IDENTIFIER = UniqueIDGenerator.next

  def printInstr(indent: Int, head: Boolean): String = "// checkpoint of type " + name

  def children: List[AstNode] = List.empty

  def nextCheckPoint = nextExecutionInstructionsOfType[CheckPoint].headOption

  def nextIndexedCheckPoint = nextExecutionInstructionsOfType[IndexedCheckPoint].headOption

  def nextID = nextIndexedCheckPoint match {
    case None => 0L
    case Some(c) => c.ID
  }

  def previousCheckPoint = try {
    Some(previousExecutionInstructionsOfType[CheckPoint].head)
  } catch {
    case _:Throwable => None
  }

  def nextCheckPoints = nextExecutionInstructionsOfType[CheckPoint]

  def nextIndexedCheckPoints = nextExecutionInstructionsOfType[IndexedCheckPoint]

  def previousIndexedCheckPoints = previousExecutionInstructionsOfType[IndexedCheckPoint]

  def firstInstruction: List[Instruction] = List(this)

  def lastInstruction: List[Instruction] = List(this)

  def nextCheckPointId: Expression = if (nextIndexedCheckPoint.isDefined) nextIndexedCheckPoint.get.ID_Ex else null

  override def clone() = this

  def getName = name


  def constant(v: Long) = Constant(v, IntType(64, signed = false), TypeMgr.format32(v).toString)

  def constant64(v: Long) = Constant(v, IntType(64, signed = false), TypeMgr.format(v).toString)

  override def equals(obj: Any) = obj match {
    case c: CheckPoint => this.IDENTIFIER == c.IDENTIFIER
    case _ => false
  }


  override def hashCode(): Int = {
    val state = Seq(IDENTIFIER)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class FirstCheckPoint(fun: FunDef, _ID: Long) extends IndexedCheckPoint("FIRST CHECKPOINT", _ID) {
  override def clone() = FirstCheckPoint(fun, ID)
}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class SiedCheckPoint(_ID: Long) extends IndexedCheckPoint("SIED", _ID) {
  override def clone() = SiedCheckPoint(ID)
}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class SimpleCheckPoint(_ID: Long) extends IndexedCheckPoint("SIMPLE CHECKPOINT", _ID) {
  override def clone() = SimpleCheckPoint(ID)
}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class ErrorCheckPoint(var left: Expression, var right: Expression) extends CheckPoint("ERROR UPDATE") {
  left.father = this
  right.father = this

  override def children: List[AstNode] = List(left, right)

  override def clone() = ErrorCheckPoint(left, right)

  def mask = Cast(Parenthesis(BitXor(left, BitNot(right))), left.computeType)

  override def replaceChild(child: AstNode, by: AstNode): Unit = {
    if (child == left) {
      left = by.asInstanceOf[Expression]
    }
    else if (child == right) {
      right = by.asInstanceOf[Expression]
    }
    else {
      throw new Exception("Not a child")
    }
  }
}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class BeforeCall(_ID: Long, call: Call) extends IndexedCheckPoint("BEFORE CALL", _ID) {
  //def call=nextSibling.get.descendantsOfType[Call]().head

  override def nextCheckPoint = {
    if (call.target.funDef != null) {
      call.target.funDef.descendantsOfType[FirstCheckPoint]().headOption
    }
    else {
      None
    }
  }

  override def nextID = nextIndexedCheckPoint match {
    case None => call.target.getProperties("allocation.procId").get.asInstanceOf[Integer].toLong << 32
    case Some(c) => c.ID
  }

  override def nextIndexedCheckPoint = nextCheckPoint.asInstanceOf[Option[IndexedCheckPoint]]

  override def clone() = BeforeCall(ID, call)
}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class BeforeReturn(_ID: Long) extends IndexedCheckPoint("BEFORE RETURN", _ID) {
  def funDef = enclosingOrDie[FunDef]

  override def clone() = BeforeReturn(ID)
}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class AfterCall(_ID: Long) extends IndexedCheckPoint("AFTER CALL", _ID) {
  def call = previousSibling.get.descendantsOfType[Call]().head

  override def clone() = AfterCall(ID)
}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class SwitchCheckPoint(_ID: Long, switchInstr: Instruction) extends IndexedCheckPoint("SWITCH", _ID) {

  def labels: Map[CheckPoint, Int] = switchInstr match {
    case x: IfThenElse => Map((nextIndexedCheckPoints(0), -1), (nextIndexedCheckPoints(1), 0))
    case x: While => Map((nextIndexedCheckPoints(0), -1), (nextIndexedCheckPoints(1), 0))
    case x: DoWhile => Map(
      (x.firstDescendantsOfType[IndexedCheckPoint]().get, -1),
      (nextIndexedCheckPoints(0), 0)
    )
    // TODO case x:SwitchCase => x.cases.list.map(aCase => (aCase.nextChildrenOfType[IndexedCheckPoint],aCase.matchedValue)).toMap
    case _ => throw new Exception("Not yet implemented")
  }

  override def const_checkpoint = nextIndexedCheckPoint match {
    case Some(next) => next.ID ^ ID ^ labels.head._2
    case None => -1
  }

  override def clone() = throw new Exception("cloneage not supported for switch checkpoint")
}