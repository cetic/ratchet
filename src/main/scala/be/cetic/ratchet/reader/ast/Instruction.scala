package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.helpers.{ASTException, VariableGenerator}
import be.cetic.ratchet.utils.{Event, Observer, Subject}


abstract class Instruction() extends AstNode {
  override def toString: String = printInstr(0, head = true)

  /**
    * builds the string representation of an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String

  def nextExecutionInstructions: List[Instruction] =
    if (nextSiblingOfType[Instruction].isDefined) nextSiblingOfType[Instruction].get.firstInstruction
    else {
      if (enclosing[Instruction].isDefined) enclosing[Instruction].get.nextExecutionInstructions else Nil
    }

  def nextExecutionInstructionsOfType[T](implicit m: Manifest[T]): List[T] =
    nextExecutionInstructions.map {
      case x if m.runtimeClass.isInstance(x) => List(x.asInstanceOf[T])
      case x => x.nextExecutionInstructionsOfType[T](m)
    }.flatten

  def nextExecutionInstructionOfType[T](implicit m: Manifest[T]) = nextExecutionInstructionsOfType[T].head

  def previousExecutionInstructionsOfType[T](implicit m: Manifest[T]): List[T] =
    previousExecutionInstructions.map {
      case x if m.runtimeClass.isInstance(x) => List(x.asInstanceOf[T])
      case x => x.previousExecutionInstructionsOfType[T](m)
    }.flatten

  def previousExecutionInstructionOfType[T](implicit m: Manifest[T]) = previousExecutionInstructionsOfType[T].head

  def previousExecutionInstructions: List[Instruction] =
    if (previousSiblingOfType[Instruction].isDefined) previousSiblingOfType[Instruction].get.lastInstruction
    else if (enclosing[Instruction].isDefined) enclosing[Instruction].get.previousExecutionInstructions
    else Nil

  def firstInstruction: List[Instruction]

  def lastInstruction: List[Instruction]

  override def clone = this
}

case class IfThenElse(
                       var internalCond: Expression,
                       var internalThen: Instruction,
                       var internalElse: Option[Instruction] = None)
  extends Instruction {

  def children: List[AstNode] = elseInstr match {
    case None => List(cond, thenInstr)
    case Some(i) => List(cond, thenInstr, i)
  }

  override def clone = IfThenElse(internalCond.clone, internalThen.clone, if (internalElse.isDefined) Some(internalElse.get.clone) else None)

  def cond: Expression = internalCond

  def cond_=(e: Expression) {
    if (cond.father == this) cond.father = null
    e.father = this
    internalCond = e
  }

  def thenInstr: Instruction = internalThen

  def thenInstr_=(t: Instruction) {
    if (thenInstr.father == this) thenInstr.father = null
    t.father = this
    internalThen = t
  }

  def elseInstr: Option[Instruction] = internalElse

  def elseInstr_=(e: Instruction) {
    elseInstr match {
      case Some(i) => if (i.father == this) i.father = null
      case None =>
    }
    e.father = this
    internalElse = Some(e)
  }

  cond.father = this
  thenInstr.father = this

  if (elseInstr.isDefined) elseInstr.get.father = this

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == cond) cond = by.asInstanceOf[Expression]
    else if (child == thenInstr) thenInstr = by.asInstanceOf[Instruction]
    else if (elseInstr.isDefined && child == elseInstr.get) elseInstr = by.asInstanceOf[Instruction]
    else throw ASTException("child not found: " + child)
  }

  def firstInstruction: List[Instruction] = thenInstr.firstInstruction ::: (elseInstr match {
    case None => nextExecutionInstructions.head.firstInstruction
    case Some(instr) => instr.firstInstruction
  })


  def lastInstruction: List[Instruction] = thenInstr.lastInstruction ::: (elseInstr match {
    case None => Nil
    case Some(instr) => instr.lastInstruction
  })

  /**
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = {
    SpacesAsHead(indent, head) + "if(" + cond + ")\n" + SpacesAsHead(indent, head) + thenInstr.printInstr(indent, head = false) +
      (elseInstr match {
        case None => ""
        case Some(instr) => "\n" + Spaces(indent) + "else\n" + Spaces(indent) + instr.printInstr(indent, head = false)
      })
  }

  def extractCondition(replacement: VarDecl) = if (cond != null) {
    // Caution the order is important
    replacement.init = Some(Nequ(internalCond.encloseIfNeeded, Constant(0, IntType(32, signed = true), "0")))
    cond = replacement.asVariable
    insertBefore(replacement)
  }

}

case class Block(var internalList: List[Instruction] = List.empty)
  extends Instruction with ASTList[Instruction] with DeclarationScope {

  def list: List[Instruction] = internalList

  def list_=(a: List[Instruction]) {
    internalList = a;
  }


  notifyAllInserts()

  override def clone = Block(list.map(t => t.clone))

  def appendDeclaration(d: Declaration) = {
    super.addDeclaration(d)
    lastDeclaration match {
      case Some(ld) => ld.insertAfter(d)
      case None => list = d :: list
    }
  }

  // def prepend(inst:Instruction) = {list = inst :: list;inst.father=this}
  // def append(inst:Instruction) = {list = list ::: inst :: Nil;inst.father=this}

  def appendDeclarations(d: List[Declaration]) = d.foreach(x => appendDeclaration(x))

  final def lastDeclaration: Option[Declaration] = try {
    Some(children.filter(d => d.isInstanceOf[Declaration]).last.asInstanceOf[Declaration])
  }

  catch {
    case e: Throwable => None
  }

  def linkGotos() = {
    val labelstoCheck = descendantsOfType[LabelledStmt]()
    if (labelstoCheck.map(l => l.label).distinct.size != labelstoCheck.size) {
      throw ASTException("Multiple label declare in this scope")
    }

    val gotos = descendantsOfType[GotoUnlinked]().map(f => f.label -> f).toMap
    val labels = labelstoCheck.map(f => f.label -> f).toMap

    val l_goto = gotos.map {
      case (label, goto) => labels.get(label) match {
        case None => throw ASTException("Error no label " + label + " declared in this scope")
        case Some(l) =>
          val ng = Goto(l)
          goto.replaceBy(ng)
          ng
      }
    }

    l_goto.foreach(_.link())

  }

  override def inserted(listBefore: List[Instruction], node: Instruction, listAfter: List[Instruction]) {
    node match {
      case d: Declaration => addDeclaration(d)
      case _ =>
    }
  }


  // To Improve
  override def nextExecutionInstructions: List[Instruction] = {
    checkFather
    if (father.isInstanceOf[IfThenElse]) father.asInstanceOf[Instruction].nextExecutionInstructions
    else if (nextSiblingOfType[Instruction].isDefined) nextSiblingOfType[Instruction].get.firstInstruction
    else if (enclosing[Instruction].isDefined) enclosing[Instruction].get.nextExecutionInstructions
    else Nil
  }


  // To Improve
  override def previousExecutionInstructions: List[Instruction] = {
    checkFather
    if (father.isInstanceOf[IfThenElse]) father.asInstanceOf[Instruction].previousExecutionInstructions
    else if (previousSiblingOfType[Instruction].isDefined) previousSiblingOfType[Instruction].get.lastInstruction
    else if (enclosing[Instruction].isDefined) enclosing[Instruction].get.previousExecutionInstructions
    else Nil
  }

  def firstInstruction: List[Instruction] = if (list.isEmpty) Nil else list.head.firstInstruction

  def lastInstruction: List[Instruction] = if (list.isEmpty) Nil else list.last.lastInstruction

  override def deleted(listBefore: List[Instruction], node: Instruction, listAfter: List[Instruction]) {
    node match {
      case d: Declaration =>
        //TODO: declaration could be deleted if not referenced, a suitable query could be performed here, but better to use the ASTReference feature to implement it.
        throw ASTException("cannot delete declaration " + d.name + " because it's not yet implemented")
      case _ =>
    }

    //les autres peuvent être deletées / remplacées.
  }

  /**
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String =
    SpacesAsHead(indent, head) + "{\n" + _printInstr(indent + 2, head) + "\n" + Spaces(indent) + "}"

  def _printInstr(indent: Int, head: Boolean) = list.map((i: Instruction) => i.printInstr(indent, head = true)).mkString("\n")


  def printInstrList(indent: Int): String = {
    list.map((i: Instruction) => i.printInstr(indent, head = true)).mkString("\n")
  }

}

case class For(
                var initialDecl: Option[Instruction],
                var internalCond: Expression,
                var loopExpr: Option[Expression],
                var internalinstr: Instruction)
  extends Instruction with DeclarationScope {

  if (internalCond != null) internalCond.father = this
  internalinstr.father = this

  initialDecl match {
    case Some(d: Declaration) => addDeclaration(d); d.father = this
    case Some(s) => s.father = this
    case _ =>
  }

  def cond: Expression = internalCond

  def cond_=(c: Expression) {
    if (internalCond.father == this) internalCond.father = null
    internalCond = c
    c.father = this
    if (!c.isInstanceOf[Integral])
      throw ASTException("only integral type accepted for condition of For")
  }

  def instr: Instruction = internalinstr

  def instr_=(i: Instruction) {
    if (internalinstr.father == this) internalinstr.father = null
    internalinstr = i
    i.father = this
  }

  override def clone = While(internalCond.clone(), internalinstr.clone())

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == cond) cond = by.asInstanceOf[Expression]
    else if (child == instr) instr = by.asInstanceOf[Instruction]
    else throw ASTException("cannot find node " + child + " in " + this)
  }

  def children: List[AstNode] = List(cond, instr)

  def initialExprStr = initialDecl match {
    case None => ";"
    case Some(x) => x match {
      case s: VarDecl => s.toString + ";"
      case s => s.toString
    }
  }

  def loopExprStr = loopExpr match {
    case None => ""
    case Some(x) => x.printMe()
  }

  def internalStr = initialExprStr + cond.printMe + ";" + loopExprStr

  override def toString: String = "for(" + internalStr + ")" + instr

  def firstInstruction: List[Instruction] = instr.firstInstruction ::: nextExecutionInstructions.head.firstInstruction

  def lastInstruction: List[Instruction] = instr.lastInstruction

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = {
    SpacesAsHead(indent, head) + "for(" + internalStr + ")" + instr.printInstr(indent + 2, head = false)
  }

  def replaceByWhile() = {
    val expr1 = initialDecl
    val expr2 = internalCond
    val expr3 = loopExpr

    val cwhile = While(expr2, internalinstr)
    replaceBy(cwhile)
    expr1 match {
      case Some(x) => cwhile.insertBefore(x)
      case None =>
    }

    expr3 match {
      case Some(x) => cwhile.instr match {
        case b: Block => b.append(ExpressionInstr(x))
        case i: Instruction => cwhile.instr = Block(List(i, ExpressionInstr(x)))
      }
      case None =>
    }
  }
}

case class While(private var internalCond: Expression, private var internalinstr: Instruction) extends Instruction {
  def cond: Expression = internalCond

  def instr: Instruction = internalinstr

  def cond_=(c: Expression) {
    if (internalCond.father == this) internalCond.father = null
    internalCond = c
    internalCond.father = this
    if (!c.resolvedType.isInstanceOf[Integral])
      throw ASTException("only integral type accepted for condition of While")
  }

  def instr_=(i: Instruction) {
    if (internalinstr.father == this) internalinstr.father = null
    internalinstr = i
    internalinstr.father = this
  }

  cond = internalCond
  instr = internalinstr


  override def clone = While(cond.clone(), instr.clone())

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == cond) cond = by.asInstanceOf[Expression]
    else if (child == instr) instr = by.asInstanceOf[Instruction]
    else throw ASTException("cannot find node " + child + " in " + this)
  }

  def children: List[AstNode] = List(cond, instr)

  override def toString: String = "while(" + cond.printMe + ")\n" + instr


  override def nextExecutionInstructions: List[Instruction] = this.firstInstruction

  def firstInstruction: List[Instruction] = instr.firstInstruction ::: (
    if (nextSiblingOfType[Instruction].isDefined) nextSiblingOfType[Instruction].get.firstInstruction
    else {
      if (enclosing[Instruction].isDefined) enclosing[Instruction].get.nextExecutionInstructions else Nil
    }
    )

  def lastInstruction: List[Instruction] = instr.lastInstruction

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = {
    SpacesAsHead(indent, head) + "while(" + cond.printMe + ")\n" + SpacesAsHead(indent, head) + instr.printInstr(indent, head = false)
  }

  def extractCondition(replacement: VarDecl) = if (internalCond != null) {
    replacement.init = Some(Nequ(cond.clone.encloseIfNeeded, Constant(0, replacement.declaredType, "0")))
    val update = ExpressionInstr(
      Affect(
        replacement.asVariable,
        Nequ(cond.clone.encloseIfNeeded,
          Constant(0, replacement.declaredType, "0"))
      ))

    cond = replacement.asVariable
    instr match {
      case b: Block => b.append(update)
      case i: Instruction => i.replaceBy(Block(List(i, update)))
    }

    insertBefore(replacement)
  }
}

case class DoWhile(var internalCond: Expression, var internalinstr: Instruction) extends Instruction {
  if (internalCond != null) internalCond.father = this
  if (internalinstr != null) internalinstr.father = this

  def cond: Expression = internalCond

  def cond_=(c: Expression) {
    if (internalCond.father == this) internalCond.father = null
    internalCond = c
    c.father = this
    if (!c.resolvedType.isInstanceOf[Integral])
      throw ASTException("only integral type accepted for condition of DoWhile")
  }

  def instr: Instruction = internalinstr

  def instr_=(i: Instruction) {
    if (internalinstr.father == this) internalinstr.father = null
    internalinstr = i
    i.father = this
  }

  override def clone = DoWhile(internalCond.clone(), internalinstr.clone())

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == cond) cond = by.asInstanceOf[Expression]
    else if (child == instr) instr = by.asInstanceOf[Instruction]
    else throw ASTException("cannot find node " + child + " in " + this)
  }

  def children: List[AstNode] = List(cond, instr)

  override def toString: String = "do" + instr + "while(" + cond.printMe + ");"

  def firstInstruction: List[Instruction] = instr.firstInstruction

  def lastInstruction: List[Instruction] = instr.lastInstruction

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = {
    SpacesAsHead(indent, head) + "do" + instr.printInstr(indent + 2, head = false) + "while(" + cond.printMe + ");"
  }

  def extractCondition(replacement: VarDecl) = if (internalCond != null) {
    replacement.init = Some(Nequ(cond.clone.encloseIfNeeded, Constant(0, replacement.declaredType, "0")))
    val update = ExpressionInstr(
      Affect(
        replacement.asVariable,
        Nequ(cond.clone.encloseIfNeeded,
          Constant(0, replacement.declaredType, "0"))
      ))

    cond = replacement.asVariable
    instr match {
      case b: Block => b.append(update)
      case i: Instruction => i.replaceBy(Block(List(i, update)))
    }

    insertBefore(replacement)
  }
}

case class Return(var internalvalue: Option[Expression] = None) extends Instruction {
  def children: List[AstNode] = value match {
    case Some(x) => List(x)
    case None => List.empty
  }

  value match {
    case Some(x) => x.father = this
    case None =>
  }

  internalvalue match {
    case Some(e) => e.father = this
    case None =>
  }

  override def clone = Return(if (internalvalue.isDefined) Some(internalvalue.get.clone()) else None)

  def value: Option[Expression] = internalvalue

  def value_=(e: Option[Expression]) {
    value match {
      case Some(expr) => expr.father = null
      case None =>
    }
    e match {
      case Some(expr) => expr.father = this
      case None =>
    }
    internalvalue = e
  }

  override def replaceChild(child: AstNode, by: AstNode) {
    value match {
      case Some(e) =>
        if (child == e) {
          value = Some(by.asInstanceOf[Expression])
        } else throw ASTException("child not found")
      case None =>
        value = Some(by.asInstanceOf[Expression])
    }
  }

  def removeParenthesis() {
    this.descendantsOfType[Parenthesis]().foreach(par => {
      par.replaceBy(par.internalE)
    })
  }

  def simplify(tvg: VariableGenerator) = {
    internalvalue match {
      case Some(e) =>
        removeParenthesis()
        e.extractFrom(this, tvg)
      case None =>
    }
  }


  def firstInstruction: List[Instruction] = List(this)

  def lastInstruction: List[Instruction] = List(this)

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + "return " +
    (value match {
      case None => ";"
      case Some(e) => e + ";"
    })
}

case class SwitchCase(var switchvalue: Expression, var cases: CaseList) extends Instruction {
  def children: List[AstNode] = List(switchvalue, cases)

  def firstInstruction: List[Instruction] = cases.firstInstruction

  def lastInstruction: List[Instruction] = cases.lastInstruction

  override def clone = SwitchCase(switchvalue.clone(), cases.clone)

  def labels = cases.internalCases.map(c => c.matchedValue)

  def caseForLabel(e: Expression) = cases.internalCases.filter(_.matchedValue.get.eq(e)).headOption

  def defaultCase = cases.internalCases.filter(_.matchedValue.isEmpty).headOption

  def haveDefault = defaultCase.isDefined

  /** s
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String =
    SpacesAsHead(indent, head) + "switch (" + switchvalue + "){\n" +
      cases.printCaseList(indent + 1) + "\n" + Spaces(indent) + "}"
}

case class CaseList(var internalCases: List[Case]) extends ASTList[Case] {
  def list: List[Case] = internalCases

  def list_=(a: List[Case]) {
    internalCases = a
  }

  override def clone = new CaseList(internalCases.map(c => c.clone))

  /** Called when some node was inserted
    * no assurance of comformance with the list at this point */
  override def inserted(listBefore: List[Case], node: Case, listAfter: List[Case]) {
    if (node.isDefault) {
      if (!listAfter.isEmpty) throw ASTException("default case mut be last one")
    } else {
      for (c <- listBefore)
        if (c.isDefault) throw ASTException("default case mut be last one")
    }

  }

  def firstInstruction: List[Instruction] = {
    var result = List[Instruction]();
    for (aCase <- internalCases) result = result ::: aCase.firstInstruction
    result
  }

  def lastInstruction: List[Instruction] = {
    var result = List[Instruction]()
    for (aCase <- internalCases if aCase.haveBreak || aCase.isDefault) result = result ::: aCase.lastInstruction
    result
  }

  def labels = internalCases.map(c => c.matchedValue)

  def caseForLabel(e: Expression) = internalCases.filter(_.matchedValue.get.eq(e)).headOption

  def defaultCase = internalCases.filter(_.matchedValue.isEmpty).headOption

  def haveDefault = defaultCase.isDefined

  def printCaseList(indent: Int): String = {
    internalCases.map((c: Case) => c.printCase(indent + 1)).mkString("\n")
  }
}

case class Case(var matchedValue: Option[Expression], block: Block) extends AstNode {

  //TODO: check type of matched
  //TODO: provide suitable block printing for this.
  def children: List[AstNode] = matchedValue match {
    case None => List(block);
    case Some(e) => List(e, block)
  }

  def isDefault: Boolean = matchedValue match {
    case None => true;
    case _ => false
  }

  def haveBreak: Boolean = descendantsOfType[Break]().size != 0

  def firstInstruction: List[Instruction] = block.firstInstruction

  def lastInstruction: List[Instruction] = block.lastInstruction

  override def clone = new Case(if (matchedValue.isDefined) Some(matchedValue.get.clone) else None, block.clone)

  def printCase(indent: Int): String =
    Spaces(indent) + (matchedValue match {
      case None => "default:";
      case Some(e) => "case " + e + ":"
    }) + "\n" + block.printInstrList(indent + 1)
}

case class Continue() extends Instruction() {
  def children: List[AstNode] = List.empty

  def firstInstruction: List[Instruction] = List(this)

  def lastInstruction: List[Instruction] = List(this)

  override def clone = Continue()

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + "continue;"
}


case class Break() extends Instruction() {
  def children: List[AstNode] = List.empty

  def firstInstruction: List[Instruction] = List(this)

  def lastInstruction: List[Instruction] = List(this)

  override def clone = Break()

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + "break;"
}

case class Nop() extends Instruction() {
  def children: List[AstNode] = List.empty

  def firstInstruction: List[Instruction] = List(this)

  def lastInstruction: List[Instruction] = List(this)

  override def clone = Nop()

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + ";"
}

case class ExpressionInstr(var e: Expression) extends Instruction() {
  def children: List[AstNode] = List(e)

  e.father = this

  override def clone = ExpressionInstr(e.clone)

  override def replaceChild(child: AstNode, by: AstNode) {
    if (child == e) {
      e.father = null
      e = by.asInstanceOf[Expression]
      e.father = this
    } else {
      throw ASTException("unknown child")
    }
  }

  def duplicateWithFunction(f: (ExpressionInstr) => List[Instruction]) = {
    var fPrime = f(this)
    if (fPrime != null) insertAfter(fPrime: _*)
  }

  def replaceWithFunction(f: (ExpressionInstr) => List[Instruction]) = {
    var fPrime = f(this)
    if (fPrime != null && !fPrime.isEmpty) {
      if (fPrime.size > 1) {
        replaceBy(fPrime.head)
        fPrime.head.insertAfter(fPrime.tail: _*)
      }
      else if (!fPrime.equals(this)) {
        replaceBy(fPrime.head)
      }
    }
  }

  def firstInstruction: List[Instruction] = this :: Nil

  def removeParenthesis() {
    this.descendantsOfType[Parenthesis]().foreach(par => {
      par.replaceBy(par.internalE)
    })
  }


  def simplify(tvg: VariableGenerator) = {
    Logger.getLogger("Instruction").log(Level.INFO, "Simplifying instruction " + this)
    removeParenthesis()
    (e match {
      case expr: Affect =>
        expr.left match {
          case l: ArrayAccess => List(expr.left.asInstanceOf[ArrayAccess].index, expr.right)
          case l => if (l.isComplex) List(expr.right)
          else expr.right.children
        }
      case expr: Call => expr.children
      case _ => Nil
    }).foreach(e => e.asInstanceOf[Expression].extractFrom(this, tvg))
  }

  def lastInstruction: List[Instruction] = this :: Nil

  /**
    * builds the string representation onf an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + e.printMe + ";"
}

case class LabelledStmt(
                         private var _label: String, private var _stmt: Instruction
                       ) extends Instruction with Subject {

  stmt = _stmt

  override def firstInstruction: List[Instruction] = List(stmt)

  override def lastInstruction: List[Instruction] = List(stmt)

  def label = _label

  def label_=(a: String) {
    _label = a
    notifyObservers(null)
  }

  def stmt = _stmt

  def stmt_=(a: Instruction) {
    _stmt = a
    stmt.father = this
    notifyObservers(null)
  }

  def gotos = observers.asInstanceOf[List[Goto]]

  /**
    * builds the string representation of an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  override def printInstr(indent: Int, head: Boolean): String =
    SpacesAsHead(indent, head) + label + ":\n" + stmt.printInstr(indent, head)

  override def children: List[AstNode] = List(stmt)

  override def replaceChild(child: AstNode, by: AstNode) = {
    child match {
      case c: Instruction =>
        assert(c equals stmt)
        by match {
          case b: Instruction => stmt.father = null; stmt = b; stmt.father = this
          case _ => throw ASTException(by + " must be a statement ")
        }
      case _ => throw ASTException(child + " is not a statement ")
    }
  }
}


case class GotoUnlinked(label: String) extends Instruction {
  override def firstInstruction: List[Instruction] = Nil

  override def lastInstruction: List[Instruction] = Nil

  /**
    * builds the string representation of an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  override def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + "goto " + label + ";"

  override def children: List[AstNode] = Nil
}

case class Goto(private var _target: LabelledStmt) extends Instruction with Observer {
  override def firstInstruction: List[Instruction] = target.firstInstruction

  override def lastInstruction: List[Instruction] = target.lastInstruction

  def label = target.label

  def target = _target

  def target_=(a: LabelledStmt) {
    _target.unregister(this)
    _target = a
    _target.register(this)
  }

  def link() = target.register(this)


  /**
    * builds the string representation of an instr. it is never finished by a "\n" so add one if you want.
    * also, it does not include the ";" at the end.
    *
    * @param indent the number of space to print before starting the instruction
    * @param head   true if starts on a new line, false otherwise
    * @return
    */
  override def printInstr(indent: Int, head: Boolean): String = SpacesAsHead(indent, head) + "goto " + label + ";"

  override def children: List[AstNode] = Nil

  override protected def _listen(sender: Subject, e: Event): Unit = {
    assert(this.enclosingOrDie[FunDef] == target.enclosingOrDie[FunDef])
  }
}

object Instruction {
  implicit def ExpressionToInstruction(e: Expression): Instruction = ExpressionInstr(e)
}



