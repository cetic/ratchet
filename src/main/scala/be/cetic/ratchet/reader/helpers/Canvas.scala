package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.ast.{Affect, ExpressionInstr, VarDecl, Variable, _}
import be.cetic.ratchet.reader.combinators.CParser
import be.cetic.ratchet.utils.TransformationException

import scala.util.matching.Regex


abstract class CanvasLine extends AstNode {
  def replace: Instruction
}


case class Macro(pattern: Regex, defaultArgs: List[Any], additionalParams: Int, instanciator: (List[Any]) => CanvasLine) {
  def instanciate(args: List[Any]) = {
    instanciator(defaultArgs ::: args)
  }
}


private case class InstructionLine(instr: Instruction) extends CanvasLine {
  instr.father = this

  def replace: Instruction = instr.clone

  override def children: List[AstNode] = List(instr)
}


private case class DeclarationLine(name: String, decl: Declaration) extends CanvasLine {
  decl.father = this

  def replace: Instruction = decl.clone

  override def children: List[AstNode] = List(decl)
}


/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
abstract class Canvas(model: String, vars: List[VarDecl], macros: List[Macro]) {
  var _nbTmpVars = -1
  var value = Map[String, List[Expression]]()
  var tmpDecl = Map[String, VarDecl]()
  var canvas = List[CanvasLine]()

  val TMP_VAR_NAME = "tmp"
  val tmpVar_pattern = "(tmp\\d+)|tmp".r

  def tmpVars: List[Declaration] = if (nbTmpVars > 1) {
    (1 until nbTmpVars + 1).map(x => VarDecl("tmp" + x)).toList
  }
  else if (nbTmpVars == 1) {
    List(VarDecl("tmp"))
  }
  else Nil

  def nbTmpVars = {
    if (_nbTmpVars < 0) _nbTmpVars = tmpVar_pattern.findAllIn(model).toList.distinct.size
    _nbTmpVars
  }

  preprocess()

  private def handle(macros: List[Macro], instr: String, vars: List[Declaration]): Option[CanvasLine] = {
    if (macros.size == 0) None
    else {
      var tempMacros: List[Macro] = macros
      var canvasLine: Option[CanvasLine] = None
      val parser = new CParser
      parser.init(vars)
      while (canvasLine.isEmpty && tempMacros.size != 0) {
        tempMacros.head.pattern findPrefixMatchOf instr match {
          case Some(x) =>
            if (x.groupCount != 0) {
              canvasLine = Some(tempMacros.head.instanciate(
                parser.parseAll(parser.argument_expression_list, x.subgroups(0)).get
              ))
            }
            else {
              canvasLine = Some(tempMacros.head.instanciate(Nil))
            }
          case None =>
        }
        tempMacros = tempMacros.tail
      }
      canvasLine
    }
  }

  private def preprocess(instr: String, vars: List[Declaration]): CanvasLine = {
    handle(macros, instr, vars) match {
      case Some(x) => x
      case None =>
        try {
          val parser = new CParser
          parser.init(vars)
          InstructionLine(parser.parseAll(parser.statement, instr).get)
        }
        catch {
          case e: Exception =>
            Logger.getLogger("Canvas").log(Level.SEVERE, "error during parsing of " + instr)
            throw e

        }
    }
  }

  def temporaryType: Type

  private def preprocess(): Unit = {
    val instructions = model.replace("\r", "").split("\n").toList.map(p => p.trim()).filter(p => !p.isEmpty)
    val initialVariables = vars.asInstanceOf[List[Declaration]] ::: tmpVars

    canvas = instructions.map(preprocess(_, initialVariables)).toList

    // foreach temporary variable, at the first use, add the corresponding declaration
    var varsToFound = tmpVars.map(_.name)
    canvas = canvas.map {
      case InstructionLine(ExpressionInstr(Affect(v: Variable, y))) if varsToFound.contains(v.name) =>
        varsToFound = varsToFound diff List(v.name)
        val decl = VarDecl(v.name, static = false, extern = false, temporaryType, Some(y))
        tmpDecl = tmpDecl.+((v.name, decl))
        DeclarationLine(v.name, decl)
      case x => x
    }

    // Linking all variables with their declaration
    canvas.map(instr => instr.descendantsOfType[Variable]()).flatten.foreach(v =>
      value = value + (v.name -> (v :: value.get(v.name).getOrElse(Nil))))
  }

  private def generateTmpVars(tmpVarsMapping: Map[String, VarDecl]): List[CanvasLine] = canvas.map {
    case DeclarationLine(n, VarDecl(nv, s, e, t, i)) if tmpVarsMapping.keySet.contains(n) =>
      tmpVarsMapping(n).init = i
      DeclarationLine(n, tmpVarsMapping(n))
    case y => y
  }

  def instantiate(vals: Map[String, Expression], tvg: VariableGenerator, tmptype: Type) = {
    //Check all variable have a valuation
    val tmpVarReplacement = tvg.next(TMP_VAR_NAME, tmptype, tmpVars.size)
    val tmpVarsMapping = (0 until tmpVars.size).map(x => (tmpVars(x).name, tmpVarReplacement(x))).toMap
    val valuation = vals.++(tmpVarsMapping.map(x => (x._1, x._2.asVariable)))

    canvas = generateTmpVars(tmpVarsMapping)

    // check if all variable has a valuation
    val d = value.keySet diff valuation.keySet
    if (!d.isEmpty) {
      throw new Exception("Not all is defined : " + d + " in " + value.keySet + "  " + valuation.keySet)
    }

    // replace all variable by its valuation
    valuation.keys.foreach(name => {
      try {
        value = value + (name -> replaceVariable(name, valuation.get(name).get))
      }
      catch {
        case e: Error if !name.matches("tmp\\d*") => throw e
        case e: Throwable => Logger.getLogger("Canvas").log(Level.SEVERE, "unused variable " + name); e.printStackTrace()
      }
    })

    // Compute the final representation of each line

    canvas.foreach {
      case DeclarationLine(n, decl: VarDecl) =>
        decl.declaredType = tmptype //decl.init.get.computeType
        Logger.getLogger("Canvas").log(Level.SEVERE, "type" + tmptype)
        val cause = decl.init + " is typed by " + decl.init.get.typeTree
        Logger.getLogger("Canvas").log(Level.INFO, "Setting type of " + decl.name + " to "
          + decl.declaredType + " because " + cause)
      case _ =>
    }
    val finalrep = canvas.map(in => in.replace)
    var map = Map[String, VarDecl]()
    finalrep.foreach(in => in.descendantsOfType[VarDecl]().foreach(v => map = map + (v.name -> v)))
    finalrep.foreach(in => in.descendantsOfType[Variable]().foreach(v => {
      if (map.contains(v.name)) v.replaceBy(Variable(map(v.name)))
    }))

    finalrep
  }

  private def replaceVariable(name: String, replacement: Expression) = value.get(name) match {
    case None => throw new Error("no variable with name : " + name + " in " + value.keys.mkString(","))
    case Some(x) => x.map(v => {
      try {
        val r = replacement.clone
        v.replaceBy(r)
        r
      }
      catch {
        case e: Exception => throw new TransformationException(
          "Replacement error from \"" + v + "\" to \"" + replacement + "\"", e)
      }
    })
  }

}
