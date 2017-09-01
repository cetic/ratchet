package be.cetic.ratchet.transformer

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.TypeMgr
import be.cetic.ratchet.reader.ast._
import be.cetic.ratchet.reader.helpers.VariableGenerator

/**
  * This file contains all simplification tasks applied just before the application of the hardening rules.
  *
  * These tasks ensure that hardener input is supportable.
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
case class Simplification(tvg: VariableGenerator,structPrefix:String,returnVarName:String)
  extends Prehandler(List(
    TypedefSimplication(tvg,structPrefix),
    InsertBlock(tvg),
    ForSimplification(tvg),
    DoWhiledSimplication(tvg),
    IfSimplication(tvg),
    WhileSimplification(tvg),
    AnonymousStruct(tvg,structPrefix),
    StructSimplification(tvg),
    ExtractComplexFunctionArgs(tvg),
    FunctionCallSimplication(tvg),
    SplitMultipleExpression(tvg), // Must be always at this place !!!!!!
    FunctionSimplication(tvg,returnVarName),
    CastToBool(tvg),
    PreIncAndDecSimplication(tvg),
    SplitInitialisation(), // Must be always at this place !!!!!!
    ModSimplication(tvg),
    LogicalSimplification(tvg),
    InPlaceOperatorSimplification(tvg)
  )) {}

case class IfSimplication(tvg: VariableGenerator) extends Transformation("IfSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[IfThenElse]().foreach(f => f.extractCondition(tvg.next(TypeMgr.TMP_NAME, TypeMgr.BOOL)))
    software
  }
}


case class InsertBlock(tvg: VariableGenerator) extends Transformation("Ensure that block exists") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[IfThenElse]().foreach(f => handle(f))
    software.root.descendantsOfType[While]().foreach(f => handle(f))
    software.root.descendantsOfType[DoWhile]().foreach(f => handle(f))
    software.root.descendantsOfType[For]().foreach(f => handle(f))
    software
  }

  def handle(ite: IfThenElse) {
    ite.thenInstr match {
      case b: Block =>
      case x => ite.thenInstr = Block(List(x))
    }
    ite.elseInstr match {
      case Some(b: Block) =>
      case Some(x) => ite.elseInstr = Block(List(x))
      case None =>
    }
  }

  def handle(wh: While) {
    wh.instr match {
      case b: Block =>
      case x => wh.instr = Block(List(x))
    }
  }

  def handle(wh: DoWhile) {
    wh.instr match {
      case b: Block =>
      case x => wh.instr = Block(List(x))
    }
  }

  def handle(fo: For) {
    fo.instr match {
      case b: Block =>
      case x => fo.instr = Block(List(x))
    }
  }
}


/**
  * Note : We always obtain a block of instructions in "{}"
  *
  * @param tvg Generator of Kafka variable
  */
case class WhileSimplification(tvg: VariableGenerator) extends Transformation("WhileSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[While]().foreach(f => f.extractCondition(tvg.next(TypeMgr.TMP_NAME, TypeMgr.BOOL)))
    software
  }
}


case class ForSimplification(tvg: VariableGenerator) extends Transformation("ForSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[For]().foreach(_.replaceByWhile())
    software
  }
}

case class DoWhiledSimplication(tvg: VariableGenerator) extends Transformation("DoWhiledSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[DoWhile]().foreach(f => f.extractCondition(tvg.next(TypeMgr.TMP_NAME, TypeMgr.BOOL)))
    software
  }
}


case class InPlaceOperatorSimplification(tvg: VariableGenerator) extends Transformation("InPlaceOperatorSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[AssignementExpression]().foreach(_.replaceByBinOp())
    software
  }
}

case class AnonymousStruct(tvg: VariableGenerator, structPrefix:String) extends Transformation("AnonymousStruct") {
  override def apply(software: AstTree): AstTree = {
    software.vardecls.filter(
      d => d.declaredType.isInstanceOf[Struct] || d.declaredType.isInstanceOf[ArrayType]
    ).foreach(d => {
      Logger.getLogger("Simplification").log(Level.INFO, "performing " + this.name + " on \"" + d + "\"")
      handle(d)
    })

    software
  }


  def handle(d: VarDecl) = d.declaredType match {
    case s: Struct if s.name.isEmpty =>
      d.declaredType = Struct(s.fields, "struct_" + structPrefix + d.name)
    case ArrayType(s: Struct, n) if s.name.isEmpty =>
      d.declaredType = ArrayType(Struct(s.fields, "struct_" + structPrefix + d.name), n)
    case _ =>
  }
}

case class StructSimplification(tvg: VariableGenerator) extends Transformation("StructSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.vardecls.foreach(d => d.declaredType match {
      case s: Struct if !s.name.isEmpty => handleStructDeclWithVar(d, s)
      case ArrayType(s: Struct, n) if !s.name.isEmpty => handleStructDeclWithVarArray(d, s, n)
      case _ =>
    })

    software
  }

  def handleStructDeclWithVar(d: VarDecl, struct: Struct) {
    val decl = StructDecl(struct)
    d.insertBefore(decl)
    d.declaredType = StructReference(decl)
  }

  def handleStructDeclWithVarArray(d: VarDecl, struct: Struct, n: Option[Int]) {
    val decl = StructDecl(struct)
    d.insertBefore(decl)
    d.declaredType = ArrayType(StructReference(decl), n)
  }
}


case class PreIncAndDecSimplication(tvg: VariableGenerator) extends Transformation("PreIncAndDecSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[PrefixUnaryOp]().foreach {
      case a@DecrementPrefix(e) => a.replaceBy(Affect(e, Minus(e, Constant(1, e.computeType, "1"))))
      case a@IncrementPrefix(e) => a.replaceBy(Affect(e, Plus(e, Constant(1, e.computeType, "1"))))
      case _ =>
    }
    software
  }

}

case class CastToBool(tvg: VariableGenerator) extends Transformation("CastToBool") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[Cast]().filter(_.to.equals(TypeMgr.BOOL)).foreach(cast => {
      cast.replaceBy(Nequ(cast.e, Constant(0L, IntType(32, signed = false), "0")))
    })
    software
  }
}


case class ModSimplication(tvg: VariableGenerator) extends Transformation("ModSimplication") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[ExpressionInstr]().foreach {
      case ExpressionInstr(v@Affect(left, s: Mod)) => simplify(v)
      case _ =>
    }
    software
  }

  def simplify(affect: Affect) = {
    val mod = affect.right.asInstanceOf[Mod]
    val tmp = tvg.next(TypeMgr.TMP_NAME, IntType(), 2)
    val tmp1 = tmp.head
    val tmp2 = tmp.tail.head

    val init1 = Div(mod.left, mod.right)
    val init2 = Times(tmp1.asVariable, mod.right)

    tmp1.declaredType = init1.computeType
    tmp2.declaredType = init2.computeType

    tmp1.init = Some(Div(mod.left, mod.right))
    tmp2.init = Some(Times(tmp1.asVariable, mod.right))
    val check = ExpressionInstr(Affect(affect.left, Minus(mod.left, tmp2.asVariable)))

    affect.checkFather
    affect.father.replaceBy(check)
    check.insertBefore(tmp1)
    check.insertBefore(tmp2)
  }
}


case class SplitInitialisation() extends Transformation("Split initialization") {
  override def apply(software: AstTree): AstTree = {
    software.vardecls.foreach(x => {
      Logger.getLogger("Simplification").log(Level.INFO, "Splitting initialization of variable declaration " +
        x.name + " " + x)
      x.init match {
        case None =>
        case Some(i) =>
          x.init = None
          x.insertAfter(ExpressionInstr(Affect(x.asVariable, i)))
      }
    }
    )
    software
  }
}


case class ExtractComplexFunctionArgs(tvg: VariableGenerator) extends Transformation("Split complex expr") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[Call]().foreach(_.extractComplexArgs(tvg))
    software
  }
}


case class SplitMultipleExpression(tvg: VariableGenerator) extends Transformation("Split complex expr") {
  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[ExpressionInstr]().foreach(_.simplify(tvg))
    software.root.descendantsOfType[Return]().foreach(_.simplify(tvg))
    software.vardecls.foreach(_.simplifyInit(tvg))
    software
  }
}


case class TypedefSimplication(tvg: VariableGenerator, structPrefix:String) extends Transformation("To manage type def struct") {
  override def apply(software: AstTree): AstTree = {
    val typed = software.root.descendantsOfType[TypeDecl]()

    typed.foreach(typedef => typedef.declaredType match {
      case s: Struct => simplify(typedef, s)
      case _ =>
    })

    software
  }

  def simplify(t: TypeDecl, s: Struct) {
    val struct = StructDecl(Struct(s.fields, "struct_" + structPrefix + t.name))
    t.enclosingOrDie[DeclarationScope].addDeclaration(struct)
    t.insertBefore(struct)
    val newt = TypeDecl(t.name, StructReference(struct))
    t.replaceByWithUsages(newt)
  }
}


case class FunctionCallSimplication(tvg: VariableGenerator) extends Transformation("Simplify Function") {
  override def apply(software: AstTree): AstTree = {
    software.calls.foreach(handleCall)
    software
  }


  def handleCall(call: Call) {
    val returnType = call.target.funType.returnType
    if (!returnType.isInstanceOf[Void]) {
      val expr = call.enclosingOrDie[Instruction]
      val tmp = tvg.next(TypeMgr.TMP_NAME, returnType)
      expr.insertBefore(tmp)
      val params = call.params ::: List(Adr(tmp.asVariable))
      expr.insertBefore(ExpressionInstr(Call(call.target, params)))
      call.replaceBy(tmp.asVariable)
    }
  }
}


case class FunctionSimplication(tvg: VariableGenerator,returnVarName:String) extends Transformation("Simplify Function") {
  override def apply(software: AstTree): AstTree = {
    software.fundecls.foreach(fundecl => if (fundecl.funType.isVoid) fundecl.funType.unsetVoid)
    software.fundecls.foreach(_.convertToProcedure(returnVarName, tvg))
    software
  }
}


case class LogicalSimplification(tvg: VariableGenerator) extends Transformation("LogicalSimplification") {

  override def apply(software: AstTree): AstTree = {
    software.root.descendantsOfType[LogicalBinOp]().foreach {
      case a@LogicalAnd(l, r) =>
        val bitand = BitAnd(l, r)
        a.replaceBy(bitand)
      case a@LogicalOr(l, r) =>
        val bitor = BitOr(l, r)
        a.replaceBy(bitor)
    }

    //software.root.descendantsOfType[Not]().foreach ( notExpr => notExpr.replaceBy(BitNot(notExpr.e)))
    software
  }
}


