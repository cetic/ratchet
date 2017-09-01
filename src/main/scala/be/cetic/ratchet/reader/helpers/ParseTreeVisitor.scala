package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast._

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */

case class VisitNotImplemented(message: String, cause: Throwable = null)
  extends Exception(message, cause);

trait ParseTreeVisitor {

  def visit(n: AstTree)

  def visit(n: AstNode)

  def visit(n: FullSoftwareCode)

  def visit(n: FunDef)

  def visit(n: TranslationUnit)

  def visit(n: Declaration)

  def visit(n: EnumDecl)

  def visit(n: EnumElement)

  def visit(n: FunDecl)

  def visit(n: StructDecl)

  def visit(n: StructPart)

  def visit(n: TypeDecl)

  def visit(n: UnionDecl)

  def visit(n: VarDecl)

  def visit(n: Instruction)

  def visit(n: Block)

  def visit(n: Break)

  def visit(n: Case)

  def visit(n: CaseList)

  def visit(n: Continue)

  def visit(n: DoWhile)

  def visit(n: ExpressionInstr)

  def visit(n: For)

  def visit(n: Goto)

  def visit(n: IfThenElse)

  def visit(n: LabelledStmt)

  def visit(n: Nop)

  def visit(n: Return)

  def visit(n: SwitchCase)

  def visit(n: While)

  def visit(n: Expression)

  def visit(n: LogicalOr)

  def visit(n: LogicalAnd)

  def visit(n: BitAnd)

  def visit(n: BitOr)

  def visit(n: BitXor)

  def visit(n: Plus)

  def visit(n: Minus)

  def visit(n: Mod)

  def visit(n: Times)

  def visit(n: Div)

  def visit(n: Equ)

  def visit(n: Nequ)

  def visit(n: G)

  def visit(n: L)

  def visit(n: Ge)

  def visit(n: Le)

  def visit(n: ShiftLeft)

  def visit(n: ShiftRight)

  def visit(n: ArrayAccess)

  def visit(n: Affect)

  def visit(n: CompoundPlus)

  def visit(n: CompoundMinus)

  def visit(n: CompoundTimes)

  def visit(n: CompoundDiv)

  def visit(n: CompoundMod)

  def visit(n: CompoundShiftLeft)

  def visit(n: CompoundShiftRight)

  def visit(n: CompoundAnd)

  def visit(n: CompoundOr)

  def visit(n: CompoundXor)

  def visit(n: PostFixPlus)

  def visit(n: PostFixMinus)

  def visit(n: Not)

  def visit(n: BitNot)

  def visit(n: Neg)

  def visit(n: Adr)

  def visit(n: DecrementPrefix)

  def visit(n: IncrementPrefix)

  def visit(n: Ptr)

  def visit(n: UPlus)

  def visit(n: SizeOfE)

  def visit(n: SizeOfT)

  def visit(n: FieldAccess)

  def visit(n: IndirectFieldAccess)

  def visit(n: Parenthesis)

  def visit(n: Variable)

  def visit(n: EnumElementExpr)

  def visit(n: Constant)

  def visit(n: Cast)

  def visit(n: Call)

  def visit(v: Visitable): Unit = v match {
    case n: FullSoftwareCode => visit(n)
    case n: FunDef => visit(n)
    case n: TranslationUnit => visit(n)

    // declaration
    case n: EnumDecl => visit(n)
    case n: EnumElement => visit(n)
    case n: FunDecl => visit(n)
    case n: StructDecl => visit(n)
    case n: StructPart => visit(n)
    case n: TypeDecl => visit(n)
    case n: UnionDecl => visit(n)
    case n: VarDecl => visit(n)
    case n: Declaration => visit(n)

    // instruction
    case n: Block => visit(n)
    case n: Break => visit(n)
    case n: Case => visit(n)
    case n: CaseList => visit(n)
    case n: Continue => visit(n)
    case n: DoWhile => visit(n)
    case n: ExpressionInstr => visit(n)
    case n: For => visit(n)
    case n: Goto => visit(n)
    case n: IfThenElse => visit(n)
    case n: LabelledStmt => visit(n)
    case n: Nop => visit(n)
    case n: Return => visit(n)
    case n: SwitchCase => visit(n)
    case n: While => visit(n)
    case n: Instruction => visit(n)

    // expression
    case n: LogicalOr => visit(n)
    case n: LogicalAnd => visit(n)
    case n: BitAnd => visit(n)
    case n: BitOr => visit(n)
    case n: BitXor => visit(n)
    case n: Plus => visit(n)
    case n: Minus => visit(n)
    case n: Mod => visit(n)
    case n: Times => visit(n)
    case n: Div => visit(n)
    case n: Equ => visit(n)
    case n: Nequ => visit(n)
    case n: G => visit(n)
    case n: L => visit(n)
    case n: Ge => visit(n)
    case n: Le => visit(n)
    case n: ShiftLeft => visit(n)
    case n: ShiftRight => visit(n)
    case n: ArrayAccess => visit(n)
    case n: Affect => visit(n)
    case n: CompoundPlus => visit(n)
    case n: CompoundMinus => visit(n)
    case n: CompoundTimes => visit(n)
    case n: CompoundDiv => visit(n)
    case n: CompoundMod => visit(n)
    case n: CompoundShiftLeft => visit(n)
    case n: CompoundShiftRight => visit(n)
    case n: CompoundAnd => visit(n)
    case n: CompoundOr => visit(n)
    case n: CompoundXor => visit(n)
    case n: PostFixPlus => visit(n)
    case n: PostFixMinus => visit(n)
    case n: Not => visit(n)
    case n: BitNot => visit(n)
    case n: Neg => visit(n)
    case n: Adr => visit(n)
    case n: DecrementPrefix => visit(n)
    case n: IncrementPrefix => visit(n)
    case n: Ptr => visit(n)
    case n: UPlus => visit(n)
    case n: SizeOfE => visit(n)
    case n: SizeOfT => visit(n)
    case n: FieldAccess => visit(n)
    case n: IndirectFieldAccess => visit(n)
    case n: Parenthesis => visit(n)
    case n: Variable => visit(n)
    case n: EnumElementExpr => visit(n)
    case n: Constant => visit(n)
    case n: Cast => visit(n)
    case n: Call => visit(n)
    case n: Expression => visit(n)

    case n: AstTree => visit(n)
    case n: AstNode => visit(n)
    case _ => throw VisitNotImplemented("Not yet implemented for :" + v.getClass.getName)
  }
}


trait Visitable {
  def accept(ptv: ParseTreeVisitor) = ptv.visit(this)
}
