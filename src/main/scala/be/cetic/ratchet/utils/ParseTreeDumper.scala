package be.cetic.ratchet.utils

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast._
import be.cetic.ratchet.reader.helpers.ParseTreeVisitor

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
class ParseTreeDumper(levelString: String = "\t") extends ParseTreeVisitor {

  private val buffer = new StringBuilder("")
  private var lvl = 0
  private val lvlc = levelString

  def reset = {
    buffer.clear()
    lvl = 0
  }

  def get = buffer.toString()

  def level = lvl

  private def indent = lvl = lvl + 1

  private def undent = lvl = lvl - 1

  private def indentStr = lvlc * lvl

  private def prehandling(in: String) = in.replace("\r", "").replace("\n", "\n" + indentStr).replaceAll("(\\ )+$", "")

  private def ap(str: String) = {
    buffer.append(indentStr)
    buffer.append(str)
  }

  private def ap(key: String, value: Any) {
    ap(key + ':' + value.toString + '\n')
  }

  private def enc(f: => Unit) = {
    indent; f; undent
  }

  override def visit(n: AstTree) = {
    ap("AstTree:")
    n.root.accept(this)
  }

  override def visit(n: AstNode) = visitSimpleNode(n)


  private def visitSimpleNode(n: AstNode) {
    ap(n.getClass.getSimpleName + ":\n")
    indent
    n.children.foreach(_.accept(this))
    undent
  }


  override def visit(n: FullSoftwareCode) = visitSimpleNode(n)

  override def visit(n: TranslationUnit) = visitSimpleNode(n)

  override def visit(n: Declaration) = visitSimpleNode(n)


  override def visit(n: UnionDecl) = {
    ap(n.getClass.getSimpleName + ":\n")
    enc {
      n.union.fields.foreach(_.accept(this))
    }
  }

  override def visit(n: FunDef) = {
    ap("Fundef: \n")
    enc {
      n.funDecl.accept(this)
      n.instructions.accept(this)
    }
  }

  override def visit(n: EnumDecl) = {
    ap("EnumDecl " + n.name + " :")
    enc {
      n.declaredEnum.elements.foreach(_.accept(this))
    }
  }

  override def visit(n: EnumElement) = {
    ap("EnumElement:")
    enc {
      ap("name", n.name)
      ap("value", n.value)
    }
  }

  override def visit(n: FunDecl) = {
    ap("Fundecl:\n")
    enc {
      ap("name", n.name)
      n.mtype match {
        case t: FunctionType => {
          ap("return type", t.returnType)
          ap("args:\n")
          enc {
            n.arguments.foreach(_.accept(this))
          }
        }
        case _ => ap("type", n.mtype)
      }
    }
  }

  override def visit(n: StructDecl) = {
    ap(n.getClass.getSimpleName + ":\n")
    enc {
      n.struct.fields.foreach(_.accept(this))
    }
  }

  override def visit(n: StructPart) = {
    n.decl.accept(this)
  }

  override def visit(n: TypeDecl) = {
    ap("TypeDecl:\n")
    enc {
      ap("name", n.name)
      ap("to", n.declaredType.toString)
    }
  }


  override def visit(n: VarDecl) = {
    ap(n.getClass.getSimpleName + ":\n")
    enc {
      ap("name", n.name)
      ap("extern", n.extern)
      ap("static", n.static)
      ap("type", n.declaredType)
      n.init match {
        case Some(n) => {
          ap("init:\n")
          enc {
            n.accept(this)
          }
        }
        case None =>
      }
    }
  }

  override def visit(n: Return) = {
    ap("RETURN:\n")
    n.value match {
      case Some(e) => enc {
        e.accept(this)
      }
      case None =>
    }
  }


  override def visit(n: For) = {
    ap("For:\n")
    enc {
      ap("init", n.initialDecl)
      ap("cond", n.cond)
      ap("loop", n.loopExpr)
      n.instr.accept(this)
    }
  }

  override def visit(n: IfThenElse) = {
    ap("If:\n")
    enc {
      ap("cond", n.cond)
      n.elseInstr match {
        case Some(e) => {
          ap("else:\n")
          enc {
            n.elseInstr
          }
        }
        case None =>
      }
    }
  }

  override def visit(n: DoWhile) = {
    ap("DoWhile:\n")
    enc {
      ap("cond", n.cond)
      n.instr.accept(this)
    }
  }

  override def visit(n: While) = {
    ap("While:\n")
    enc {
      ap("cond", n.cond)
      n.instr.accept(this)
    }
  }

  override def visit(n: Block) = visitSimpleNode(n)

  override def visit(n: Break) = ap("BREAK")

  override def visit(n: Case) = visitSimpleNode(n)

  override def visit(n: CaseList) = visitSimpleNode(n)

  override def visit(n: Continue) = ap("CONTINUE")

  override def visit(n: ExpressionInstr) = visitSimpleNode(n)

  override def visit(n: Goto) = visitSimpleNode(n)

  override def visit(n: Instruction) = visitSimpleNode(n)

  override def visit(n: LabelledStmt) = visitSimpleNode(n)

  override def visit(n: Nop) = ap("NOP")

  override def visit(n: SwitchCase) = visitSimpleNode(n)


  override def visit(n: Variable) = {
    ap("Variable:\n")
    enc {
      ap("name", n.name)
      ap("type", n.computeType)
    }
  }

  override def visit(n: Constant) = {
    ap("Constant:\n")
    enc {
      ap("value", n.value)
      ap("type", n.computeType)
      ap("string", n.parserString)
    }
  }

  override def visit(n: Cast) = {
    ap("Cast:\n")
    enc {
      n.e.accept(this)
      ap("to", n.computeType)
    }
  }

  override def visit(n: Call) = {
    ap("Call:\n")
    enc {
      ap("args")
      enc {
        n.params.foreach(_.accept(this))
      }
      ap("target", n.target.name)
    }
  }

  override def visit(n: EnumElementExpr) = {
    ap("EnumElementExpr:\n")
    enc {
      ap("name", n.enumElem.name)
      ap("type", n.computeType)
    }
  }

  override def visit(n: Expression) = visitSimpleNode(n)

  override def visit(n: LogicalOr) = visitSimpleNode(n)

  override def visit(n: LogicalAnd) = visitSimpleNode(n)

  override def visit(n: BitAnd) = visitSimpleNode(n)

  override def visit(n: BitOr) = visitSimpleNode(n)

  override def visit(n: BitXor) = visitSimpleNode(n)

  override def visit(n: Plus) = visitSimpleNode(n)

  override def visit(n: Minus) = visitSimpleNode(n)

  override def visit(n: Mod) = visitSimpleNode(n)

  override def visit(n: Times) = visitSimpleNode(n)

  override def visit(n: Div) = visitSimpleNode(n)

  override def visit(n: Equ) = visitSimpleNode(n)

  override def visit(n: Nequ) = visitSimpleNode(n)

  override def visit(n: G) = visitSimpleNode(n)

  override def visit(n: L) = visitSimpleNode(n)

  override def visit(n: Ge) = visitSimpleNode(n)

  override def visit(n: Le) = visitSimpleNode(n)

  override def visit(n: ShiftLeft) = visitSimpleNode(n)

  override def visit(n: ShiftRight) = visitSimpleNode(n)

  override def visit(n: Affect) = visitSimpleNode(n)

  override def visit(n: CompoundPlus) = visitSimpleNode(n)

  override def visit(n: CompoundMinus) = visitSimpleNode(n)

  override def visit(n: CompoundTimes) = visitSimpleNode(n)

  override def visit(n: CompoundDiv) = visitSimpleNode(n)

  override def visit(n: CompoundMod) = visitSimpleNode(n)

  override def visit(n: CompoundShiftLeft) = visitSimpleNode(n)

  override def visit(n: CompoundShiftRight) = visitSimpleNode(n)

  override def visit(n: CompoundAnd) = visitSimpleNode(n)

  override def visit(n: CompoundOr) = visitSimpleNode(n)

  override def visit(n: CompoundXor) = visitSimpleNode(n)

  override def visit(n: PostFixPlus) = visitSimpleNode(n)

  override def visit(n: PostFixMinus) = visitSimpleNode(n)

  override def visit(n: Not) = visitSimpleNode(n)

  override def visit(n: BitNot) = visitSimpleNode(n)

  override def visit(n: Neg) = visitSimpleNode(n)

  override def visit(n: Adr) = visitSimpleNode(n)

  override def visit(n: DecrementPrefix) = visitSimpleNode(n)

  override def visit(n: IncrementPrefix) = visitSimpleNode(n)

  override def visit(n: Ptr) = visitSimpleNode(n)

  override def visit(n: UPlus) = visitSimpleNode(n)

  override def visit(n: Parenthesis) = visitSimpleNode(n)

  override def visit(n: ArrayAccess) = visitSimpleNode(n)

  override def visit(n: FieldAccess) = visitSimpleNode(n)

  override def visit(n: IndirectFieldAccess) = visitSimpleNode(n)

  override def visit(n: SizeOfE) = visitSimpleNode(n)

  override def visit(n: SizeOfT) = visitSimpleNode(n)


}

