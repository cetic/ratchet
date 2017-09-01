package be.cetic.ratchet

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast.{IntType, TypeDecl, TypeDefReference, VarDecl, _}
import be.cetic.ratchet.reader.helpers.ASTException

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
object TypeMgr {
  val INT7 = TypeDefReference(TypeDecl("INT7", CharType(signed = true)))
  val INT8 = TypeDefReference(TypeDecl("INT8", CharType(signed = true)))
  val INT15 = TypeDefReference(TypeDecl("INT15", IntType(16, signed = true)))
  val INT16 = TypeDefReference(TypeDecl("INT16", IntType(16, signed = true)))
  val INT31 = TypeDefReference(TypeDecl("INT31", IntType(32, signed = true)))
  val INT32 = TypeDefReference(TypeDecl("INT32", IntType(32, signed = true)))
  val INT63 = TypeDefReference(TypeDecl("INT63", IntType(64, signed = true)))
  val INT64 = TypeDefReference(TypeDecl("INT64", IntType(64, signed = true)))

  val UINT7 = TypeDefReference(TypeDecl("UINT7", CharType(signed = false)))
  val UINT8 = TypeDefReference(TypeDecl("UINT8", CharType(signed = false)))
  val UINT15 = TypeDefReference(TypeDecl("UINT15", IntType(16, signed = false)))
  val UINT16 = TypeDefReference(TypeDecl("UINT16", IntType(16, signed = false)))
  val UINT31 = TypeDefReference(TypeDecl("UINT31", IntType(32, signed = false)))
  val UINT32 = TypeDefReference(TypeDecl("UINT32", IntType(32, signed = false)))
  val UINT63 = TypeDefReference(TypeDecl("UINT63", IntType(64, signed = false)))
  val UINT64 = TypeDefReference(TypeDecl("UINT64", IntType(64, signed = false)))

  val BOOL = TypeDefReference(TypeDecl("BOOL", IntType(32, signed = true)))

  val ALL_TYPES = List(
    UINT7, UINT8, UINT15, UINT16, UINT31, UINT32, UINT63, UINT64,
    INT7, INT8, INT15, INT16, INT31, INT32, INT63, INT64, BOOL)

  val ALL_TYPE_DECLS = ALL_TYPES.map(t => t.to)

  private var _TMP_TYPE: Type = INT32

  def TMP_TYPE = _TMP_TYPE

  def TMP_TYPE_=(a: Type) {
    _TMP_TYPE = a
  }

  val HEADER_EXT = "h"

  val TMP_NAME = "tmp"

  val FORMAT = "0x%016xULL"
  val FORMAT32 = "0x%08xUL"


  def format(id: Long) = FORMAT.format(id)

  def format32(id: Long) = FORMAT32.format(id.toInt)


  def shift(c: Constant, a: Int): Constant = c.constType match {
    case t: IntType => Constant(c.asLong << a, c.constType, (c.asLong << a).toString)
    case t: CharType => Constant(c.asLong << a, c.constType, (c.asLong << a).toString)
    case t: TypeDefReference => Constant(c.asLong << a, c.constType, (c.asLong << a).toString)
    case _ => throw new Exception(c.constType + " not managed")
  }

}


object TypeConverter {
  def migrate(ast: AstTree) = {
    // Constant to locotrac type
    ast.root.descendantsOfType[Constant]().foreach(c => c.replaceBy(Constant(c.value, map(c.constType),
      c.parserString)))

    ast.root.descendantsOfType[Declaration]().foreach {
      case d: VarDecl => d.declaredType = map(d.declaredType)
      case d: TypeDecl => //ignore
      case d: UnionDecl => d.union = map(d.union).asInstanceOf[Union]
      case d: StructDecl => d.struct = map(d.struct).asInstanceOf[Struct]
      case d: FunDef => //will be managed in fundecl
      case d: FunDecl => d.mtype = map(d.funType).asInstanceOf[FunctionType]
      case d: EnumDecl => //ignore
      case d: EnumElement => //ignore
      case _ => throw ASTException("Unsupported declaration")
    }

    ast
  }

  def migrate(instr: Instruction) = {
    // Constant to locotrac type
    instr.descendantsOfType[Constant]().foreach(c => c.replaceBy(Constant(c.value, map(c.constType),
      c.parserString)))

    instr.descendantsOfType[Declaration]().foreach {
      case d: VarDecl => d.declaredType = map(d.declaredType)
      case d: TypeDecl => //ignore
      case d: UnionDecl => d.union = map(d.union).asInstanceOf[Union]
      case d: StructDecl => d.struct = map(d.struct).asInstanceOf[Struct]
      case d: FunDef => //will be managed in fundecl
      case d: FunDecl => d.mtype = map(d.funType).asInstanceOf[FunctionType]
      case d: EnumDecl => //ignore
      case d: EnumElement => //ignore
      case _ => throw ASTException("Unsupported declaration")
    }

    instr
  }

  def migrate(instrs: List[Instruction]): Unit = instrs.foreach(migrate)

  def map(in: Type): Type = in match {
    case IntType(7, true) => TypeMgr.INT7
    case IntType(8, true) => TypeMgr.INT8
    case IntType(15, true) => TypeMgr.INT15
    case IntType(16, true) => TypeMgr.INT16
    case IntType(31, true) => TypeMgr.INT31
    case IntType(32, true) => TypeMgr.INT32
    case IntType(63, true) => TypeMgr.INT63
    case IntType(64, true) => TypeMgr.INT64
    case IntType(7, false) => TypeMgr.UINT7
    case IntType(8, false) => TypeMgr.UINT8
    case IntType(15, false) => TypeMgr.UINT15
    case IntType(16, false) => TypeMgr.UINT16
    case IntType(31, false) => TypeMgr.UINT31
    case IntType(32, false) => TypeMgr.UINT32
    case IntType(63, false) => TypeMgr.UINT63
    case IntType(64, false) => TypeMgr.UINT64
    case BoolType() => TypeMgr.BOOL
    case t: IntType => throw ASTException("Bad integer type")
    case CharType(true) => TypeMgr.INT8
    case CharType(false) => TypeMgr.UINT8
    case ArrayType(of, size) => ArrayType(map(of), size)
    case PointerType(of) => PointerType(map(of))
    case s: Struct => Struct(s.fields.map(part => StructPart(VarDecl(part.name, static = false, extern = false, map(part.declType),
      part.decl.init))), s.name)
    case u: Union => throw ASTException("Unsupported by hardener")
    case e: Enum => e
    case f: FunctionType => FunctionType(map(f.returnType),
      f.parameters.map(arg => VarDecl(arg.name, arg.static, arg.extern, map(arg.declaredType), arg.init)))
    case x => x //reference
  }
}


