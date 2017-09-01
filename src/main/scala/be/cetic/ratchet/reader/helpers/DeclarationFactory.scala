package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.ast.{ArrayType, FunDecl, FunctionType, IntType, PointerType, TypeDecl, VarDecl, Void, _}


case class TypeMapper(signed: Boolean,
                      primitive: List[String],
                      declType: Option[Type],
                      template: Option[DeclaratorTemplate]) {

  assert(declType != null, "Declared type must be defined as None or Some(x)")
  assert(template != null, "Template must be defined as None or Some(x)")

  private val qualifierSize = List("short", "long")
  private val _size = primitive.filter(qualifierSize.contains(_))

  def size = _size

  private def strictPrimitive = primitive.filterNot(qualifierSize.contains(_))

  def generateType: Type = applySuffixes(template, handleDeclType)

  private def handleDeclType: Type = {
    var multiplier = 1D

    if (size.size > 1) size match {
      case List("long", "long") => return IntType(64, this.signed)
      case _ => throw ParserException("multiple size declarator (short or long) ")
    }
    else if (size.size == 1)
      multiplier = size.head match {
        case "short" => 0.5
        case "long" => 1
      }

    declType match {
      case Some(x) => x
      case None => this.strictPrimitive.headOption match {
        case Some("void") => Void()
        case Some("int") => IntType((32 * multiplier).toInt, this.signed)
        case Some("char") => CharType(this.signed)
        case Some("_Bool") => BoolType()
        case Some(x: String) => throw ParserException("Type" + x + " not supported")
        case None if !size.isEmpty => IntType((32 * multiplier).toInt, this.signed)
      }
    }
  }

  private def applySuffixes(template: Option[DeclaratorTemplate], basicType: Type): Type = template match {
    case None => basicType
    case Some(t: Anonymous) => basicType
    case Some(t: Identifier) => basicType
    case Some(t: SuffixedDeclarator) => {
      t.pointer match {
        case None => handleSuffixes(t, basicType)
        case Some(x) => TypeUtils.generateNestedPointer(x.depth, handleSuffixes(t, basicType))
      }
    }
  }

  private def handleSuffixes(template: SuffixedDeclarator, basicType: Type): Type =
    template.suffixes.foldRight(applySuffixes(Some(template.declarator), basicType))((suf, x) => suf match {
      case s: ArrayDeclaration => ArrayType(x, s.intSize)
      case s: FunctionDeclaration => FunctionType(x, s.args)
    })

}

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
object DeclarationFactory {

  val primitive = List("void", "char", "short", "int", "long", "float", "double")
  val primitiveType = List("void", "char", "int", "float", "double")
  val qualifierScope = List("volatile", "extern")

  def createDeclaration(declSpec: List[TypePart],
                        typeTemplate: Option[DeclaratorTemplate],
                        initializer: Option[Expression]
                       ): Declaration = typeTemplate match {
    case None => completeDeclaration(declSpec)
    case Some(x) => completeDeclaration(declSpec, x, initializer)
  }


  private def completeDeclaration(declSpec: List[TypePart],
                                  typeTemplate: DeclaratorTemplate,
                                  initializer: Option[Expression]
                                 ): Declaration = {

    if (declSpec.filter(p => p.isInstanceOf[TypeDefSpecifier]).size > 0)
      DeclarationFactory.computeTypeDef(declSpec, typeTemplate)
    else {
      val isExtern = extern(declSpec)
      val isStatic = static(declSpec)
      computeType(declSpec, Some(typeTemplate)) match {
        case x: Struct if (typeTemplate.name == null) => StructDecl(x)
        case x: Union if (typeTemplate.name == null) => UnionDecl(x)
        case x: Enum if (typeTemplate.name == null) => EnumDecl(x.name, x)
        case x: FunctionType => FunDecl(typeTemplate.name, x, isExtern)
        case x: PointerType => x.baseType match {
          case y: FunctionType => FunDecl(typeTemplate.name, x, isExtern)
          case y => VarDecl(typeTemplate.name, isStatic, isExtern, x, initializer)
        }
        case x => VarDecl(typeTemplate.name, isStatic, isExtern, x, initializer)
      }
    }
  }


  private def completeDeclaration(declSpec: List[TypePart]): Declaration = {
    //completeDeclaration(declSpec:List[TypePart],null,None)

    if (declSpec.filter(p => p.isInstanceOf[TypeDefSpecifier]).size > 0)
      throw ParserException("not here for " + declSpec.mkString(", "))
    else {
      val isExtern = extern(declSpec)
      val isStatic = static(declSpec)
      computeType(declSpec, None) match {
        case x: Struct => StructDecl(x)
        case x: Union => UnionDecl(x)
        case x: Enum => EnumDecl(x.name, x)
        case x => VarDecl("", isStatic, isExtern, x, None)
      }
    }
  }


  def computeType(declSpec: List[TypePart],
                  typeTemplate: Option[DeclaratorTemplate]
                 ): Type = {
    parse(declSpec, typeTemplate).generateType
  }

  private def computeTypeDef(declSpec: List[TypePart], template: DeclaratorTemplate): Declaration = {
    assert(template.name != null)
    assert(declSpec.size > 0)
    TypeDecl(template.name, computeType(declSpec, Some(template)))
  }

  private def extern(declSpec: List[TypePart]): Boolean = {
    declSpec.find(_.isInstanceOf[TypeScope]) match {
      case Some(x) => x.asInstanceOf[TypeScope].name.equals("extern")
      case None => false
    }
  }

  private def static(declSpec: List[TypePart]): Boolean = {
    declSpec.find(_.isInstanceOf[TypeScope]) match {
      case Some(x) => x.asInstanceOf[TypeScope].name.equals("static")
      case None => false
    }
  }

  private def parse(declSpec: List[TypePart], template: Option[DeclaratorTemplate]) = {

    var primitive = List[StringSpecifier]();

    var qualifier = List[TypeQualifier]();
    var signed: SignedSpecifier = null;
    var declType: Type = null;

    declSpec.foreach(x => x match {
      case s: SignedSpecifier => if (signed == null) signed = s else throw ParserException("Only one signed or unsigned specifier authorized")
      case s: StringSpecifier => primitive = s :: primitive
      case s: Struct => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: Union => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: Enum => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: TypeDefReference => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: StructReference => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: UnionReference => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: EnumReference => if (declType == null) declType = s else throw ParserException("Only one type authorized")
      case s: TypeQualifier => qualifier = s :: qualifier
      case s: TypeDefSpecifier => //ignore
      case s: TypeScope => //ignore
      case _ => throw ParserException("Not supported : " + x)
    })

    if (signed == null) signed = SignedSpecifier(true)

    if (declType != null && primitive.nonEmpty) {
      throw ParserException("Only one type authorized")
    }
    else {
      if (declType != null)
        TypeMapper(
          signed.signed,
          primitive.map(x => x.part),
          Some(declType),
          template)
      else
        TypeMapper(
          signed.signed,
          primitive.map(x => x.part),
          None,
          template)
    }
  }

}