package be.cetic.ratchet.reader.combinators

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.util.logging.{Level, Logger}

import be.cetic.ratchet.reader.ast.{Adr, Affect, BitAnd, BitNot, BitOr, BitXor, Block, Break, Call, CharType, Constant, Div, Equ, ExpressionInstr, FunDecl, FunDef, G, Ge, IfThenElse, IntType, L, Le, LogicalAnd, LogicalOr, Minus, Neg, Nequ, Nop, Not, Parenthesis, Plus, Ptr, Return, ShiftLeft, ShiftRight, Struct, StructDecl, StructPart, StructReference, Times, TranslationUnit, UPlus, Union, UnionReference, VarDecl, Variable, While, _}
import be.cetic.ratchet.reader.helpers.{VirtualFieldAccess, _}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator._


class CParser extends RegexParsers {

  var _logger = Logger.getLogger("CParser")

  def logger = _logger

  def logger_=(l: Logger) {
    _logger = l
  }

  def log(lvl: Level, msg: String) =
    if (logger.isLoggable(lvl)) logger.log(lvl, msg)

  def info(msg: String) = log(Level.INFO, msg)

  def severe(msg: String) = log(Level.SEVERE, msg)

  def warning(msg: String) = log(Level.WARNING, msg)

  var ps = new ParsingStack(logger)
  var unit = TranslationUnit(Nil)

  val ANONYMOUS_UNION = ""
  val ANONYMOUS_STRUCTURE = ""

  def init() = {
    ps = new ParsingStack(logger)
    unit = TranslationUnit(Nil)

    ps.push(unit)
  }

  def init(vars: List[Declaration]) = {
    ps = new ParsingStack(logger)
    unit = TranslationUnit(Nil)

    ps.push(unit)
    vars.foreach(ps.addDeclaration(_))
  }

  val unsupported = Array[Regex]()
  val reservedWords = Array(
    "auto", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register",
    "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef",
    "union", "unsigned", "void", "volatile", "while", "_Bool", "_Complex", "_Imaginary")

  def isReservedWord(input: String) = reservedWords.contains(input)

  def check(input: String) = {
    unsupported.foreach(pattern =>
      pattern findFirstIn input match {
        case Some(x) => throw ParserException("unsupported statement corresponding to pattern : " + pattern)
        case _ => ;
      }
    )
  }

  /*def link(tu:TranslationUnit):TranslationUnit={
    tu.descendantsOfType[Call]().filter(c => c.target match{
      case VarDecl(n,e,t:FunctionType,i) => true
      case _ => false
    }).foreach(c => c.target = tu.lookForLink(c))
    tu
  }*/

  private def _apply(input: String): TranslationUnit = {
    check(input)
    val preprocessed = Source.fromBytes(input.getBytes, "UTF-8").mkString
    return parseAll(translation_unit, preprocessed) match {
      case Success(result: TranslationUnit, _) => result
      case failure: NoSuccess => scala.sys.error("At position " + failure.next.pos.line + ":" + failure.next.pos.column + " : " + failure.msg)
      case _ => throw new ParserException("Unknown error during parsing");
    }
  }

  def apply(input: String): TranslationUnit = {
    init()
    _apply(input)
  }

  def apply(input: String, vars: List[Declaration]): TranslationUnit = {
    init(vars)
    _apply(input)
  }


  override type Elem = Char

  private val eoi = """\z""".r // end of input

  //def createGhostVar(name:String)=Variable(VarDecl("ghost",false,Ghost()))
  //def createGhostVar(name:String,ctype:PointerType)=Variable(VarDecl("ghost",false,ctype))

  def createUnaryOp(op: String, unaryExpr: Expression) = op match {
    case "&" => Adr(unaryExpr)
    case "*" => Ptr(unaryExpr)
    case "+" => UPlus(unaryExpr)
    case "-" => Neg(unaryExpr)
    case "~" => BitNot(unaryExpr)
    case "!" => Not(unaryExpr)
  }

  val translation_unit: Parser[Any] = rep(external_declaration) <~ eoi ^^ { x => TranslationUnit(x) }

  def external_declaration: Parser[Declaration] = declaration | function_definition

  val function_definition_header: Parser[FunDecl] =
    declaration_specifiers.? ~ declarator ~ rep(declaration) >> {
      case Some(decl_spec) ~ declarator ~ declarations => {
        try {
          val fundecl = DeclarationFactory.createDeclaration(
            decl_spec, Some(declarator), None).asInstanceOf[FunDecl]
          success(fundecl)
        }
        catch {
          case e: Throwable => {
            logger.log(Level.WARNING, "cannot properly create a function decl" + decl_spec + " "
              + declarator + " " + declarations, e)
            failure("bad function decl" + e)
          }
        }
      }
      case None ~ declarator ~ declarations => failure("specifiers must be defined"); null; //DeclarationFactory.createFunctionDecl(declarator,declarations)
    }

  val function_definition: Parser[Declaration] =
    function_definition_header ~ compound_statement >> { case x ~ y =>
      ps.get(x.name) match {
        case Some(ref: FunDecl) => {
          y.linkGotos
          ps.addDeclaration(x)
          success(FunDef(x, y, Some(ref)))
        }
        case Some(decl) => failure(decl.name + " is not a function")
        case _ => {
          y.linkGotos
          ps.addDeclaration(x)
          success(FunDef(x, y))
        }
      }
    }

  //TODO improvement : multiple declarators (not needed for locotrac project)
  val declaration: Parser[Declaration] =
    (declaration_specifiers ~ declarator.? ~ ("=" ~> initializer).? <~ ";") >> {
      case decl_spec ~ declarator ~ initializer => {
        try {
          val decl = DeclarationFactory.createDeclaration(decl_spec, declarator, initializer)
          decl match {
            case x: VarDecl => x.declaredType match {
              case s: Struct => ps.addDeclaration(StructDecl(s))
              case s: Enum => ps.addDeclaration(EnumDecl(s.name, s))
              case _ =>
            }
            case x@TypeDecl(name: String, y) =>
              if (ps.isDeclared(name)) {
                failure("Typedef already declared : " + name)
              }
              else {
                ps.addDeclaration(decl)
              }
            case _ =>
          }
          info("adding vardecl -- " + decl.name)
          ps.addDeclaration(decl)
          success(decl)
        }
        catch {
          case e: Exception => {
            logger.log(Level.SEVERE, "Can't properly create a declaration with " + decl_spec + " "
              + declarator + " init= " + initializer, e)
            failure("Can't properly create a declaration with " + decl_spec)
          }
        }
      }
    }


  def initializer: Parser[Expression] = assignment_expression // TODO adding 2 other

  def declaration_specifiers: Parser[List[TypePart]] =
    rep1(
      type_scope
        | type_specifier
        | type_qualifier
    )

  def type_scope: Parser[TypeScope] = ("extern" | "static") ^^ {
    TypeScope(_)
  }

  def type_specifier: Parser[TypeSpecifier] = (
    "typedef" ^^ { x => TypeDefSpecifier() }
      | "signed" ^^ { x => SignedSpecifier(true) }
      | "unsigned" ^^ { x => SignedSpecifier(false) }
      | ("void" | "char" | "short" | "int" | "long" | "float" | "double" | "_Bool") ^^ { x => {
      StringSpecifier(x)
    }
    }
      | type_id ^^ { x => x }
      | struct_or_union_specifier ^^ { x => x }
      | enum_specifier ^^ { x => x }
    )

  def struct_or_union_specifier: Parser[Type] = (
    ("struct" ~> IDENTIFIER.? ~ "{" ~ struct_declaration_list <~ "}") ^^ {
      case Some(id) ~ "{" ~ list => Struct(list, id.name)
      case None ~ "{" ~ list => Struct(list, ANONYMOUS_STRUCTURE)
    }
      | ("union" ~> IDENTIFIER.? ~ "{" ~ struct_declaration_list <~ "}") ^^ {
      case Some(id) ~ "{" ~ list => Union(list, id.name)
      case None ~ "{" ~ list => Union(list, ANONYMOUS_UNION)
    }
      | ("union" ~> type_id) >> {
      case id => id match {
        case s: UnionReference => success(s)
        case _ => failure(id + " has not been declared as a union")
      }
    }
      | ("struct" ~> type_id) >> {
      case id => id match {
        case s: StructReference => success(s)
        case _ => failure(id + " has not been declared as a struct")
      }
    }

    )


  def struct_declaration_list: Parser[List[StructPart]] = (struct_declaration +) ^^ {
    _.flatten
  }

  def struct_declaration: Parser[List[StructPart]] = (
    specifier_qualifier_list ~ struct_declarator_list <~ ";" >> {
      case specifier ~ declarator_list =>
        try {
          success(declarator_list.map(x => {
            val decl = DeclarationFactory.createDeclaration(specifier, Some(x), None)
            StructPart(decl.asInstanceOf[VarDecl])
          }))
        }
        catch {
          case _: Throwable => failure("Incorrect declaration in structure")
        }
    }
    )

  def specifier_qualifier_list: Parser[List[TypePart]] = (type_qualifier | type_specifier) +

  def struct_declarator_list: Parser[List[DeclaratorTemplate]] =
    (struct_declarator ^^ { x => List(x) }
      | struct_declarator ~ ("," ~> struct_declarator).* ^^ {
      case x ~ others => x :: others
    })

  def struct_declarator: Parser[DeclaratorTemplate] = (
    (declarator) ^^ { case d => d }
      | (declarator ~ (":" ~> constant_expression)) ^^ { case decl ~ constant => InitDeclarator(decl, Some(constant)) }
      | (":" ~ constant_expression) >> { x => failure("Not Supported") }
    )

  def enum_specifier: Parser[Type] = (
    ("enum" ~ "{" ~> enumerator_list <~ "}") ^^ {
      // declaration of enumeration of name anonymous
      case enumerators => {
        val enum = Enum(Nil, "")
        enum.elements = enumerators.map(f => EnumElement(f.name, f.value))
        enum.elements.foreach(elem => ps.addDeclaration(elem))
        enum
      }
    }
      | ("enum" ~> IDENTIFIER ~ "{" ~ enumerator_list <~ "}") ^^ {
      // declaration of enumeration of name identifier
      case identifier ~ sep ~ enumerators => {
        val enum = Enum(Nil, identifier.name)
        enum.elements = enumerators.map(f => EnumElement(f.name, f.value))
        enum.elements.foreach(elem => ps.addDeclaration(elem))
        enum
      }
    }
      | ("enum" ~> type_id) >> {
      // just an enumeration reference
      case id => id match {
        case s: EnumReference => success(s)
        case _ => failure(id + " has not been declared as a enum")
      }
    }
    )

  def enumerator_list: Parser[List[EnumElement]] = rep1sep(enumerator, ",")

  def enumerator: Parser[EnumElement] =
    IDENTIFIER ~ ("=" ~> constant_expression).? ^^ {
      case x ~ None => EnumElement(x.name, None)
      case x ~ Some(y) => y match {
        case Constant(value: Long, ctype, s) => EnumElement(x.name, Some(value.toInt))
        case _ => throw new Exception("not supported : " + y)
      }
    }

  def type_qualifier: Parser[TypeQualifier] = ("const" | "volatile") ^^ {
    TypeQualifier(_)
  }

  def declarator: Parser[SuffixedDeclarator] = (
    (pointer ~ direct_declarator) ^^ { case (x: PointerTemplate) ~ y =>
      SuffixedDeclarator(y.declarator, y.suffixes, Some(x))
    }
      | "(" ~> direct_declarator <~ ")" ^^ { x => x }
      | direct_declarator ^^ { x => x }
    )

  def direct_declarator: Parser[SuffixedDeclarator] = (
    (IDENTIFIER ~ declarator_suffix.* ^^ { case x ~ y => SuffixedDeclarator(x, y, None) })
      | (("(" ~> declarator <~ ")") ~ declarator_suffix.* ^^ { case x ~ y => x match {
      case SuffixedDeclarator(decl, List(), pointer) => SuffixedDeclarator(decl, y, pointer)
      case _ => SuffixedDeclarator(x, y, None)
    }
    })
    )

  def declarator_suffix: Parser[DeclaratorSuffix] = (
    ("[" ~> constant_expression <~ "]") ^^ {
      case x => ArrayDeclaration(Some(x))
    }
      | (("[" ~ "]") | ("[" ~ "*" ~ "]")) ^^ {
      case x ~ y => ArrayDeclaration(None)
    }
      | ("(" ~ "void" ~ ")") ^^ { x => FunctionDeclaration(Nil) }
      // |   ("(" ~ rep1sep(IDENTIFIER,',') ~ ")" ^^ {x => FunctionType()} ) //Call stmt <- wrong
      | ("(" ~ ")") ^^ { x => FunctionDeclaration(Nil) }
      | ("(" ~> parameter_declaration_list <~ ")") ^^ { x => FunctionDeclaration(x) }
    )

  // TODO manage type qualifier
  def pointer: Parser[PointerTemplate] = (
    ("*" ~ rep1(type_qualifier)) >> { x => failure("Const on pointer not supported") }
      | ("*" ~ rep1(type_qualifier) ~ pointer) >> { x => failure("Const on pointer not supported") }
      | ("*" ~> pointer) ^^ { x: PointerTemplate => PointerTemplate(x.depth + 1) }
      | ("*") ^^ { x => PointerTemplate(1) }
    )

  def parameter_declaration_list: Parser[List[VarDecl]] = rep1sep(parameter_declaration, ",")

  // TODO add support to multiple declarator
  def parameter_declaration: Parser[VarDecl] =
    declaration_specifiers ~ (declarator | abstract_declarator).? ^^ {
      case decl_spec ~ declarator => {
        /*
         The decl_spec is a list of string containing the type specifier and qualifier.

         The declarator variable is a expression as a nested suite of Ptr expression with a variable corresponding to
         the identifier

         For decl_spec and declarator variables, we need to compute the correct corresponding declaration.
         */
        val decl = DeclarationFactory.createDeclaration(decl_spec, declarator, None).asInstanceOf[VarDecl]
        ps.addDeclaration(decl)
        decl
      }
    }

  def type_name: Parser[Type] =
    specifier_qualifier_list ~ abstract_declarator.? ^^ {
      case decl_spec ~ declarator => DeclarationFactory.computeType(decl_spec, declarator)
    }

  def abstract_declarator: Parser[SuffixedDeclarator] = (
    pointer ~ direct_abstract_declarator ^^ { case (x: PointerTemplate) ~ y =>
      SuffixedDeclarator(y.declarator, y.suffixes, Some(x))
    }
      | pointer >> { x =>
      try {
        success(SuffixedDeclarator(Anonymous(), Nil, Some(x)))
      } catch {
        case e: Throwable => failure(e.getMessage)
      }
    }
      | direct_abstract_declarator ^^ { x => x }
    )

  def direct_abstract_declarator: Parser[SuffixedDeclarator] = (
    ("(" ~> abstract_declarator <~ ")") ~ abstract_declarator_suffix.* ^^ {
      case decl ~ suffixes => SuffixedDeclarator(decl, suffixes, None)
    }
      | abstract_declarator_suffix.+ ^^ {
      case suffixes => SuffixedDeclarator(null, suffixes, None)
    }
    )

  def abstract_declarator_suffix: Parser[DeclaratorSuffix] = (
    "[" ~ "]" ^^ { case x ~ y => ArrayDeclaration(None) }
      | "[" ~> constant_expression <~ "]" ^^ { case x => ArrayDeclaration(Some(x)) }
      | ("(" ~ ")") ^^ { x => FunctionDeclaration(Nil) }
      | "(" ~> parameter_declaration_list <~ ")" ^^ { x => FunctionDeclaration(x) }
    )


  // E x p r e s s i o n s
  //////////////////////////////////////////////////////////////////////////////////////////

  def argument_expression_list: Parser[List[Expression]] = repsep(assignment_expression, ",")

  def additive_expression: Parser[Expression] =
    multiplicative_expression ~ (("+" ^^^ true) ~ multiplicative_expression | ("-" ^^^ false) ~ multiplicative_expression).* ^^ { case a ~ b =>
      b.foldLeft(a)((a, add) => (if (add._1) Plus(a, add._2) else Minus(a, add._2)))
    }


  def multiplicative_expression: Parser[Expression] =
    (cast_expression ~ (("*" ^^^ 1) ~ cast_expression | ("/" ^^^ 2) ~ cast_expression | ("%" ^^^ 3) ~ cast_expression).*) ^^ {
      case a ~ b => b.foldLeft(a)((a, op) => op._1 match {
        case 1 => Times(a, op._2)
        case 2 => Div(a, op._2)
        case 3 => Mod(a, op._2)
      })
    }


  def cast_expression: Parser[Expression] = (
    ("(" ~> type_name <~ ")") ~ cast_expression ^^ {
      case t ~ e => Cast(e, t)
    }
      | unary_expression
    )


  def unary_expression: Parser[Expression] = (
    postfix_expression ^^ { case x => x }
      | "++" ~> unary_expression ^^ { case x => IncrementPrefix(x) }
      | "--" ~> unary_expression ^^ { case x => DecrementPrefix(x) }
      | unary_operator ~ cast_expression ^^ { case x ~ y => createUnaryOp(x, y) }
      | "sizeof" ~ "(" ~> type_name <~ ")" ^^ { case x => SizeOfT(x) }
      | "sizeof" ~> unary_expression ^^ { case x => SizeOfE(x) }
    )


  def postfix_expression: Parser[Expression] = ((
    primary_expression ~
      rep1(
        ("[" ~> expression <~ "]") ^^ { x => ArrayDef(x) }
          | "(" ~ ")" ^^ { case "(" ~ ")" => List[Expression]() }
          | "(" ~> argument_expression_list <~ ")" ^^ { x => x }
          | "." ~> IDENTIFIER ^^ { x => VirtualFieldAccess(x.name, directReference = true) }
          | "->" ~> IDENTIFIER ^^ { x => VirtualFieldAccess(x.name, directReference = false) }
          | "++" ^^ { x => PostFixOp("++") }
          | "--" ^^ { x => PostFixOp("--") }
      )
    ) ^^ { case x ~ y => ExpressionFactory.createPostfixExpression(x, y, ps)
  }
    | primary_expression ^^ { x => x }
    )

  def unary_operator: Parser[String] = (
    "&" // to manage "&&"
      | "*"
      | "+"
      | "-"
      | "~"
      | "!")

  def and_operator: Parser[String] = "&" ~ "&" ^^ { case x ~ y => "&&" }

  def bitand_operator: Parser[String] = "&" ~ not("&") ^^ { case x ~ y => "&" }

  def primary_expression: Parser[Expression] = (
    IDENTIFIER >> { w =>
      ps.get(w.name) match {
        //case Some(b) => success(Variable(b))
        case Some(b: VarDecl) => success(Variable(b))
        case Some(f: FunDecl) => success(Call(f, List()))
        case Some(f: FunDef) => success(Call(f.funDecl, List()))
        case x => failure(x + " cannot be handled") //success(createGhostVar(w.name))
      }
    }
      | constant ^^ { w => w }
      | enumelement ^^ { x => x }
      | ("(" ~> expression <~ ")") ^^ { x => Parenthesis(x) }
    //We save the parentheses because they wil be re-inserted into the generated output.
    )


  def constant: Parser[Constant] = (
    HEX_LITERAL ^^ { x => x }
      | OCTAL_LITERAL ^^ { x => x }
      | DECIMAL_LITERAL ^^ { x => x }
      | "\'" ~> LETTER <~ "\'" ^^ { x => Constant(x, CharType(), "'" + x + "'") } // TODO
    )

  def expression: Parser[Expression] =
    assignment_expression

  //assignment_expression ~ ("," ~ assignment_expression).* //sequencing is forbidden

  //TODO: check sémantique ici
  def constant_expression: Parser[Expression] = conditional_expression >> {
    x =>
      if (x.descendantsOfType[terminalASTNode]().filter(
        p => !(p.isInstanceOf[Constant] || p.isInstanceOf[EnumElementExpr])).isEmpty) success(x) else {
        failure("have no constant leaf")
      }
  }

  def assignment_expression: Parser[Expression] = (
    lvalue ~ ("=" ~> assignment_expression) ^^ { case a ~ b => Affect(a, b) }
      | lvalue ~ ("*=" ~> assignment_expression) ^^ { case a ~ b => CompoundTimes(a, b) }
      | lvalue ~ ("/=" ~> assignment_expression) ^^ { case a ~ b => CompoundDiv(a, b) }
      | lvalue ~ ("%=" ~> assignment_expression) ^^ { case a ~ b => CompoundMod(a, b) }
      | lvalue ~ ("+=" ~> assignment_expression) ^^ { case a ~ b => CompoundPlus(a, b) }
      | lvalue ~ ("-=" ~> assignment_expression) ^^ { case a ~ b => CompoundMinus(a, b) }
      | lvalue ~ ("<<=" ~> assignment_expression) ^^ { case a ~ b => CompoundShiftLeft(a, b) }
      | lvalue ~ (">>=" ~> assignment_expression) ^^ { case a ~ b => CompoundShiftRight(a, b) }
      | lvalue ~ ("&=" ~> assignment_expression) ^^ { case a ~ b => CompoundAnd(a, b) }
      | lvalue ~ ("^=" ~> assignment_expression) ^^ { case a ~ b => CompoundXor(a, b) }
      | lvalue ~ ("|=" ~> assignment_expression) ^^ { case a ~ b => CompoundOr(a, b) }
      | conditional_expression ^^ { w => w }
    )

  //TODO: must add semantic test here!!
  def lvalue: Parser[Expression] = unary_expression

  def conditional_expression: Parser[Expression] =
    logical_or_expression ~ ("?" ~ expression ~ ":" ~ conditional_expression).? ^^ {
      case a ~ Some(b) => throw new ParserException("Ternary operator not yet supported")
      case a ~ None => a
    }

  def logical_or_expression: Parser[Expression] = //TODO: check that foldLeft is the right method
    logical_and_expression ~ ("||" ~> logical_and_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, b) => LogicalOr(a, b)) }

  def logical_and_expression: Parser[Expression] =
    inclusive_or_expression ~ (and_operator ~> inclusive_or_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, b) => LogicalAnd(a, b)) }

  def inclusive_or_expression: Parser[Expression] =
    exclusive_or_expression ~ ("|" ~> exclusive_or_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, b) => BitOr(a, b)) }

  def exclusive_or_expression: Parser[Expression] =
    and_expression ~ ("^" ~> and_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, b) => BitXor(a, b)) }

  def and_expression: Parser[Expression] =
    equality_expression ~ (bitand_operator ~> equality_expression).* ^^ {
      case a ~ b => b.foldLeft(a)((a, b) => b match {
        case b: Adr => LogicalAnd(a, b.internalE) // <- to manage "&&" pattern
        case b => BitAnd(a, b)
      })
    }

  def equality_expression: Parser[Expression] =
    relational_expression ~ (("==" ^^^ true | "!=" ^^^ false) ~ relational_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, equ) => (if (equ._1) Equ(a, equ._2) else Nequ(a, equ._2))) }

  def relational_expression: Parser[Expression] =
    shift_expression ~ (("<=" ^^^ 3 | ">=" ^^^ 4 | "<" ^^^ 1 | ">" ^^^ 2) ~ shift_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, equ) => (equ._1 match {
      case 1 => L(a, equ._2)
      case 2 => G(a, equ._2)
      case 3 => Le(a, equ._2)
      case 4 => Ge(a, equ._2)
    }))
    }

  def shift_expression: Parser[Expression] =
    additive_expression ~ (("<<" ^^^ true | ">>" ^^^ false) ~ additive_expression).* ^^ { case a ~ b => b.foldLeft(a)((a, shift) => (if (shift._1) ShiftLeft(a, shift._2) else ShiftRight(a, shift._2))) }

  ////////////////////////////////////////////////////////////////////////////////////////

  def statement: Parser[Instruction] = (
    compound_statement
      | ";" ^^ { _ => Nop() }
      | expression <~ ";" ^^ { (e: Expression) => new ExpressionInstr(e) }
      | "if" ~ "(" ~> (expression <~ ")") ~ statement ~ ("else" ~> statement).? ^^ { case expression ~ statement ~ elsestatement => IfThenElse(expression, statement, elsestatement) }
      | "switch" ~ "(" ~> (expression <~ ")") ~ ("{" ~> switch_case <~ "}") ^^ { case e ~ cases => SwitchCase(e, cases) }
      | "while" ~ "(" ~> (expression <~ ")") ~ statement ^^ { case e ~ i => While(e, i) }
      | ("do" ~> statement) ~ ("while" ~ "(" ~> expression <~ ")" ~ ";") ^^ { case i ~ e => DoWhile(e, i) }
      | "for" ~ "(" ~> expression.? ~ (";" ~> expression.?) ~ (";" ~> expression.? <~ ")") ~ statement ^^ {
      case init ~ cond ~ loop ~ s =>
        For(
          init match {
            case Some(i) => Some(ExpressionInstr(i))
            case None => None
          },
          cond.getOrElse(Constant(1, IntType(), "1")),
          loop,
          s)
    }
      | "for" ~ "(" ~> declaration ~ expression ~ (";" ~> expression.? <~ ")") ~ statement ^^ {
      case init ~ cond ~ loop ~ s => For(Some(init), cond, loop, s)
    }
      | IDENTIFIER ~ (":" ~> statement) ^^ {
      case id ~ stmt => LabelledStmt(id.str, stmt)
    }
      | jump_statement ^^ { x => x }
    )

  def jump_statement: Parser[Instruction] = (
    "goto" ~> IDENTIFIER <~ ";" ^^ { x => GotoUnlinked(x.str) }
      | "continue" ~ ";" ^^^ (Continue())
      | "break" ~ ";" ^^^ (Break())
      | "return" ~ ";" ^^^ (Return())
      | "return" ~> expression <~ ";" ^^ { (e: Expression) => Return(Some(e)) }
    )

  def compound_statement: Parser[Block] = (
    compound_statement_start ~ compound_statement_content <~ "}" ^^ {
      case (start: Block) ~ (content: List[Instruction]) => {
        ps.pop();
        start.append(content: _*);
        start
      }
    }
    )

  def switch_case: Parser[CaseList] = (
    rep(simplecase) ~ default ^^ { case x ~ y => CaseList(x ::: (y :: Nil)) }
      | rep(simplecase) ^^ { case x => CaseList(x) }
    )

  def simplecase: Parser[Case] = (
    "case" ~> constant_expression ~ (":" ~> statement.*) ^^ {
      case e ~ i => Case(Some(e), Block(i))
    }
    )

  def default: Parser[Case] = "default" ~ ":" ~> statement ^^ {
    case s: Block => Case(None, s)
    case s: Instruction => Case(None, Block(List(s)))
  }

  //TODO: comment supporter le typedef qui renvoie à lui-même?
  def compound_statement_start: Parser[Block] = ("{" ^^ { _ => var b = Block(); ps.push(unit); b })

  def compound_statement_content: Parser[List[Instruction]] = rep(declaration | statement)

  //    def switch_case:Parser[(Option[Expression],List[Instruction])] =(
  //       "case" ~ constant_expression ~ ":" ~ statement.* ^^ {case "case" ~ constant_expression ~ ":" ~ statementlist => (Some(constant_expression),statementlist)}
  //     | "default" ~ ":" ~ statement.* ^^ {case "default" ~ ":" ~ statementlist => (None,statementlist)}
  //     )

  // regular expression part
  ///////////////////////////////////////////////////////////////////////////////////////

  override val whiteSpace = """([ \r\t\u000C\n]|(/*.*\*/)|(//[^\n|\r]*\r?\n)|restrict|const)+""".r

  val COMMENT: Regex = """/*.*\*/""".r
  val LINE_COMMENT: Regex = """//~(\n|\r).*\r?\n""".r

  val IDENTIFIER: Parser[Identifier] =
    """[$A-Za-z_][$A-Za-z_0-9]*""".r >> {
      x =>
        if (ps.isTypeName(x) || isReservedWord(x) || ps.isEnumElement(x))
          failure("type or reserved word : " + x) else success(Identifier(x))
    }

  val enumelement: Parser[EnumElementExpr] =
    """[$A-Za-z_][$A-Za-z_0-9]*""".r >> {
      x =>
        if (!ps.isEnumElement(x)) failure("not an enum element : " + x) else success(EnumElementExpr(
          ps.getEnumElement(x).get))
    }

  val type_id: Parser[NewTypeSpecifier] =
    """[$A-Za-z_][$A-Za-z_0-9]*""".r >> {
      x => if (ps.isTypeName(x)) success(ps.toReference(x)) else failure("not a type :" + x)
    }

  val LETTER: Regex = """[A-Za-z]""".r

  val IntegerTypeSuffix: Parser[(String, IntType)] = (
    """(?i)\Qull\E""".r ^^^ ("ULL", IntType(64, false))
      |
      """(?i)\Qul\E""".r ^^^ ("UL", IntType(64, false))
      |
      """(?i)\Qu\E""".r ^^^ ("U", IntType(32, false))
      |
      """(?i)\Ql\E""".r ^^^ ("L", IntType(64, true))
    )

  val HEX_LITERAL: Parser[Constant] = (
    """0(x|X)[0-9a-fA-F]+""".r ~ IntegerTypeSuffix ^^ { case digits ~ suffix => Constant(java.lang.Long.parseLong(digits.substring(2), 16), suffix._2, "" + digits + suffix._1) }
      |
      """0(x|X)[0-9a-fA-F]+""".r ^^ { case digits => Constant(java.lang.Long.parseLong(digits.substring(2), 16), IntType(32, true), "" + digits) }
    )

  val DECIMAL_LITERAL: Parser[Constant] = (
    """([1-9][0-9]*|0)""".r ~ IntegerTypeSuffix ^^ { case digits ~ suffix => Constant(digits.toLong, suffix._2, "" + digits + suffix._1) }
      |
      """([1-9][0-9]*|0)""".r ^^ { case digits => Constant(digits.toLong, IntType(32, true), "" + digits) }
    )

  val OCTAL_LITERAL: Parser[Constant] = (
    """0[0-7]+""".r ~ IntegerTypeSuffix ^^ { case digits ~ suffix => Constant(Integer.parseInt(digits, 8), suffix._2, digits + suffix._1) }
      |
      """0[0-7]+""".r ^^ { case digits => Constant(Integer.parseInt(digits, 8), IntType(32, true), digits) }
    )

}
