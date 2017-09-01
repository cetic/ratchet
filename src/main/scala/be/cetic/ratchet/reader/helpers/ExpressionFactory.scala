package be.cetic.ratchet.reader.helpers

import be.cetic.ratchet.reader.ast._

/**
  *
  * @author : dimitri durieux <dimitri.durieux@cetic.be>
  */
object ExpressionFactory {

  def createPostfixExpression(pri: Expression, postOps: List[Any], ps: ParsingStack): Expression = {
    var current = pri

    for (postOp <- postOps) {
      current = postOp match {
        case x: List[Expression] => createCall(current, x, ps)
        case x: ArrayDef => createArrayAccess(current, x)
        case x@VirtualFieldAccess(a, true) => FieldAccess(current, a)
        case x@VirtualFieldAccess(a, false) => IndirectFieldAccess(current, a)
        case x@PostFixOp("++") => PostFixPlus(current)
        case x@PostFixOp("--") => PostFixMinus(current)
        case _ => throw new Exception("Not supported postfix operator : " + postOp)
      }
    }

    current
  }

  private def createCall(pri: Expression, args: List[Expression], ps: ParsingStack): Call = pri match {
    case x: Variable if (ps.get(x.name).isDefined) => ps.get(x.name).get match {
      case x: FunDecl => Call(x, args)
    }
    case x: Call if (ps.get(x.target.name).isDefined) => Call(x.target, args)
    case x: Variable if (!ps.get(x.name).isDefined) => throw new Exception("undeclared function " + x.name)
    case x: Constant => throw new Exception("postfix operator () not applicable on constant " + x.value)
    case x: Parenthesis => throw new Exception("parenthesis not yet implemented for postfix operator ()")
  }

  private def createArrayAccess(pri: Expression, args: ArrayDef): Expression =
    ArrayAccess(pri, args.e)
}
