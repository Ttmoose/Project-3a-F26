package edu.luc.cs.laufer.cs371.expressions

import Expr.*

object ASTBuilder extends ExprParser[Expr]:

  override def onExpr: Expr ~ List[String ~ Expr] => Expr =
    case first ~ rest =>
      rest.foldLeft(first):
        case (l, "+" ~ r) => Plus(l, r)
        case (l, "-" ~ r) => Minus(l, r)
        case (l, _)       => l

  override def onTerm: Expr ~ List[String ~ Expr] => Expr =
    case first ~ rest =>
      rest.foldLeft(first):
        case (l, "*" ~ r) => Times(l, r)
        case (l, "/" ~ r) => Div(l, r)
        case (l, "%" ~ r) => Mod(l, r)
        case (l, _)       => l

  override def onIdent: String => Expr = Variable.apply

  override def onNumber: String => Expr = Constant.apply compose (_.toInt)

  override def onPlusFactor: Expr => Expr = identity

  override def onMinusFactor: Expr => Expr = UMinus.apply

  override def onParenExpr: Expr => Expr = identity

  override def onAssignment: String ~ Expr => Expr =
    case name ~ e => Assign(Variable(name), e)

  override def onConditional: (Expr ~ Expr) ~ Option[Expr] => Expr =
    case guard ~ thenB ~ elseB => Cond(guard, thenB, elseB.getOrElse(Block(Nil)))

  override def onLoop: Expr ~ Expr => Expr =
    case guard ~ body => Loop(guard, body)

  override def onBlock: List[Expr] => Expr = ss => Block(ss)

  override def onExprStatement: Expr => Expr = identity

  override def onRepl: List[Expr] => Expr = ss => Block(ss)

end ASTBuilder
