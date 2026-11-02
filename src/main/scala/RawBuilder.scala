package edu.luc.cs.laufer.cs371.expressions

object RawBuilder extends ExprParser[Any]:
  override def onExpr = identity
  override def onTerm = identity
  override def onIdent = identity
  override def onNumber = identity
  override def onPlusFactor = identity
  override def onMinusFactor = identity
  override def onParenExpr = identity
  override def onAssignment = identity
  override def onConditional = identity
  override def onLoop = identity
  override def onBlock = identity
  override def onExprStatement = identity
  override def onRepl = identity
end RawBuilder
