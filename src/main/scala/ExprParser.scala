package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*

trait ExprParser[Result] extends JavaTokenParsers:

  /**
   * Enable missing typesafe equality for `~`.
   */
  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

  override def ident: Parser[String] =
    """(?!(?:if|else|while)\b)[a-zA-Z_][a-zA-Z0-9_]*""".r

  def number: Parser[String] =
    """\d+""".r

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Result] =
    term ~ rep(("+" | "-") ~ term) ^^ onExpr

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Result] =
    factor ~ rep(("*" | "/" | "%") ~ factor) ^^ onTerm

  /** factor ::= ident | number | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Result] =
    ident ^^ onIdent
    | number ^^ onNumber
    | "+" ~> factor ^^ onPlusFactor
    | "-" ~> factor ^^ onMinusFactor
    | "(" ~> expr <~ ")" ^^ onParenExpr

  /** statement ::= block | conditional | loop | assignment | expr ";" */
  def statement: Parser[Result] =
    block
    | conditional
    | loop
    | assignment
    | expr <~ ";" ^^ onExprStatement

  /** assignment ::= ident "=" expr ";" */
  def assignment: Parser[Result] =
    ident ~ ("=" ~> expr) <~ ";" ^^ onAssignment

  /** conditional ::= "if" "(" expr ")" block [ "else" ( conditional | block ) ] */
  def conditional: Parser[Result] =
    "if" ~> ("(" ~> expr <~ ")") ~ block ~ opt("else" ~> (conditional | block)) ^^ onConditional

  /** loop ::= "while" "(" expr ")" block */
  def loop: Parser[Result] =
    "while" ~> ("(" ~> expr <~ ")") ~ block ^^ onLoop

  /** block ::= "{" statement* "}" */
  def block: Parser[Result] =
    "{" ~> rep(statement) <~ "}" ^^ onBlock

  /** repl ::= statement* */
  def repl: Parser[Result] =
    rep(statement) ^^ onRepl

  def onExpr: Result ~ List[String ~ Result] => Result
  def onTerm: Result ~ List[String ~ Result] => Result
  def onIdent: String => Result
  def onNumber: String => Result
  def onPlusFactor: Result => Result
  def onMinusFactor: Result => Result
  def onParenExpr: Result => Result
  def onAssignment: String ~ Result => Result
  def onConditional: (Result ~ Result) ~ Option[Result] => Result
  def onLoop: Result ~ Result => Result
  def onBlock: List[Result] => Result
  def onExprStatement: Result => Result
  def onRepl: List[Result] => Result

end ExprParser
