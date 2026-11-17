package edu.luc.cs.laufer.cs371.expressions

import scala.collection.mutable
import util.Try
import Expr.*

/** A value produced at run time by a MiniJS program. */
enum Value derives CanEqual:
  case Num(value: Int)

object behaviors:

  import Value.*

  /** Mutable memory shared by all statements in one interpreter session. */
  type Store = mutable.Map[String, Value]

  /** The value of statements that do not otherwise produce a useful result. */
  val Void: Value = Num(0)

  def emptyStore(): Store = mutable.LinkedHashMap.empty[String, Value]

  private def numeric(store: Store)(e: Expr): Int =
    interpret(store)(e) match
      case Num(value) => value

  /**
   * Interprets an expression in the supplied store.
   *
   * Runtime errors deliberately propagate from here. `evaluate` catches them
   * once, at the top level, so evaluation stops at the first failure.
   */
  def interpret(store: Store)(e: Expr): Value = e match
    case Constant(c) => Num(c)
    case Variable(name) =>
      store.getOrElse(name, throw NoSuchFieldException(name))
    case UMinus(r)   => Num(-numeric(store)(r))
    case Plus(l, r)  => Num(numeric(store)(l) + numeric(store)(r))
    case Minus(l, r) => Num(numeric(store)(l) - numeric(store)(r))
    case Times(l, r) => Num(numeric(store)(l) * numeric(store)(r))
    case Div(l, r)   => Num(numeric(store)(l) / numeric(store)(r))
    case Mod(l, r)   => Num(numeric(store)(l) % numeric(store)(r))
    case Assign(Variable(name), right) =>
      val value = interpret(store)(right)
      store.update(name, value)
      Void
    case Assign(left, _) =>
      throw IllegalArgumentException(s"Cannot assign to $left")
    case Cond(guard, thenBranch, elseBranch) =>
      if numeric(store)(guard) != 0 then interpret(store)(thenBranch)
      else interpret(store)(elseBranch)
    case Loop(guard, body) =>
      while numeric(store)(guard) != 0 do
        interpret(store)(body): Unit
      Void
    case Block(statements) =>
      statements.foldLeft(Void):
        case (_, statement) => interpret(store)(statement)

  /** Evaluates a complete program and converts the first runtime error to a failure. */
  def evaluate(store: Store)(e: Expr): Try[Value] = Try(interpret(store)(e))

  /** Evaluates a side-effect-free program using a fresh, initially empty store. */
  def evaluate(e: Expr): Try[Value] = evaluate(emptyStore())(e)

  def size(e: Expr): Int = e match
    case Constant(_)       => 1
    case Variable(_)       => 1
    case UMinus(r)         => 1 + size(r)
    case Plus(l, r)        => 1 + size(l) + size(r)
    case Minus(l, r)       => 1 + size(l) + size(r)
    case Times(l, r)       => 1 + size(l) + size(r)
    case Div(l, r)         => 1 + size(l) + size(r)
    case Mod(l, r)         => 1 + size(l) + size(r)
    case Block(statements) => 1 + statements.map(size).sum
    case Cond(g, t, el)    => 1 + size(g) + size(t) + size(el)
    case Loop(g, b)        => 1 + size(g) + size(b)
    case Assign(l, r)      => 1 + size(l) + size(r)

  def height(e: Expr): Int = e match
    case Constant(_)       => 1
    case Variable(_)       => 1
    case UMinus(r)         => 1 + height(r)
    case Plus(l, r)        => 1 + math.max(height(l), height(r))
    case Minus(l, r)       => 1 + math.max(height(l), height(r))
    case Times(l, r)       => 1 + math.max(height(l), height(r))
    case Div(l, r)         => 1 + math.max(height(l), height(r))
    case Mod(l, r)         => 1 + math.max(height(l), height(r))
    case Block(statements) => 1 + (if statements.isEmpty then 0 else statements.map(height).max)
    case Cond(g, t, el)    => 1 + math.max(height(g), math.max(height(t), height(el)))
    case Loop(g, b)        => 1 + math.max(height(g), height(b))
    case Assign(l, r)      => 1 + math.max(height(l), height(r))

  import org.json4s.JsonAST.JValue
  import org.json4s.JsonDSL.*

  def toJson(e: Expr): JValue = e match
    case Constant(c)       => c
    case Variable(name)    => e.productPrefix -> name
    case UMinus(r)         => e.productPrefix -> toJson(r)
    case Plus(l, r)        => e.productPrefix -> List(toJson(l), toJson(r))
    case Minus(l, r)       => e.productPrefix -> List(toJson(l), toJson(r))
    case Times(l, r)       => e.productPrefix -> List(toJson(l), toJson(r))
    case Div(l, r)         => e.productPrefix -> List(toJson(l), toJson(r))
    case Mod(l, r)         => e.productPrefix -> List(toJson(l), toJson(r))
    case Block(statements) => e.productPrefix -> statements.map(toJson)
    case Cond(g, t, el)    => e.productPrefix -> List(toJson(g), toJson(t), toJson(el))
    case Loop(g, b)        => e.productPrefix -> List(toJson(g), toJson(b))
    case Assign(l, r)      => e.productPrefix -> List(toJson(l), toJson(r))

  def toInfix(e: Expr): String = e match
    case Constant(c)    => c.toString
    case Variable(name) => name
    case UMinus(r)      => s"-${toInfix(r)}"
    case Plus(l, r)     => s"(${toInfix(l)} + ${toInfix(r)})"
    case Minus(l, r)    => s"(${toInfix(l)} - ${toInfix(r)})"
    case Times(l, r)    => s"(${toInfix(l)} * ${toInfix(r)})"
    case Div(l, r)      => s"(${toInfix(l)} / ${toInfix(r)})"
    case Mod(l, r)      => s"(${toInfix(l)} % ${toInfix(r)})"
    case other          => toUnparsed(other)

  private def indent(depth: Int): String = "  " * depth

  def toUnparsed(e: Expr): String = unparse(e, 0)

  def unparse(e: Expr, depth: Int = 0): String = e match
    case Block(statements) =>
      val header = s"${indent(depth)}{\n"
      val body = statements.map(s => unparse(s, depth + 1)).mkString("\n")
      val bodyWithNl = if body.isEmpty then "" else body + "\n"
      val footer = s"${indent(depth)}}"
      s"$header$bodyWithNl$footer"

    case Cond(guard, thenBranch, elseBranch) =>
      val header = s"${indent(depth)}if (${toInfix(guard)}) {\n"
      val thenBody = thenBranch match
        case Block(ss) => ss.map(s => unparse(s, depth + 1)).mkString("\n")
        case other     => unparse(other, depth + 1)
      val thenWithNl = if thenBody.isEmpty then "" else thenBody + "\n"
      elseBranch match
        case c @ Cond(_, _, _) =>
          val elseCondStr = unparseCond(c, depth)
          s"$header$thenWithNl$elseCondStr"
        case Block(ss) =>
          val mid = s"${indent(depth)}} else {\n"
          val elseBody = ss.map(s => unparse(s, depth + 1)).mkString("\n")
          val elseWithNl = if elseBody.isEmpty then "" else elseBody + "\n"
          val footer = s"${indent(depth)}}"
          s"$header$thenWithNl$mid$elseWithNl$footer"
        case other =>
          val mid = s"${indent(depth)}} else {\n"
          val elseBody = unparse(other, depth + 1)
          val elseWithNl = if elseBody.isEmpty then "" else elseBody + "\n"
          val footer = s"${indent(depth)}}"
          s"$header$thenWithNl$mid$elseWithNl$footer"

    case Loop(guard, body) =>
      val header = s"${indent(depth)}while (${toInfix(guard)}) {\n"
      val bodyStr = body match
        case Block(ss) => ss.map(s => unparse(s, depth + 1)).mkString("\n")
        case other     => unparse(other, depth + 1)
      val bodyWithNl = if bodyStr.isEmpty then "" else bodyStr + "\n"
      val footer = s"${indent(depth)}}"
      s"$header$bodyWithNl$footer"

    case Assign(left, right) =>
      s"${indent(depth)}${toInfix(left)} = ${toInfix(right)};"

    case expr =>
      s"${indent(depth)}${toInfix(expr)};"

  private def unparseCond(cond: Cond, depth: Int): String =
    val header = s"${indent(depth)}} else if (${toInfix(cond.guard)}) {\n"
    val thenBody = cond.thenBranch match
      case Block(ss) => ss.map(s => unparse(s, depth + 1)).mkString("\n")
      case other     => unparse(other, depth + 1)
    val thenWithNl = if thenBody.isEmpty then "" else thenBody + "\n"
    cond.elseBranch match
      case c @ Cond(_, _, _) =>
        s"$header$thenWithNl${unparseCond(c, depth)}"
      case Block(ss) =>
        val mid = s"${indent(depth)}} else {\n"
        val elseBody = ss.map(s => unparse(s, depth + 1)).mkString("\n")
        val elseWithNl = if elseBody.isEmpty then "" else elseBody + "\n"
        val footer = s"${indent(depth)}}"
        s"$header$thenWithNl$mid$elseWithNl$footer"
      case other =>
        val mid = s"${indent(depth)}} else {\n"
        val elseBody = unparse(other, depth + 1)
        val elseWithNl = if elseBody.isEmpty then "" else elseBody + "\n"
        val footer = s"${indent(depth)}}"
        s"$header$thenWithNl$mid$elseWithNl$footer"

end behaviors
