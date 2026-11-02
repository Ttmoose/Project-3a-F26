package edu.luc.cs.laufer.cs371.expressions

/** An initial algebra of arithmetic expressions and imperative statements. */
enum Expr derives CanEqual:
  case Constant(value: Int)
  case Variable(name: String)
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)
  case Block(statements: List[Expr])
  case Cond(guard: Expr, thenBranch: Expr, elseBranch: Expr)
  case Loop(guard: Expr, body: Expr)
  case Assign(left: Expr, right: Expr)

  override def toString: String = this match
    case Expr.Constant(c)   => s"Constant($c)"
    case Expr.Variable(n)   => s"Variable($n)"
    case Expr.UMinus(r)     => s"UMinus($r)"
    case Expr.Plus(l, r)    => s"Plus($l, $r)"
    case Expr.Minus(l, r)   => s"Minus($l, $r)"
    case Expr.Times(l, r)   => s"Times($l, $r)"
    case Expr.Div(l, r)     => s"Div($l, $r)"
    case Expr.Mod(l, r)     => s"Mod($l, $r)"
    case Expr.Block(ss)     => s"Block(${ss.mkString(", ")})"
    case Expr.Cond(g, t, e) => s"Cond($g, $t, $e)"
    case Expr.Loop(g, b)    => s"Loop($g, $b)"
    case Expr.Assign(l, r)  => s"Assign($l, $r)"

object Expr:
  def block(statements: Expr*): Block = Block(statements.toList)
