package edu.luc.cs.laufer.cs371.expressions

import java.io.ByteArrayOutputStream
import org.scalatest.funsuite.AnyFunSuite
import Expr.*
import Value.*
import behaviors.*

class TestEvaluation extends AnyFunSuite:

  private def parse(input: String): Expr =
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    assert(result.successful, s"Failed to parse '$input': $result")
    result.get

  test("constants and arithmetic evaluate to numeric values without changing memory") {
    val store = emptyStore()
    val program = parse("(3 + 4) * 2 - 5 % 3;")

    assert(evaluate(store)(program).get == Num(12))
    assert(store.isEmpty)
  }

  test("variables read values and unknown variables fail") {
    val store = emptyStore()
    store.update("x", Num(9))

    assert(evaluate(store)(parse("x + 1;")).get == Num(10))

    val failure = evaluate(store)(parse("missing;"))
    assert(failure.isFailure)
    assert(failure.failed.get.isInstanceOf[NoSuchFieldException])
    assert(failure.failed.get.getMessage == "missing")
  }

  test("assignment defines new variables, updates old variables, and returns void") {
    val store = emptyStore()

    assert(evaluate(store)(parse("x = 3;")).get == Num(0))
    assert(store("x") == Num(3))
    assert(evaluate(store)(parse("x = x + 4;")).get == Num(0))
    assert(store("x") == Num(7))
  }

  test("a failed right-hand side does not create its assignment target") {
    val store = emptyStore()
    val result = evaluate(store)(parse("x = unknown + 1;"))

    assert(result.isFailure)
    assert(!store.contains("x"))
  }

  test("blocks return their last value and empty blocks return void") {
    val store = emptyStore()

    assert(evaluate(store)(parse("{ x = 7; x; }")).get == Num(7))
    assert(store("x") == Num(7))
    assert(evaluate(store)(parse("{}")).get == Num(0))
  }

  test("blocks stop immediately at the first runtime failure") {
    val store = emptyStore()
    val result = evaluate(store)(parse("x = 1; unknown; x = 2;"))

    assert(result.isFailure)
    assert(store("x") == Num(1))
  }

  test("conditionals evaluate only the selected branch and return its value") {
    val truthyStore = emptyStore()
    val truthy = parse("if (-2) { x = 4; x; } else { unknown; }")
    assert(evaluate(truthyStore)(truthy).get == Num(4))
    assert(truthyStore("x") == Num(4))

    val falseyStore = emptyStore()
    val falsey = parse("if (0) { unknown; } else { y = 6; y + 1; }")
    assert(evaluate(falseyStore)(falsey).get == Num(7))
    assert(falseyStore("y") == Num(6))

    assert(evaluate(emptyStore())(parse("if (0) { unknown; }")).get == Num(0))
  }

  test("loops repeatedly evaluate their guard and body and return void") {
    val store = emptyStore()
    val program = parse(
      "x = 2; y = 3; r = 0; while (y) { r = r + x; y = y - 1; }"
    )

    assert(evaluate(store)(program).get == Num(0))
    assert(store("x") == Num(2))
    assert(store("y") == Num(0))
    assert(store("r") == Num(6))
  }

  test("a zero-guarded loop skips its body") {
    val store = emptyStore()
    val program = parse("x = 1; while (0) { x = 99; unknown; } x;")

    assert(evaluate(store)(program).get == Num(1))
    assert(store("x") == Num(1))
  }

  test("imperative constructs compose in a larger program") {
    val store = emptyStore()
    val factorial = parse(
      "n = 5; r = 1; while (n) { if (n) { r = r * n; } n = n - 1; } r;"
    )

    assert(evaluate(store)(factorial).get == Num(120))
    assert(store("n") == Num(0))
    assert(store("r") == Num(120))
  }

  test("arithmetic errors become failure values") {
    val result = evaluate(emptyStore())(parse("3 / 0;"))

    assert(result.isFailure)
    assert(result.failed.get.isInstanceOf[ArithmeticException])
  }

  test("the REPL reports results and retains memory between entries") {
    val store = emptyStore()
    val output = new ByteArrayOutputStream()

    Console.withOut(output):
      Calculator.process("x = 3;", store)
      Calculator.process("x + 4;", store)

    val text = output.toString
    assert(text.contains("It evaluates to Success(Num(0))"))
    assert(text.contains("It evaluates to Success(Num(7))"))
    assert(text.contains("Memory: Map(x -> Num(3))"))
    assert(store("x") == Num(3))
  }

  test("the REPL reports parse and runtime failures without losing its store") {
    val store = emptyStore()
    store.update("x", Num(3))
    val output = new ByteArrayOutputStream()

    Console.withOut(output):
      Calculator.process("not valid ;;", store)
      Calculator.process("unknown;", store)

    val text = output.toString
    assert(text.contains("could not be parsed"))
    assert(text.contains("It evaluates to Failure(java.lang.NoSuchFieldException: unknown)"))
    assert(store("x") == Num(3))
  }

end TestEvaluation
