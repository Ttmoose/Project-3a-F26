package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import Expr.*
import behaviors.*

class TestMiniJS extends AnyFunSuite:

  def parse(input: String): Expr =
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    assert(result.successful, s"Failed to parse '$input': $result")
    result.get

  def assertRejects(input: String): Unit =
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    val rejected = !result.successful
    assert(rejected, s"Expected failure but succeeded on '$input': $result"): Unit

  test("Assignment statements") {
    val ast1 = parse("x = 5;")
    assert(ast1 == Block(List(Assign(Variable("x"), Constant(5)))))

    val ast2 = parse("x = 5 ; y = 7;")
    assert(ast2 == Block(List(
      Assign(Variable("x"), Constant(5)),
      Assign(Variable("y"), Constant(7))
    )))

    val ast3 = parse("x = 3 + 4 - 5;")
    assert(ast3 == Block(List(
      Assign(Variable("x"), Minus(Plus(Constant(3), Constant(4)), Constant(5)))
    )))

    val ast4 = parse("x = ((1 + y2) - (3 * y4)) / 5;")
    assert(ast4 == Block(List(
      Assign(
        Variable("x"),
        Div(
          Minus(
            Plus(Constant(1), Variable("y2")),
            Times(Constant(3), Variable("y4"))
          ),
          Constant(5)
        )
      )
    )))
  }

  test("Expression statements") {
    val ast = parse("((1 + y2) - (3 * y4)) / 5;")
    assert(ast == Block(List(
      Div(
        Minus(
          Plus(Constant(1), Variable("y2")),
          Times(Constant(3), Variable("y4"))
        ),
        Constant(5)
      )
    )))

    val astMod = parse("x % 2;")
    assert(astMod == Block(List(Mod(Variable("x"), Constant(2)))))
  }

  test("Conditional statements") {
    // if without else
    val ast1 = parse("if (1) { x = 2; }")
    assert(ast1 == Block(List(
      Cond(Constant(1), Block(List(Assign(Variable("x"), Constant(2)))), Block(Nil))
    )))

    // if with else
    val ast2 = parse("if (1) { x = 2; } else { x = 3; }")
    assert(ast2 == Block(List(
      Cond(
        Constant(1),
        Block(List(Assign(Variable("x"), Constant(2)))),
        Block(List(Assign(Variable("x"), Constant(3))))
      )
    )))

    // if with compound block
    val ast3 = parse("if (4) { r = r + x; y = y + 1; }")
    assert(ast3 == Block(List(
      Cond(
        Constant(4),
        Block(List(
          Assign(Variable("r"), Plus(Variable("r"), Variable("x"))),
          Assign(Variable("y"), Plus(Variable("y"), Constant(1)))
        )),
        Block(Nil)
      )
    )))
  }

  test("Successive else if (Extra Credit)") {
    val ast = parse("if (x) { y = 1; } else if (z) { y = 2; } else { y = 3; }")
    assert(ast == Block(List(
      Cond(
        Variable("x"),
        Block(List(Assign(Variable("y"), Constant(1)))),
        Cond(
          Variable("z"),
          Block(List(Assign(Variable("y"), Constant(2)))),
          Block(List(Assign(Variable("y"), Constant(3))))
        )
      )
    )))
  }

  test("Loop statements") {
    val ast1 = parse("while (y) { r = r + x; y = y - 1; }")
    assert(ast1 == Block(List(
      Loop(
        Variable("y"),
        Block(List(
          Assign(Variable("r"), Plus(Variable("r"), Variable("x"))),
          Assign(Variable("y"), Minus(Variable("y"), Constant(1)))
        ))
      )
    )))

    val ast2 = parse("while (y) { r = r + x ; y = y - 1 ;}")
    assert(ast2 == Block(List(
      Loop(
        Variable("y"),
        Block(List(
          Assign(Variable("r"), Plus(Variable("r"), Variable("x"))),
          Assign(Variable("y"), Minus(Variable("y"), Constant(1)))
        ))
      )
    )))
  }

  test("Block statements") {
    val ast1 = parse("{ r = r + x; y = y + 1 ; }")
    assert(ast1 == Block(List(
      Block(List(
        Assign(Variable("r"), Plus(Variable("r"), Variable("x"))),
        Assign(Variable("y"), Plus(Variable("y"), Constant(1)))
      ))
    )))

    val ast2 = parse("{}")
    assert(ast2 == Block(List(Block(Nil))))
  }

  test("Instructor complex example") {
    val input = "if(-3+4+5*6){while(0){x=3;y=5;{xy=88;}}}"
    val ast = parse(input)
    val expected = Block(List(
      Cond(
        Plus(Plus(UMinus(Constant(3)), Constant(4)), Times(Constant(5), Constant(6))),
        Block(List(
          Loop(
            Constant(0),
            Block(List(
              Assign(Variable("x"), Constant(3)),
              Assign(Variable("y"), Constant(5)),
              Block(List(
                Assign(Variable("xy"), Constant(88))
              ))
            ))
          )
        )),
        Block(Nil)
      )
    ))
    assert(ast == expected)

    val expectedUnparsed =
      "{\n" +
      "  if (((-3 + 4) + (5 * 6))) {\n" +
      "    while (0) {\n" +
      "      x = 3;\n" +
      "      y = 5;\n" +
      "      {\n" +
      "        xy = 88;\n" +
      "      }\n" +
      "    }\n" +
      "  } else {\n" +
      "  }\n" +
      "}"
    assert(toUnparsed(ast) == expectedUnparsed)
  }

  test("Invalid syntax rejection") {
    // Missing semicolon
    assertRejects("x = 5")
    // Invalid assignment target (number)
    assertRejects("5 = x;")
    // Unclosed block
    assertRejects("if (1) { x = 2;")
    // Missing parens around if condition
    assertRejects("if 1 { x = 2; }")
    // Unclosed paren around while condition
    assertRejects("while (y { x = 1; }")
    // Empty condition in while
    assertRejects("while () { x = 1; }")
    // Missing loop body block
    assertRejects("while (x);")
    // Keyword as identifier
    assertRejects("while = 5;")
    assertRejects("if = 5;")
    assertRejects("else = 5;")
    // Consecutive invalid binary operators
    assertRejects("x = 3 * * 4;")
    assertRejects("x = 3 / / 4;")
  }

  test("Unparser round-trip property") {
    val programs = Seq(
      "x = 5;",
      "x = 5; y = 7;",
      "((1 + y2) - (3 * y4)) / 5;",
      "x = ((1 + y2) - (3 * y4)) / 5;",
      "if (1) { x = 2; }",
      "if (1) { x = 2; } else { x = 3; }",
      "{ r = r + x; y = y + 1; }",
      "if (4) { r = r + x; y = y + 1; }",
      "while (y) { r = r + x; y = y - 1; }",
      "x = 3 + 4 - 5;",
      "if(-3+4+5*6){while(0){x=3;y=5;{xy=88;}}}",
      "if (x) { y = 1; } else if (z) { y = 2; } else { y = 3; }"
    )

    for (p <- programs) {
      val ast1 = parse(p)
      val unparsed = toUnparsed(ast1)
      val ast2 = parse(unparsed)
      assert(ast2 == Block(List(ast1)), s"Round-trip failed for:\nOriginal: $p\nUnparsed:\n$unparsed\nAst1: $ast1\nAst2: $ast2")
    }
  }

  test("toJson structure") {
    import org.json4s.native.JsonMethods.{pretty, render}
    val ast = parse("x = 5;")
    val json = toJson(ast)
    val rendered = pretty(render(json))
    assert(rendered.contains("\"Block\""))
    assert(rendered.contains("\"Assign\""))
    assert(rendered.contains("\"Variable\" : \"x\"") || rendered.contains("\"Variable\":\"x\"") || rendered.contains("\"Variable\": \"x\""))
  }

  test("size and height") {
    val e1 = Constant(5)
    assert(size(e1) == 1)
    assert(height(e1) == 1)

    val v1 = Variable("x")
    assert(size(v1) == 1)
    assert(height(v1) == 1)

    val assign = Assign(v1, e1)
    assert(size(assign) == 3)
    assert(height(assign) == 2)

    val block = Block(List(assign))
    assert(size(block) == 4)
    assert(height(block) == 3)

    val loop = Loop(v1, block)
    assert(size(loop) == 6)
    assert(height(loop) == 4)

    val cond = Cond(e1, block, Block(Nil))
    assert(size(cond) == 7)
    assert(height(cond) == 4)

    val emptyBlock = Block(Nil)
    assert(size(emptyBlock) == 1)
    assert(height(emptyBlock) == 1)
  }

  test("evaluate throws UnsupportedOperationException for statements") {
    val s = Assign(Variable("x"), Constant(5))
    assert(evaluate(s).isFailure)
  }

  test("RawBuilder parses statements and expressions") {
    val raw1 = RawBuilder.parseAll(RawBuilder.expr, "1 + 2")
    assert(raw1.successful)

    val raw2 = RawBuilder.parseAll(RawBuilder.statement, "x = 5;")
    assert(raw2.successful)

    val raw3 = RawBuilder.parseAll(RawBuilder.repl, "x = 5; y = 7;")
    assert(raw3.successful)
  }

  test("Calculator.process handles valid and invalid inputs") {
    Calculator.process("x = 5;")
    Calculator.process("invalid syntax ;;")
    Calculator.processExpr("y = 10;")
  }

  test("Expr.block factory method") {
    val b = Expr.block(Constant(1), Constant(2))
    assert(b == Block(List(Constant(1), Constant(2))))
  }

end TestMiniJS
