package edu.luc.cs.laufer.cs371.expressions

import scala.collection.mutable.ListBuffer
import org.jline.reader.{LineReader, LineReaderBuilder, EndOfFileException, UserInterruptException}
import org.jline.terminal.TerminalBuilder
import org.json4s.native.JsonMethods.{pretty, render}
import behaviors.*

object Calculator:

  /** Memory persists across successive entries in this REPL session. */
  val memory: Store = emptyStore()

  private def showMemory(store: Store): String =
    store.iterator.map((name, value) => s"$name -> $value").mkString("Map(", ", ", ")")

  def process(input: String, store: Store): Unit =
    println("You entered: " + input)
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    if !result.successful then
      println("These statements could not be parsed: " + result)
    else
      val ast = result.get
      println("The parsed statements are:")
      println(ast)
      println("The corresponding JSON structure is:")
      println(pretty(render(toJson(ast))))
      println("The unparsed statements are:")
      println(toUnparsed(ast))
      println("It evaluates to " + evaluate(store)(ast))
    println("Memory: " + showMemory(store))

  def process(input: String): Unit = process(input, memory)

  def processExpr(input: String): Unit = process(input)

  def runWithJLine(): Unit =
    val terminal = TerminalBuilder.builder().system(true).build()
    val reader = LineReaderBuilder.builder().terminal(terminal).appName("minijs").build()
    val buffer = ListBuffer[String]()
    var running = true

    while running do
      try
        val prompt = if buffer.isEmpty then "minijs> " else "  ...> "
        val line = reader.readLine(prompt)
        if line == null then
          if buffer.nonEmpty then
            process(buffer.mkString("\n"))
            buffer.clear()
          running = false
        else if line.trim.isEmpty then
          if buffer.nonEmpty then
            process(buffer.mkString("\n"))
            buffer.clear()
        else
          buffer += line
      catch
        case _: EndOfFileException =>
          if buffer.nonEmpty then
            process(buffer.mkString("\n"))
            buffer.clear()
          running = false
        case _: UserInterruptException =>
          buffer.clear()

    terminal.close()

  def runWithStdin(): Unit =
    val buffer = ListBuffer[String]()
    print("minijs> ")
    for line <- scala.io.Source.stdin.getLines() do
      if line.trim.isEmpty then
        if buffer.nonEmpty then
          process(buffer.mkString("\n"))
          buffer.clear()
        print("minijs> ")
      else
        buffer += line
        print("  ...> ")
    if buffer.nonEmpty then
      process(buffer.mkString("\n"))

  def main(args: Array[String]): Unit =
    println("Memory: " + showMemory(memory))
    if args.length > 0 then
      process(args.mkString(" "))
    else
      try
        runWithJLine()
      catch
        case _: Throwable =>
          runWithStdin()

end Calculator
