package poly.ai.lab1

import scala.collection.mutable
import scala.io.StdIn

class REPL {
  private var u: U[String] = U(Nil)
  private var bindings: mutable.Map[String, O[String]] = mutable.Map()

  private val REGEX = "([a-zA-Z0-9]+)\\s?\\(\\+|\\-|\\*|\\\\|union){1}\\s?([a-zA-Z0-9]+)"

  def init() = {
    u = U(invite("U = ").split(" ").toList)
    bindings = mutable.Map()
    progress()
  }

  def progress(): Unit = {
    val input = invite()
    val trimmed = input.trim
    if(trimmed == "U") result(u.o().toString())
    else if(bindings.contains(trimmed)) result(bindings(trimmed).toString)
    else if(input.contains("=")) bind(input)
    else if(input == "reset") init()
    if (input == "quit" || input == "exit" || input == "q") exit()
    else progress()
  }

  private def bind(input: String) = {
    val parts = input.split("=")
    bindings(parts(0).trim) = u << parts(1).split(" ").map(_.trim)
  }
  private def result(str: String) = println(str)
  private def invite(): String = invite("")
  private def invite(msg: String): String = {
    print(s">> $msg"); StdIn.readLine()
  }

  private def exit() = result("Exiting.")
}

object Lab1 {
  def main(args: Array[String]): Unit = {
    val repl = new REPL()
    repl.init()
  }
}
