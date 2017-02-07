package poly.ai.lab1

import scala.collection.mutable
import scala.io.StdIn
import scala.util.matching._

class REPL {
  private var u: U[String] = U(Nil)
  private var bindings: mutable.Map[String, O[String]] = mutable.Map()

  val DESCRIPTION = "This is a CLI application that allows you to perform set operations.\n" +
    "First of all you have to create an universal set (U) which is a superset of any other set created.\n" +
    "Type in 'help' to the console to see the available commands."
  val HELP = "desc - description\nreset - delete all data, restart seesion;\n[SET_NAME] = [SPACE_SEPARATED_VALUES] - " +
    "create new set\n\nSet operations:\n" +
    "+ | or | OR | union - create a union\n" +
    "* | and | AND | & | && | conj - create an intersection\n" +
    "\\ | - | disj - create a complement\n" +
    "! - create a negation"
  val EXIT = "Exiting."

  private val NEGATE_REGEX = "\\!([a-zA-Z0-9]+)".r
  private val ASSIGNMENT_REGEX = "([a-zA-Z0-9]+)\\s?\\=\\s?([a-zA-Z0-9]+)\\s?(\\+|\\-|\\*|&|&&|\\\\|union|disj|conj|OR|or|AND|and){1}\\s?([a-zA-Z0-9]+)".r
  private val ASSIGN_NEGATION_REGEX = "([a-zA-Z0-9]+)\\s?\\=\\s?\\!([a-zA-Z0-9]+)".r
  private val SIMPLE_OP_REGEX = "([a-zA-Z0-9]+)\\s?(\\+|\\-|\\*|&|&&|\\\\|union|disj|conj|OR|or|AND|and){1}\\s?([a-zA-Z0-9]+)".r

  def init() = {
    u = U(invite("Create a universal set U = ").split(" ").toList)
    bindings = mutable.Map()
    progress()
  }

  def progress(): Unit = {
    val input = invite().trim
    if(input == "U") result(u.o().toString())
    else if(bindings.contains(input)) result(bindings(input).toString)
    else if(input == "reset") init()
    else if(matches(NEGATE_REGEX, input)){
      val NEGATE_REGEX(operand) = input
      result( negate(operand).toString )
    }
    else if(matches(ASSIGNMENT_REGEX, input)){
      val ASSIGNMENT_REGEX(assign, left, op, right) = input
      val res = operate(left, op, right)
      bind(assign, res)
      result(res.toString)
    }
    else if(matches(ASSIGN_NEGATION_REGEX, input)){
      val ASSIGN_NEGATION_REGEX(left, right) = input
      val res = negate(right)
      bind(left, res)
      result(res.toString)
    }
    else if(input.contains("=")) bind(input)
    else if(matches(SIMPLE_OP_REGEX, input)){
      val SIMPLE_OP_REGEX(left, op, right) = input
      result( operate(left, op, right).toString )
    }
    else if(input == "help") help()
    else if(input == "desc") desc()
    if (input == "quit" || input == "exit" || input == "q") exit()
    else progress()
  }

  def matches(regex: Regex, line: String) = regex.pattern.matcher(line).matches()
  def operate(left: String, op: String, right: String) =
    op match {
      case "+"  | "union" | "or" | "OR" => union(left, right)
      case "\\" | "disj" | "-" => disj(left, right)
      case "*"  | "&" | "&&" | "conj" | "and" | "AND"  => conj(left, right)
    }

  private def union(left: String, right: String) =  bindings(left) ∪ bindings(right)
  private def disj(left: String, right: String)  =  bindings(left) \ bindings(right)
  private def conj(left: String, right: String)  =  bindings(left) ∩ bindings(right)
  private def negate(operand: String)            = !bindings(operand)

  private def bind(key: String, value: O[String]) = {
    bindings(key) = value
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

  private def help() = result(HELP)
  private def desc() = result(DESCRIPTION)
  private def exit() = result(EXIT)
}

object Lab1 {
  def main(args: Array[String]): Unit = {
    val repl = new REPL()
    repl.init()
  }
}
