package poly.ai.lab3

import scala.io.StdIn

/**
  * Created by nsokil on 11.03.2017.
  */
object PolishParser {
  private def parseDouble = java.lang.Double.parseDouble _
  private def parseBool = java.lang.Boolean.parseBoolean _

  def rpnAlgebraic(expr: String): Double = {
    expr.split(' ').toList.foldLeft(List[Double]())(
      (list, token) => (list, token) match {
        case (x :: y :: rest, "*") => (y * x) :: rest
        case (x :: y :: rest, "+") => (y + x) :: rest
        case (x :: y :: rest, "-") => (y - x) :: rest
        case (x :: y :: rest, "/") => (y / x) :: rest
        case (_, _) => parseDouble(token) :: list
      }).head
  }

  def rpnLogic(expr: String): Boolean = {
    expr.split(' ').toList.foldLeft(List[Boolean]())(
      (list, token) => (list, token) match {
        case (x :: y :: rest, "&") => (y && x) :: rest
        case (x :: y :: rest, "|") => (y | x) :: rest
        case (x ::      rest, "!") => !x :: rest
        case (_, _) => parseBool(token) :: list
      }).head
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      println("Введіть запис в оберненій польській нотації:")
      print(">> ")
      val input = StdIn.readLine()
      val output = if(input.contains("true") || input.contains("false")) rpnLogic(input) else rpnAlgebraic(input)
      println(output)
    }
  }
}
