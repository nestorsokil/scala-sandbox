package codekata

import scala.io.Source

object WeatherData {
  case class Day(number: Int, maxTemp: Int, minTemp: Int)

  def lines(resource: String): List[String] =
    Source.fromInputStream(getClass getResourceAsStream resource).getLines().toList.drop(2).dropRight(1)

  def linesToDays(iterator: List[String]): List[Day] = {
    iterator.map(line => {
      val parts = line.split("  ").filter(!_.isEmpty).map(_.replaceAll("\\*", ""))
      Day(parts(0).trim.toInt, parts(1).trim.toInt, parts(2).trim.toInt)
    })
  }

  def main(args: Array[String]): Unit = {
    val days = linesToDays(lines("/resources/weather.dat"))
    val leastSpread = days.sortBy(day => day.maxTemp - day.minTemp).head
    println(leastSpread)
  }
}
