package codekata

import scala.io.Source

object DataMunging {
  def lines(resource: String): List[String] =
    Source.fromInputStream(getClass getResourceAsStream resource).getLines().toList.drop(1).dropRight(1)

  def parseLine(line: String): Array[String] =
    line.replaceAll("-", "").replaceAll("\\*", "").split(" ").filter(!_.isEmpty)

  def linesToCaseValues[T](fun: Array[String] => T)(lines: List[String]): List[T] =
    lines.map(parseLine).withFilter(_.nonEmpty).map(fun(_))
}

object WeatherData {
  case class Day(number: Int, maxTemp: Int, minTemp: Int)

  def linesToDays(iterator: List[String]): List[Day] =
    DataMunging.linesToCaseValues(parts => Day(parts(0).trim.toInt, parts(1).trim.toInt, parts(2).trim.toInt))(iterator)

  def main(args: Array[String]): Unit = {
    val days = linesToDays(DataMunging.lines("/resources/weather.dat"))
    val leastSpread = days.sortBy(day => day.maxTemp - day.minTemp).head
    println(leastSpread)
  }
}

object SoccerLeague{
  case class Team(name: String, scored: Int, missed: Int)

  def linesToTeams(iterator: List[String]): List[Team] =
    DataMunging.linesToCaseValues(parts => Team(parts(1).trim, parts(6).trim.toInt, parts(7).trim.toInt))(iterator)

  def main(args: Array[String]): Unit = {
    val teams = linesToTeams(DataMunging.lines("/resources/football.dat"))
    val best = teams.sortBy(team => math.abs(team.scored - team.missed)).head
    println(best)
  }
}