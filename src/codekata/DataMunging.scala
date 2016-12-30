package codekata

import scala.io.Source

case class Day(number: Int, maxTemp: Int, minTemp: Int)
case class Team(name: String, scored: Int, missed: Int)

abstract class DataMunging {
  def lines(resource: String): List[String] =
    Source.fromInputStream(getClass getResourceAsStream resource).getLines().toList.drop(1).dropRight(1)

  def parseLine(line: String): Array[String] =
    line.replaceAll("-", "").replaceAll("\\*", "").split(" ").filter(!_.isEmpty)

  def linesToCaseValues[T](fun: Array[String] => T)(lines: List[String]): List[T] =
    lines.map(parseLine).withFilter(_.nonEmpty).map(fun(_))
}

object WeatherData extends DataMunging{
  def linesToDays(path: String): List[Day] =
    linesToCaseValues(parts => Day(parts(0).trim.toInt, parts(1).trim.toInt, parts(2).trim.toInt))(lines(path))
}

object SoccerLeague extends DataMunging{
  def linesToTeams(path: String): List[Team] =
    linesToCaseValues(parts => Team(parts(1).trim, parts(6).trim.toInt, parts(7).trim.toInt))(lines(path))
}

object Main{
  def main(args: Array[String]): Unit = {
    val teams = SoccerLeague.linesToTeams("/resources/football.dat")
    val best = teams.sortBy(team => math.abs(team.scored - team.missed)).head

    val days = WeatherData.linesToDays("/resources/weather.dat")
    val leastSpread = days.sortBy(day => day.maxTemp - day.minTemp).head
  }
}