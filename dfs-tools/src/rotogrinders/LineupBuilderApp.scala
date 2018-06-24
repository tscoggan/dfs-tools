package rotogrinders

import utils.DateTimeUtils._
import utils.Logger._
import utils._
import LineupFilters._
import scala.collection.mutable

/**
 * Takes RotoGrinders player projections CSV as input and generates one or more optimal lineups
 * as specified in the build.conf file.
 */
object LineupBuilderApp extends App {

  val projectionsFile = {
    print(s"\nRG player projections file name:\n>> ")
    val input = scala.io.StdIn.readLine.trim
    if (input.contains("/") || input.contains("\\")) input
    else Configs.LineupBuilder.dataFileDir + "/" + input
  }

  log(s"Parsing $projectionsFile...")

  val players = Player.parseFrom(projectionsFile).distinct

  print(s"Parsed ${players.length} players")
  //players.sortBy(_.ptsPerSalary).foreach { x => log(x) }

  print(s"\nDFS site (${Configs.LineupBuilder.contestTypes.map(_.site).distinct.mkString(",")}):\n>> ")
  val site = scala.io.StdIn.readLine.trim.toUpperCase

  print(s"\nSport (${Configs.LineupBuilder.contestTypes.filter(ct => ct.site == site).map(_.sport).distinct.mkString(",")}):\n>> ")
  val sport = scala.io.StdIn.readLine.trim.toUpperCase

  val contestType = Configs.LineupBuilder.contestTypes.find(ct => ct.site == site && ct.sport == sport)
    .getOrElse(throw new Exception(s"Invalid site ($site) or sport ($sport)"))
  //log(contestType)

  print(s"\n# of lineups to build:\n>> ")
  val numberOfLineups = scala.math.max(scala.io.StdIn.readLine.trim.toInt, 1) // build at least 1 lineup

  val builder = LineupIterator(players.filter(p => p.projPoints >= contestType.minPlayerFPTS || p.projValue >= contestType.minPlayerValue), contestType)
  log(builder.toString)

  log(s"Building top $numberOfLineups lineups...")
  val lineups = new TopNArray[Lineup](numberOfLineups)
  var count = 1
  val (_, elapsedSeconds) = timeInSeconds {
    while (builder.hasNext) {
      builder.nextValidLineup.foreach { lineup =>
        lineups.add(lineup)
      }
      count = count + 1
      if (count == 100000) {
        count = 0
        logDebug(s"Built ${builder.numberBuilt} lineups")
      }
    }
  }
  log(s"Built ${builder.numberBuilt} lineups in $elapsedSeconds seconds")

  val chosenLineups: mutable.ArrayBuffer[Lineup] = LineupFilters(lineups.array)
    .differBy(Configs.LineupBuilder.minPlayersDifferentPerLineup)
    .maxExposure(Configs.LineupBuilder.maxExposurePercentage, numberOfLineups) //########## NOT WORKING ############
    .lineups

  log(s"Final ${chosenLineups.length} lineups:\n\t${
    chosenLineups.map {
      lineup => "Proj: " + f"${lineup.projectedPoints}%.2f" + s", Salary: ${lineup.salary} --- $lineup"
    }.mkString("\n\t")
  }\n")

  log(LineupAnalyzer.playerExposuresString(chosenLineups.toList))

}