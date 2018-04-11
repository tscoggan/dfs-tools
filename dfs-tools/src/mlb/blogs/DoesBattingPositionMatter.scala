package mlb.blogs

import mlb._
import mlb.model._
import mlb.model.CustomTypes._
import mlb.retrosheet._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._
import mlb.Season2017Stats._

object DoesBattingPositionMatter extends App {

  //  leagueAvgFptsPerAtBatByBattingPosition.toList.sortBy(_._1).tail.foreach {
  //    case (battingPosition, (atBats, avgFptsPerAB)) =>
  //      println(s"$battingPosition) ${avgFptsPerAB.rounded(2)} FPTS per plate appearance ($atBats plates appearances)")
  //  }

  log("\n*************************************************************")
  log("***2017 league-average FPTS per game by batting position  ***")
  log("*************************************************************\n")

  log("\n### 2017 league-average FPTS per game by batting position: ###\n")
  log(toTable(
    List("Batting position", "Total # of plate appearances", "Avg FPTS / plate appearance (FD)", "Avg plate appearances / game", "Avg FPTS / game"),
    leagueAvgStatsByBattingPosition.toList.sortBy(_._1).tail.map {
      case (battingPosition, stats) =>
        List(battingPosition,
          stats.totalAtBats,
          stats.fptsPerAtBat.rounded(2),
          stats.atBatsPerGame.rounded(2),
          stats.fptsPerGame.rounded(2))
    }))

  log("\n### 2017 league-average FPTS per game by batting position (visiting team): ###\n")
  log(toTable(
    List("Batting position", "Total # of plate appearances", "Avg FPTS / PA (FD)", "Avg plate appearances / game", "Avg FPTS / game"),
    leagueAvgStatsByBattingPosition_VisitingTeam.toList.sortBy(_._1).tail.map {
      case (battingPosition, stats) =>
        List(battingPosition,
          stats.totalAtBats,
          stats.fptsPerAtBat.rounded(2),
          stats.atBatsPerGame.rounded(2),
          stats.fptsPerGame.rounded(2))
    }))

  log("\n### 2017 league-average FPTS per game by batting position (home team): ###\n")
  log(toTable(
    List("Batting position", "Total # of plate appearances", "Avg FPTS / PA (FD)", "Avg plate appearances / game", "Avg FPTS / game"),
    leagueAvgStatsByBattingPosition_HomeTeam.toList.sortBy(_._1).tail.map {
      case (battingPosition, stats) =>
        List(battingPosition,
          stats.totalAtBats,
          stats.fptsPerAtBat.rounded(2),
          stats.atBatsPerGame.rounded(2),
          stats.fptsPerGame.rounded(2))
    }))

  log("\n*********************************************************************************************************************")
  log("***+/- FPTS/PA in each batting position compared to each player's avg across all batting positions (hitters only) ***")
  log("*********************************************************************************************************************\n")
  
  

}