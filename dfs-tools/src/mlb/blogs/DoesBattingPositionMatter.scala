package mlb.blogs

import mlb._
import mlb.model._
import mlb.model.CustomTypes._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._
import mlb.HistoricalStats._

object DoesBattingPositionMatter extends App {
  
  import mlb.Past1YearStats.stats._

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
  
  case class BattingPositionStats(player: Player, batPosition: Int, atBats: Int, fptsPerABForThisBP: Double, fptsPerABForAllBP: Double)
  
  val statsPerBatPosition: Map[Int, List[BattingPositionStats]] = season.allHitters.flatMap { seasonStats =>
    val player = seasonStats.player
    val statsPerBattingPos = seasonStats.games.groupBy(_.battingPosition).map { case (bp, games) =>
      val atBats = games.map(_.hittingStats.atBats).sum
      val fptsPerAB = games.map(_.hittingStats.fantasyPoints().toDouble).sum / atBats
      BattingPositionStats(player, bp, atBats, fptsPerAB, seasonStats.fptsPerAtBat())
    }.toList.sortBy(_.batPosition).filter(_.atBats >= 20)
    //println(s"$player:\n\t${statsPerBattingPos.map{stats => s"${stats.batPosition}) ${stats.fptsPerABForThisBP.rounded(2)} FPTS/PA in ${stats.atBats} PA"}.mkString("\n\t")}")
    statsPerBattingPos
  }.groupBy(_.batPosition)
  
  val avgDeltaPerBatPosition = statsPerBatPosition.map{ case (bp, stats) =>
    val weightedDeltas = stats.map(s => (s.fptsPerABForThisBP - s.fptsPerABForAllBP) * s.atBats)
    val avgDelta = weightedDeltas.sum / stats.map(_.atBats).sum
    (bp, avgDelta)
  }
  
  println(avgDeltaPerBatPosition.toList.sortBy(_._1).map {case (bp, avgDelta) => s"$bp) ${avgDelta.rounded(3)}"}.mkString("\n"))

}