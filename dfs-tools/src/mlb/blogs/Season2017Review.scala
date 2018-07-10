package mlb.blogs

import mlb._
import mlb.model._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._

object Season2017Review extends App {

  import mlb.Season2017Stats._

  val hitterLeagueAvgPointsPerGameStarted = mean(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  val hitterLeaguePointsPerGameStartedStdDev = stdDev(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  log(s"League avg PPG for hitters: ${hitterLeagueAvgPointsPerGameStarted.rounded(2)}, std deviation: ${hitterLeaguePointsPerGameStartedStdDev.rounded(2)}")

  val pitcherLeagueAvgPointsPerGameStarted = mean(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  val pitcherLeaguePointsPerGameStartedStdDev = stdDev(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  log(s"League avg PPG for pitchers: ${pitcherLeagueAvgPointsPerGameStarted.rounded(2)}, std deviation: ${pitcherLeaguePointsPerGameStartedStdDev.rounded(2)}")

  case class Stats(stdDev: Double, downsideDev: Double, upsideDev: Double) {
    val netUpsideDev: Double = upsideDev - downsideDev
  }

  // Part 1:

  log("\n***********************************************")
  log("***Who were the top-scoring players in 2017?***")
  log("***********************************************\n")

  log("\n### Hitters with most FPTS per game started (min 25 games started): ###\n")
  val mostPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = season.allHitters.filter(_.numberOfGamesStarted >= 25)
    .map(p => (p, p.hitterFptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerGameStarted.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Hitters with most FPTS per plate appearance (min 100 plate appearances): ###\n")
  val mostPointsPerAtBat: List[(PlayerSeasonStats, Float)] = season.allHitters.filter(_.hittingPA >= 100).map(p => (p, p.hitterFptsPerPA()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerAtBat.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.hittingPA} plate appearances)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with most FPTS per game started (min 10 games started): ###\n")
  val mostPitcherPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = season.allPitchers.filter(_.numberOfGamesStarted >= 10)
    .map(p => (p, p.hitterFptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPitcherPointsPerGameStarted.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n******************************************************************")
  log("***Which players improved the most throughout the 2017 season? ***")
  log("******************************************************************\n")

  log("\n### Hitters with most FPTS per plate appearance after All-Star break (min 100 plate appearances): ###\n")
  val mostPointsPerAtBatPostAllStar: List[(PlayerSeasonStats, Float)] = season2ndHalf.allHitters.filter(_.hittingPA >= 100).map(p => (p, p.hitterFptsPerPA()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerAtBatPostAllStar.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.hittingPA} plate appearances)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Hitters with biggest increase in FPTS per plate appearance after All-Star break (min 100 plate appearances): ###\n")
  val hitters = season.allHitters.filter { p =>
    season1stHalf.statsByPlayer.get(p.playerID).exists(_.hittingPA >= 100) && season2ndHalf.statsByPlayer.get(p.playerID).exists(_.hittingPA >= 100)
  }
  val mostImprovedPointsPerAtBat: List[(PlayerSeasonStats, Float)] = hitters.map(p =>
    (p, season2ndHalf.statsByPlayer(p.playerID).hitterFptsPerPA() - season1stHalf.statsByPlayer(p.playerID).hitterFptsPerPA()))
    .sortBy(_._2).reverse.take(10)
  mostImprovedPointsPerAtBat.map {
    case (p, fptsIncrease) =>
      s"${p.player} - ${fptsIncrease.rounded(2)} FPTS increase " +
        s"(1st half: ${season1stHalf.statsByPlayer(p.playerID).hitterFptsPerPA().rounded(1)} FPTS/PA in ${season1stHalf.statsByPlayer(p.playerID).hittingPA} plate appearances)" +
        s"(2nd half: ${season2ndHalf.statsByPlayer(p.playerID).hitterFptsPerPA().rounded(1)} FPTS/PA in ${season2ndHalf.statsByPlayer(p.playerID).hittingPA} plate appearances)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with most FPTS per game started after All-Star break (min 10 games started): ###\n")
  val mostPitcherPointsPerGameStartedPostAllStar: List[(PlayerSeasonStats, Float)] = season2ndHalf.allPitchers.filter(_.numberOfGamesStarted >= 10)
    .map(p => (p, p.hitterFptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPitcherPointsPerGameStartedPostAllStar.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with biggest increase in FPTS per game started after All-Star break (min 10 games started): ###\n")
  val pitchers = season.allPitchers.filter { p =>
    season1stHalf.statsByPlayer.get(p.playerID).exists(_.numberOfGamesStarted >= 10) && season2ndHalf.statsByPlayer.get(p.playerID).exists(_.numberOfGamesStarted >= 10)
  }
  val pitcherMostImprovedPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = pitchers.map(p =>
    (p, season2ndHalf.statsByPlayer(p.playerID).hitterFptsPerGameAsStarter() - season1stHalf.statsByPlayer(p.playerID).hitterFptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  pitcherMostImprovedPointsPerGameStarted.map {
    case (p, fptsIncrease) =>
      s"${p.player} - ${fptsIncrease.rounded(2)} FPTS increase " +
        s"(1st half: ${season1stHalf.statsByPlayer(p.playerID).hitterFptsPerGameAsStarter().rounded(1)} FPTS/game in ${season1stHalf.statsByPlayer(p.playerID).numberOfGamesStarted} starts)" +
        s"(2nd half: ${season2ndHalf.statsByPlayer(p.playerID).hitterFptsPerGameAsStarter().rounded(1)} FPTS/game in ${season2ndHalf.statsByPlayer(p.playerID).numberOfGamesStarted} starts)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  // Part 2:

  log("\n************************************************************")
  log("***Which of 2017's top players were the most consistent? ***")
  log("************************************************************\n")

  log("\n### Hitters with lowest standard deviation in FPTS per game started (min 25 games started, min 10 FPTS/game): ###\n")
  val pointsPerGameStartedStdDev: List[(PlayerSeasonStats, Double)] = season.allHitters
    .filter(p => p.numberOfGamesStarted >= 25 && p.hitterFptsPerGameAsStarter() >= 10)
    .map(p => (p, stdDev(p.gamesStarted.map(_.fantasyPoints()))))
  pointsPerGameStartedStdDev.sortBy(_._2).take(10).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.hitterFptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Top hitters ranked by standard deviation in FPTS per game started (min 25 games started, min 13 FPTS/game): ###\n")
  pointsPerGameStartedStdDev.filter(_._1.hitterFptsPerGameAsStarter() >= 13).sortBy(_._2).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.hitterFptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Top pitchers with lowest standard deviation in FPTS per game started (min 10 games started, min 35 FPTS/game): ###\n")
  val pitcherPointsPerGameStartedStdDev: List[(PlayerSeasonStats, Double)] = season.allPitchers
    .filter(p => p.numberOfGamesStarted >= 10 && p.hitterFptsPerGameAsStarter() >= 35)
    .map(p => (p, stdDev(p.gamesStarted.map(_.fantasyPoints()))))
  pitcherPointsPerGameStartedStdDev.sortBy(_._2).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.hitterFptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n*******************************************************************")
  log("***Which of 2017's top players were the most consistently good? ***")
  log("*******************************************************************\n")

  log("\n### Hitters with highest net upside deviation in fantasy points per game started (min 25 games started): ###\n")
  val pointsPerGameStartedDeviation: List[(PlayerSeasonStats, Stats)] = season.allHitters
    .filter(p => p.numberOfGamesStarted >= 25)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints())),
      downsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), hitterLeagueAvgPointsPerGameStarted),
      upsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), hitterLeagueAvgPointsPerGameStarted))))
  pointsPerGameStartedDeviation.sortBy(_._2.netUpsideDev).reverse.take(30).map {
    case (p, stats) =>
      s"${p.player} - ${stats.netUpsideDev.rounded(2)} Net Upside Dev, ${stats.downsideDev.rounded(2)} Downside Dev" +
        s", ${stats.upsideDev.rounded(2)} Upside Dev, ${stats.stdDev.rounded(2)} Std Dev (${p.hitterFptsPerGameAsStarter().rounded(1)} FPTS/game" +
        s", ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with highest net upside deviation in fantasy points per game started (min 10 games started): ###\n")
  val pitcherPointsPerGameStartedDeviation: List[(PlayerSeasonStats, Stats)] = season.allPitchers
    .filter(p => p.numberOfGamesStarted >= 10)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints())),
      downsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), pitcherLeagueAvgPointsPerGameStarted + pitcherLeaguePointsPerGameStartedStdDev),
      upsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), pitcherLeagueAvgPointsPerGameStarted + pitcherLeaguePointsPerGameStartedStdDev))))
  pitcherPointsPerGameStartedDeviation.sortBy(_._2.netUpsideDev).reverse.take(30).map {
    case (p, stats) =>
      s"${p.player} - ${stats.netUpsideDev.rounded(2)} Net Upside Dev, ${stats.downsideDev.rounded(2)} Downside Dev" +
        s", ${stats.upsideDev.rounded(2)} Upside Dev, ${stats.stdDev.rounded(2)} Std Dev (${p.hitterFptsPerGameAsStarter().rounded(1)} FPTS/game" +
        s", ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  // Part 3:

  log("\n********************************************************")
  log("***Who were the best high-ceiling players for GPP's? ***")
  log("********************************************************\n")

  log("\n### Hitters with highest % of games started > 1 std deviation above avg FPTS for all starters (min 25 games started): ###\n")
  val hitter1PPGLeagueAvgPlus1StdDev = hitterLeagueAvgPointsPerGameStarted + hitterLeaguePointsPerGameStartedStdDev
  val hitterPercentOfGamesAboveLeagueAvgPlus1StdDev = season.allHitters
    .filter(p => p.numberOfGamesStarted >= 25)
    .map(p => (p, percent(p.gamesStarted, (pgs: PlayerGameStats) => pgs.fantasyPoints() > hitter1PPGLeagueAvgPlus1StdDev)))
    .sortBy(_._2).reverse
  log("1 std deviation above avg FPTS for all starters = " + hitter1PPGLeagueAvgPlus1StdDev.rounded(1))
  hitterPercentOfGamesAboveLeagueAvgPlus1StdDev.take(10).map {
    case (p, percent) =>
      s"${p.player} - ${percent.rounded(1)}% (${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n**************************************************************")
  log("***Who were the safest (low floor) players for cash games? ***")
  log("**************************************************************\n")

  //highest % of games > 1 std deviation below avg FPTS for all starters

  log("\n*********************************************")
  log("***Which positions score the most points? ***")
  log("*********************************************\n")

  //average FPTS per game for all starters per position

}