package mlb.blogs

import mlb._
import mlb.model._
import mlb.retrosheet._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._

object Season2017Review extends App {

  val allStarGameDate = "2017-07-11".toDate("yyyy-MM-dd")

  val games = FileUtils.getListOfFiles(Configs.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

  val season = Season(2017, games)

  val season1stHalf = Season(2017, games.filter(_.date.before(allStarGameDate)))

  val season2ndHalf = Season(2017, games.filter(_.date.after(allStarGameDate)))

  log(s"Finished loading ${games.length} games --- ${season2ndHalf.games.length} games after All-Star break")

  log("\n***********************************************")
  log("***Who were the top-scoring players in 2017?***")
  log("***********************************************\n")

  log("\n### Hitters with most FPTS per game started (min 25 games started): ###\n")
  val mostPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = season.allHitters.filter(_.numberOfGamesStarted >= 25)
    .map(p => (p, p.fptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerGameStarted.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Hitters with most FPTS per plate appearance (min 100 plate appearances): ###\n")
  val mostPointsPerAtBat: List[(PlayerSeasonStats, Float)] = season.allHitters.filter(_.atBats >= 100).map(p => (p, p.fptsPerAtBat()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerAtBat.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.atBats} plate appearances)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with most FPTS per game started (min 10 games started): ###\n")
  val mostPitcherPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = season.allPitchers.filter(_.numberOfGamesStarted >= 10)
    .map(p => (p, p.fptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPitcherPointsPerGameStarted.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n******************************************************************")
  log("***Which players improved the most throughout the 2017 season? ***")
  log("******************************************************************\n")

  log("\n### Hitters with most FPTS per plate appearance after All-Star break (min 100 plate appearances): ###\n")
  val mostPointsPerAtBatPostAllStar: List[(PlayerSeasonStats, Float)] = season2ndHalf.allHitters.filter(_.atBats >= 100).map(p => (p, p.fptsPerAtBat()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerAtBatPostAllStar.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.atBats} plate appearances)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Hitters with biggest increase in FPTS per plate appearance after All-Star break (min 100 plate appearances): ###\n")
  val hitters = season.allHitters.filter { p =>
    season1stHalf.statsByPlayer.get(p.playerID).exists(_.atBats >= 100) && season2ndHalf.statsByPlayer.get(p.playerID).exists(_.atBats >= 100)
  }
  val mostImprovedPointsPerAtBat: List[(PlayerSeasonStats, Float)] = hitters.map(p =>
    (p, season2ndHalf.statsByPlayer(p.playerID).fptsPerAtBat() - season1stHalf.statsByPlayer(p.playerID).fptsPerAtBat()))
    .sortBy(_._2).reverse.take(10)
  mostImprovedPointsPerAtBat.map {
    case (p, fptsIncrease) =>
      s"${p.player} - ${fptsIncrease.rounded(2)} FPTS increase " +
        s"(1st half: ${season1stHalf.statsByPlayer(p.playerID).fptsPerAtBat().rounded(1)} FPTS/PA in ${season1stHalf.statsByPlayer(p.playerID).atBats} plate appearances)" +
        s"(2nd half: ${season2ndHalf.statsByPlayer(p.playerID).fptsPerAtBat().rounded(1)} FPTS/PA in ${season2ndHalf.statsByPlayer(p.playerID).atBats} plate appearances)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with most FPTS per game started after All-Star break (min 10 games started): ###\n")
  val mostPitcherPointsPerGameStartedPostAllStar: List[(PlayerSeasonStats, Float)] = season2ndHalf.allPitchers.filter(_.numberOfGamesStarted >= 10)
    .map(p => (p, p.fptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPitcherPointsPerGameStartedPostAllStar.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(2)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with biggest increase in FPTS per game started after All-Star break (min 10 games started): ###\n")
  val pitchers = season.allPitchers.filter { p =>
    season1stHalf.statsByPlayer.get(p.playerID).exists(_.numberOfGamesStarted >= 10) && season2ndHalf.statsByPlayer.get(p.playerID).exists(_.numberOfGamesStarted >= 10)
  }
  val pitcherMostImprovedPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = pitchers.map(p =>
    (p, season2ndHalf.statsByPlayer(p.playerID).fptsPerGameAsStarter() - season1stHalf.statsByPlayer(p.playerID).fptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  pitcherMostImprovedPointsPerGameStarted.map {
    case (p, fptsIncrease) =>
      s"${p.player} - ${fptsIncrease.rounded(2)} FPTS increase " +
        s"(1st half: ${season1stHalf.statsByPlayer(p.playerID).fptsPerGameAsStarter().rounded(1)} FPTS/game in ${season1stHalf.statsByPlayer(p.playerID).gamesStarted} starts)" +
        s"(2nd half: ${season2ndHalf.statsByPlayer(p.playerID).fptsPerGameAsStarter().rounded(1)} FPTS/game in ${season2ndHalf.statsByPlayer(p.playerID).gamesStarted} starts)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n************************************************************")
  log("***Which of 2017's top players were the most consistent? ***")
  log("************************************************************\n")

  log("\n### Hitters with lowest standard deviation in DFS pts per game started (min 25 games started, min 10 FPTS/game): ###\n")
  val pointsPerGameStartedStdDev: List[(PlayerSeasonStats, Double)] = season.allHitters
    .filter(p => p.numberOfGamesStarted >= 25 && p.fptsPerGameAsStarter() >= 10)
    .map(p => (p, stdDev(p.gamesStarted.map(_.fantasyPoints()))))
  pointsPerGameStartedStdDev.sortBy(_._2).take(10).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.fptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with lowest standard deviation in DFS pts per game started (min 10 games started, min 30 FPTS/game): ###\n")
  val pitcherPointsPerGameStartedStdDev: List[(PlayerSeasonStats, Double)] = season.allPitchers
    .filter(p => p.numberOfGamesStarted >= 10 && p.fptsPerGameAsStarter() >= 30)
    .map(p => (p, stdDev(p.gamesStarted.map(_.fantasyPoints()))))
  pitcherPointsPerGameStartedStdDev.sortBy(_._2).take(10).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.fptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n*************************************************************")
  log("***Which of 2017's top players were the least consistent? ***")
  log("*************************************************************\n")

  log("\n### Hitters with highest standard deviation in DFS pts per game started (min 25 games started, min 10 FPTS/game): ###\n")
  pointsPerGameStartedStdDev.sortBy(_._2).reverse.take(10).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.fptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Pitchers with highest standard deviation in DFS pts per game started (min 10 games started, min 30 FPTS/game): ###\n")
  pitcherPointsPerGameStartedStdDev.sortBy(_._2).reverse.take(10).map {
    case (p, stdDev) =>
      s"${p.player} - ${stdDev.rounded(2)} Std Dev (${p.fptsPerGameAsStarter().rounded(1)} FPTS/game, ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

}