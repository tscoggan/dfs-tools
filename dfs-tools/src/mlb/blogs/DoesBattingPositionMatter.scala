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

object DoesBattingPositionMatter extends App {

  val allStarGameDate = "2017-07-11".toDate("yyyy-MM-dd")

  val games = FileUtils.getListOfFiles(Configs.Retrosheet.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

  val season = Season(2017, games)

  val season1stHalf = Season(2017, games.filter(_.date.before(allStarGameDate)))

  val season2ndHalf = Season(2017, games.filter(_.date.after(allStarGameDate)))

  log(s"Finished loading ${games.length} games --- ${season2ndHalf.games.length} games after All-Star break")

  val hitterLeagueAvgPointsPerGameStarted = mean(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  val hitterLeaguePointsPerGameStartedStdDev = stdDev(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  log(s"League avg PPG for hitters: ${hitterLeagueAvgPointsPerGameStarted.rounded(2)}, std deviation: ${hitterLeaguePointsPerGameStartedStdDev.rounded(2)}")

  val pitcherLeagueAvgPointsPerGameStarted = mean(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  val pitcherLeaguePointsPerGameStartedStdDev = stdDev(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints()))
  log(s"League avg PPG for pitchers: ${pitcherLeagueAvgPointsPerGameStarted.rounded(2)}, std deviation: ${pitcherLeaguePointsPerGameStartedStdDev.rounded(2)}")

  case class Stats(stdDev: Double, downsideDev: Double, upsideDev: Double) {
    val netUpsideDev: Double = upsideDev - downsideDev
  }

  type BattingPosition = Int
  type AtBats = Int
  type AvgFpts = Double

  log("\n***************************************************************************************")
  log("***2017 league-average FPTS per plate appearance by batting position (hitters only) ***")
  log("***************************************************************************************\n")

  val leagueAvgFptsPerAtBatByBattingPosition: Map[BattingPosition, (AtBats, AvgFpts)] = season.allHitters.flatMap(_.games).groupBy(_.battingPosition).map {
    case (bp, games) => (bp -> (games.map(_.atBats).sum, games.map(_.fantasyPoints().toDouble).sum / games.map(_.atBats).sum))
  }

  leagueAvgFptsPerAtBatByBattingPosition.toList.sortBy(_._1).tail.foreach {
    case (battingPosition, (atBats, avgFptsPerAB)) =>
      println(s"$battingPosition) ${avgFptsPerAB.rounded(2)} FPTS per plate appearance ($atBats plates appearances)")
  }

  log("\n### 2017 league-average FPTS per plate appearance by batting position (hitters only): ###\n")
  log(toHtmlTable(
    List("Batting position", "Avg FPTS per plate appearance (FD)", "Total # of plate appearances"),
    leagueAvgFptsPerAtBatByBattingPosition.toList.sortBy(_._1).tail.map {
      case (battingPosition, (atBats, avgFptsPerAB)) =>
        List(battingPosition,
          avgFptsPerAB.rounded(2),
          atBats)
    }))

  log("\n*********************************************************************************************************************")
  log("***+/- FPTS/PA in each batting position compared to each player's avg across all batting positions (hitters only) ***")
  log("*********************************************************************************************************************\n")

}