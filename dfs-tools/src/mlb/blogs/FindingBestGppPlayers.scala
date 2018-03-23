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

object FindingBestGppPlayers extends App {

  val allStarGameDate = "2017-07-11".toDate("yyyy-MM-dd")

  val games = FileUtils.getListOfFiles(Configs.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

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
  hitterPercentOfGamesAboveLeagueAvgPlus1StdDev.take(30).map {
    case (p, percent) =>
      s"${p.player} - ${percent.rounded(1)}% (${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))
  
  log("\n### Hitters with highest upside deviation in fantasy points per game started (min 25 games started): ###\n")
  val pointsPerGameStartedDeviation: List[(PlayerSeasonStats, Stats)] = season.allHitters
    .filter(p => p.numberOfGamesStarted >= 25)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints())),
      downsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), hitterLeagueAvgPointsPerGameStarted),
      upsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), hitterLeagueAvgPointsPerGameStarted))))
  pointsPerGameStartedDeviation.sortBy(_._2.upsideDev).reverse.take(30).map {
    case (p, stats) =>
      s"${p.player} - ${stats.netUpsideDev.rounded(2)} Net Upside Dev, ${stats.downsideDev.rounded(2)} Downside Dev" +
        s", ${stats.upsideDev.rounded(2)} Upside Dev, ${stats.stdDev.rounded(2)} Std Dev (${p.fptsPerGameAsStarter().rounded(1)} FPTS/game" +
        s", ${p.numberOfGamesStarted} games started)"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))
  
  log("\n### Hitters with highest (upside deviation - avg FPTS/game) in fantasy points per game started (min 25 games started): ###\n")
  pointsPerGameStartedDeviation.sortBy{case (p,s) => s.upsideDev - p.fptsPerGameAsStarter()}.reverse.take(30).map {
    case (p, stats) =>
      s"${p.player} - ${stats.netUpsideDev.rounded(2)} Net Upside Dev, ${stats.downsideDev.rounded(2)} Downside Dev" +
        s", ${stats.upsideDev.rounded(2)} Upside Dev, ${stats.stdDev.rounded(2)} Std Dev (${p.fptsPerGameAsStarter().rounded(1)} FPTS/game" +
        s", ${p.numberOfGamesStarted} games started)" +
        s"\nGame log: ${p.gamesStarted.sortBy(_.gameDate).map(_.fantasyPoints()).mkString(", ")}"
  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))


}