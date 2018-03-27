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

  val pointsPerGameStartedDeviation: List[(PlayerSeasonStats, Stats)] = season.allHitters
    .filter(p => p.numberOfGamesStarted >= 25)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints())),
      downsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), hitterLeagueAvgPointsPerGameStarted),
      upsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), hitterLeagueAvgPointsPerGameStarted))))

  val pitcherPointsPerGameStartedDeviation: List[(PlayerSeasonStats, Stats)] = season.allPitchers
    .filter(p => p.numberOfGamesStarted >= 10)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints())),
      downsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), pitcherLeagueAvgPointsPerGameStarted + pitcherLeaguePointsPerGameStartedStdDev),
      upsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), pitcherLeagueAvgPointsPerGameStarted + pitcherLeaguePointsPerGameStartedStdDev))))

  log("\n**************************************************")
  log("***How can we identify good cash game players? ***")
  log("**************************************************\n")

  log("\n### Hitters with lowest downside deviation in fantasy points per game started (min 25 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Downside deviation", "Upside deviation", "Std deviation", "Avg FPTS / game", "# of games started"),
    pointsPerGameStartedDeviation.sortBy(_._2.downsideDev).take(30).map {
      case (p, stats) =>
        List(p.player,
          stats.downsideDev.rounded(2),
          stats.upsideDev.rounded(2),
          stats.stdDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

  log("\n### Pitchers with lowest downside deviation in fantasy points per game started (min 10 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Downside deviation", "Upside deviation", "Std deviation", "Avg FPTS / game", "# of games started"),
    pitcherPointsPerGameStartedDeviation.sortBy(_._2.downsideDev).take(30).map {
      case (p, stats) =>
        List(p.player,
          stats.downsideDev.rounded(2),
          stats.upsideDev.rounded(2),
          stats.stdDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

  log("\n********************************************")
  log("***How can we identify good GPP players? ***")
  log("********************************************\n")

  log("\n### Hitters with highest upside deviation in fantasy points per game started (min 25 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Upside deviation", "Downside deviation", "Std deviation", "Avg FPTS / game", "# of games started"),
    pointsPerGameStartedDeviation.sortBy(_._2.upsideDev).reverse.take(30).map {
      case (p, stats) =>
        List(p.player,
          stats.upsideDev.rounded(2),
          stats.downsideDev.rounded(2),
          stats.stdDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

  //  log("\n### Hitters with highest % of games started > 1 std deviation above avg FPTS for all starters (min 25 games started): ###\n")
  //  val hitter1PPGLeagueAvgPlus1StdDev = hitterLeagueAvgPointsPerGameStarted + hitterLeaguePointsPerGameStartedStdDev
  //  val hitterPercentOfGamesAboveLeagueAvgPlus1StdDev = season.allHitters
  //    .filter(p => p.numberOfGamesStarted >= 25)
  //    .map(p => (p, percent(p.gamesStarted, (pgs: PlayerGameStats) => pgs.fantasyPoints() > hitter1PPGLeagueAvgPlus1StdDev)))
  //    .sortBy(_._2).reverse
  //  log("1 std deviation above avg FPTS for all starters = " + hitter1PPGLeagueAvgPlus1StdDev.rounded(1))
  //  hitterPercentOfGamesAboveLeagueAvgPlus1StdDev.take(30).map {
  //    case (p, percent) =>
  //      s"${p.player} - ${percent.rounded(1)}% (${p.numberOfGamesStarted} games started)"
  //  }.zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Hitters with highest (upside deviation / avg FPTS/game) in fantasy points per game started (min 25 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Upside Dev / FPPG", "Downside deviation", "Upside deviation", "Avg FPTS / game", "# of games started"),
    pointsPerGameStartedDeviation.sortBy { case (p, s) => s.upsideDev / p.fptsPerGameAsStarter() }.reverse.take(30).map {
      case (p, stats) =>
        List(p.player,
          (stats.upsideDev / p.fptsPerGameAsStarter()).rounded(2),
          stats.downsideDev.rounded(2),
          stats.upsideDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

  //  val pitcherPointsPerGameStartedDeviation2: List[(PlayerSeasonStats, Stats)] = season.allPitchers
  //    .filter(p => p.numberOfGamesStarted >= 10)
  //    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints())),
  //      downsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), pitcherLeagueAvgPointsPerGameStarted),
  //      upsideDev(p.gamesStarted.map(_.fantasyPoints().toDouble), pitcherLeagueAvgPointsPerGameStarted))))

  log("\n### Pitchers with highest (upside deviation / avg FPTS/game) in fantasy points per game started (min 10 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Upside Dev / FPPG", "Downside deviation", "Upside deviation", "Avg FPTS / game", "# of games started"),
    pitcherPointsPerGameStartedDeviation.sortBy { case (p, s) => s.upsideDev / p.fptsPerGameAsStarter() }.reverse.take(30).map {
      case (p, stats) =>
        List(p.player,
          (stats.upsideDev / p.fptsPerGameAsStarter()).rounded(2),
          stats.downsideDev.rounded(2),
          stats.upsideDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

  log("\n### Hitters with highest net upside deviation in fantasy points per game started (min 25 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Net upside deviation", "Upside deviation", "Downside deviation", "Avg FPTS / game", "# of games started"),
    pointsPerGameStartedDeviation.sortBy { case (p, s) => s.netUpsideDev }.reverse.take(20).map {
      case (p, stats) =>
        List(p.player,
          stats.netUpsideDev.rounded(2),
          stats.upsideDev.rounded(2),
          stats.downsideDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

  log("\n### Pitchers with highest net upside deviation in fantasy points per game started (min 10 games started): ###\n")
  log(toHtmlTable(
    List("Player", "Net upside deviation", "Upside deviation", "Downside deviation", "Avg FPTS / game", "# of games started"),
    pitcherPointsPerGameStartedDeviation.sortBy { case (p, s) => s.netUpsideDev }.reverse.take(20).map {
      case (p, stats) =>
        List(p.player,
          stats.netUpsideDev.rounded(2),
          stats.upsideDev.rounded(2),
          stats.downsideDev.rounded(2),
          p.fptsPerGameAsStarter().rounded(1),
          p.numberOfGamesStarted)
    }))

}