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
import mlb.Season2017Stats._

object Draftbook20180329 extends App {

  Season2017Stats.logSummary

  val (pitchers, hitters) = Players.allPlayers.filter(p => p.fanduel.nonEmpty || p.draftkings.nonEmpty).partition(_.position == Pitcher)
  val startingPitchers = pitchers.filter(_.fanduel.flatMap(_.starter).getOrElse(false))

  //  println("\n\nStarting pitchers: \n" + startingPitchers.sortBy(_.name).map { pitcher =>
  //    s"$pitcher [${pitcherStatsAgainstAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("???")} FPTS/AB against (FanDuel), " +
  //      s"${pitcherStatsAgainstAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("???")} FPTS/AB against (DraftKings)] vs: \n\t${
  //        pitcher.opposingHitters(hitters).sortBy(p => season.statsByPlayer(p.id).fptsPerAtBat(FanDuelMLB)).reverse.map { hitter =>
  //          s"$hitter - ${season.statsByPlayer(hitter.id).fptsPerAtBat(FanDuelMLB).rounded(1)} FPTS/AB (FanDuel), " +
  //            s"${season.statsByPlayer(hitter.id).fptsPerAtBat(DraftKingsMLB).rounded(1)} FPTS/AB (DraftKings)"
  //        }.mkString("\n\t")
  //      }"
  //  }.mkString("\n"))

  println("\n\nStarting pitchers: \n" + startingPitchers.sortBy(_.name).map { pitcher =>
    s"$pitcher [${pitcherStatsAgainstAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("???")} FPTS/AB against (FanDuel), " +
      s"${pitcherStatsAgainstAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("???")} FPTS/AB against (DraftKings)] vs: \n\t${
        pitcher.opposingHitters(hitters).sortBy(p => season.statsByPlayer(p.id).fptsPerAtBat(FanDuelMLB)).reverse.map { hitter =>
          s"${hitter.name} (${hitter.bats}) - ${
            hitter.fanduel.map(_.salary) match {
              case Some(salary) => ((season.statsByPlayer(hitter.id).fptsPerAtBat(FanDuelMLB).toDouble / salary.toDouble) * 1000d).rounded(2)
              case None         => "???"
            }
          } value (FanDuel), ${
            hitter.draftkings.map(_.salary) match {
              case Some(salary) => ((season.statsByPlayer(hitter.id).fptsPerAtBat(DraftKingsMLB).toDouble / salary.toDouble) * 1000d).rounded(2)
              case None         => "???"
            }
          } value (DraftKings)"
        }.mkString("\n\t")
      }"
  }.mkString("\n"))

  val hitterFPPGDeviation_FD: List[(PlayerSeasonStats, Stats)] = season.allHitters.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD))))

  val pitcherPointsPerGameStartedDeviation_FD: List[(PlayerSeasonStats, Stats)] = season.allPitchers //.filter(p => startingPitchers.contains(p.player))
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD))))

  val pointsPerGameStartedDeviation_DK: List[(PlayerSeasonStats, Stats)] = season.allHitters //.filter(p => hitters.contains(p.player))
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK))))

  val pitcherPointsPerGameStartedDeviation_DK: List[(PlayerSeasonStats, Stats)] = season.allPitchers //.filter(p => startingPitchers.contains(p.player))
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK))))

  log("\n**************************************************")
  log("*** Hitter stacks ***")
  log("**************************************************\n")

  log("\n### Pitchers ranked by FPTS given up per plate appearance: ###\n")
  log(toHtmlTable(
    List("Pitcher", "Opponent", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
    startingPitchers
      .sortBy { p => pitcherStatsAgainstAllHitters.get(p).map(_.fptsPerAtBatAgainst_FD).getOrElse(0.0d) }.reverse
      .map { pitcher =>
        val statsAgainst = pitcherStatsAgainstAllHitters.get(pitcher)
        List(pitcher,
          pitcher.opponent.get,
          statsAgainst.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("Unknown"),
          statsAgainst.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("Unknown"),
          statsAgainst.map(_.atBatsAgainst).getOrElse("Unknown"))
      }))

  log("\n### Top 10 pitchers ranked by FPTS given up per plate appearance (by batter handedness): ###\n")
  log(toHtmlTable(
    List("Pitcher", "Opponent", "Against hitters who bat...", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
    startingPitchers
      .flatMap { pitcher =>
        List(pitcherStatsAgainstLefties.get(pitcher), pitcherStatsAgainstRighties.get(pitcher), pitcherStatsAgainstSwitchHitters.get(pitcher)).flatten
      }
      .sortBy(_.fptsPerAtBatAgainst_FD).reverse
      //.take(10)
      .map { stats =>
        List(stats.pitcher,
          stats.pitcher.opponent.get,
          stats.batterHandedness.get.toVerboseString,
          stats.fptsPerAtBatAgainst_FD.rounded(1),
          stats.fptsPerAtBatAgainst_DK.rounded(1),
          stats.atBatsAgainst)
      }))

  log("\n**************************************************")
  log("*** Pitchers ***")
  log("**************************************************\n")

  //  log("\n### Probable starting pitchers ranked by fantasy points per game started (FPPG) net upside deviation / salary: ###\n")
  //  log(toHtmlTable(
  //    List("Player", "NUD / FD salary", "FD salary", "Avg FD FPPG", "NUD / DK salary", "DK salary", "Avg DK FPPG", "# of games started (2017)"),
  //    pitcherPointsPerGameStartedDeviation.sortBy { case (p, s) => s.netUpsideDev }.reverse.take(20).map {
  //      case (p, stats) =>
  //        List(p.player,
  //          stats.netUpsideDev.rounded(2),
  //          stats.upsideDev.rounded(2),
  //          stats.downsideDev.rounded(2),
  //          p.fptsPerGameAsStarter().rounded(1),
  //          p.numberOfGamesStarted)
  //    }))

}