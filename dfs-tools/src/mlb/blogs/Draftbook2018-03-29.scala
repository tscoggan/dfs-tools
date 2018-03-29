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

  val hitters_FD = hitters.filter(_.fanduel.map(_.salary).nonEmpty)
  val hitters_DK = hitters.filter(_.draftkings.map(_.salary).nonEmpty)

  val expensiveHitters_FD = hitters_FD.filter(_.fanduel.map(_.salary).get >= 3500)
  val cheapHitters_FD = hitters_FD.filter(p => p.fanduel.map(_.salary).get < 3500 && p.fanduel.map(_.salary).get >= 2500)

  val expensiveHitters_DK = hitters_DK.filter(_.draftkings.map(_.salary).get >= 4000)
  val cheapHitters_DK = hitters_DK.filter(p => p.draftkings.map(_.salary).get < 4000 && p.draftkings.map(_.salary).get > 3000)

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

  val hitter2017Stats_FD: Map[Player, (PlayerSeasonStats, Stats)] = season.allHitters.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val pitcher2017Stats_FD: Map[Player, (PlayerSeasonStats, Stats)] = season.allPitchers.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val hitter2017Stats_DK: Map[Player, (PlayerSeasonStats, Stats)] = season.allHitters.filter(_.player.draftkings.nonEmpty)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val pitcher2017Stats_DK: Map[Player, (PlayerSeasonStats, Stats)] = season.allPitchers.filter(_.player.draftkings.nonEmpty)
    .map(p => (p, Stats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

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
  log("*** Value hitters - FD ***")
  log("**************************************************\n")

  log("\n### Top 10 expensive hitters ranked by value (FanDuel): ###\n")
  log(toHtmlTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Value"),
    expensiveHitters_FD.flatMap(p => hitter2017Stats_FD.get(p).map(stats => (p, stats))).map {
      case (p, (seasonStats, deviationStats)) =>
        val pitcherFptsPerAtBatAgainst = p.bats match {
          case Left   => pitcherStatsAgainstLefties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_FD).get //.getOrElse(0)
          case Right  => pitcherStatsAgainstRighties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_FD).get //.getOrElse(0)
          case Switch => pitcherStatsAgainstSwitchHitters.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_FD).get //.getOrElse(0)
        }
        val value = mean(List(seasonStats.fptsPerAtBat(FanDuelMLB), seasonStats.fptsPerAtBat(FanDuelMLB), pitcherFptsPerAtBatAgainst)) / p.fanduel.map(_.salary).get
        (p, value)
    }.sortBy(_._2).reverse.take(10).map {
      case (p, value) =>
        List(p, p.fanduel.map(fd => "$" + fd.salary).getOrElse("Unknown"), p.opposingPitcher(startingPitchers), (value * 1000).rounded(2))
    }))

  log("\n### Top 10 cheap hitters ranked by value (FanDuel): ###\n")
  log(toHtmlTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Value"),
    cheapHitters_FD.flatMap(p => hitter2017Stats_FD.get(p).map(stats => (p, stats))).map {
      case (p, (seasonStats, deviationStats)) =>
        val pitcherFptsPerAtBatAgainst = p.bats match {
          case Left   => pitcherStatsAgainstLefties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_FD).get //.getOrElse(0)
          case Right  => pitcherStatsAgainstRighties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_FD).get //.getOrElse(0)
          case Switch => pitcherStatsAgainstSwitchHitters.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_FD).get //.getOrElse(0)
        }
        val value = mean(List(seasonStats.fptsPerAtBat(FanDuelMLB), seasonStats.fptsPerAtBat(FanDuelMLB), pitcherFptsPerAtBatAgainst)) / p.fanduel.map(_.salary).get
        (p, value)
    }.sortBy(_._2).reverse.take(10).map {
      case (p, value) =>
        List(p, p.fanduel.map(fd => "$" + fd.salary).getOrElse("Unknown"), p.opposingPitcher(startingPitchers), (value * 1000).rounded(2))
    }))

  log("\n**************************************************")
  log("*** Value hitters - DK ***")
  log("**************************************************\n")

  log("\n### Top 10 expensive hitters ranked by value (DraftKings): ###\n")
  log(toHtmlTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Value"),
    expensiveHitters_DK.flatMap(p => hitter2017Stats_DK.get(p).map(stats => (p, stats))).map {
      case (p, (seasonStats, deviationStats)) =>
        val pitcherFptsPerAtBatAgainst = p.bats match {
          case Left   => pitcherStatsAgainstLefties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_DK).get //.getOrElse(0)
          case Right  => pitcherStatsAgainstRighties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_DK).get //.getOrElse(0)
          case Switch => pitcherStatsAgainstSwitchHitters.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_DK).get //.getOrElse(0)
        }
        val value = mean(List(seasonStats.fptsPerAtBat(DraftKingsMLB), seasonStats.fptsPerAtBat(DraftKingsMLB), pitcherFptsPerAtBatAgainst)) / p.draftkings.map(_.salary).get
        (p, value)
    }.sortBy(_._2).reverse.take(10).map {
      case (p, value) =>
        List(p, p.draftkings.map(dk => "$" + dk.salary).getOrElse("Unknown"), p.opposingPitcher(startingPitchers), (value * 1000).rounded(2))
    }))

  log("\n### Top 10 cheap hitters ranked by value (DraftKings): ###\n")
  log(toHtmlTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Value"),
    cheapHitters_DK.flatMap(p => hitter2017Stats_DK.get(p).map(stats => (p, stats))).map {
      case (p, (seasonStats, deviationStats)) =>
        val pitcherFptsPerAtBatAgainst = p.bats match {
          case Left   => pitcherStatsAgainstLefties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_DK).get //.getOrElse(0)
          case Right  => pitcherStatsAgainstRighties.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_DK).get //.getOrElse(0)
          case Switch => pitcherStatsAgainstSwitchHitters.get(p.opposingPitcher(startingPitchers)).map(_.fptsPerAtBatAgainst_DK).get //.getOrElse(0)
        }
        val value = mean(List(seasonStats.fptsPerAtBat(DraftKingsMLB), seasonStats.fptsPerAtBat(DraftKingsMLB), pitcherFptsPerAtBatAgainst)) / p.draftkings.map(_.salary).get
        (p, value)
    }.sortBy(_._2).reverse.take(10).map {
      case (p, value) =>
        List(p, p.draftkings.map(dk => "$" + dk.salary).getOrElse("Unknown"), p.opposingPitcher(startingPitchers), (value * 1000).rounded(2))
    }))

  log("\n**************************************************")
  log("*** Pitchers ***")
  log("**************************************************\n")

  log("\n### Starting pitchers ranked by value (FanDuel): ###\n")
  log(toHtmlTable(
    List("Pitcher", "FD Salary", "Opponent", "Value"),
    startingPitchers.flatMap(p => pitcher2017Stats_FD.get(p).map(stats => (p, stats))).map {
      case (p, (seasonStats, deviationStats)) =>
        val value = mean(List(seasonStats.fptsPerGameAsStarter(FanDuelMLB))) / p.fanduel.map(_.salary).get
        (p, value)
    }.sortBy(_._2).reverse.map {
      case (p, value) =>
        List(p, p.fanduel.map(fd => "$" + fd.salary).getOrElse("Unknown"), p.opponent.get, (value * 1000).rounded(2))
    }))

}