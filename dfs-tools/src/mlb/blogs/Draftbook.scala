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
import mlb.Players._

object Draftbook extends App {

  Season2017Stats.logSummary

  def projectedAtBats(player: Player): Double = player.visitingOrHomeTeam match {
    case Some(vh) => vh match {
      case Visiting => player.battingPosition match {
        case Some(bp) => leagueAvgStatsByBattingPosition_VisitingTeam.get(bp).map(_.atBatsPerGame).getOrElse(0.0)
        case None     => 0.0
      }
      case Home => player.battingPosition match {
        case Some(bp) => leagueAvgStatsByBattingPosition_HomeTeam.get(bp).map(_.atBatsPerGame).getOrElse(0.0)
        case None     => 0.0
      }
    }
    case None => 0.0
  }

  val teamsOnSlate = startingPlayersByTeam.keys.toList

  val hitters_FD = startingHitters.filter(_.fanduel.map(_.salary).nonEmpty)
  val hitters_DK = startingHitters.filter(_.draftkings.map(_.salary).nonEmpty)

  val expensiveHitters_FD = hitters_FD.filter(_.fanduel.map(_.salary).get >= 3500)
  val midrangeHitters_FD = hitters_FD.filter(p => p.fanduel.map(_.salary).get < 3500 && p.fanduel.map(_.salary).get >= 2500)
  val cheapHitters_FD = hitters_FD.filter(p => p.fanduel.map(_.salary).get < 2500)

  val expensiveHitters_DK = hitters_DK.filter(_.draftkings.map(_.salary).get >= 4000)
  val midrangeHitters_DK = hitters_DK.filter(p => p.draftkings.map(_.salary).get < 4000 && p.draftkings.map(_.salary).get >= 3000)
  val cheapHitters_DK = hitters_DK.filter(p => p.draftkings.map(_.salary).get < 3000)

  teamsOnSlate.filter(t => !startingPitchers.map(_.team).contains(t)) match {
    case Nil      => // OK
    case notFound => println("\n\nWARNING: No starting pitcher found for " + notFound.mkString(", "))
  }

  val hitter2017Stats_FD: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allHitters.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val pitcher2017Stats_FD: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allPitchers.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val hitter2017Stats_DK: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allHitters.filter(_.player.draftkings.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val pitcher2017Stats_DK: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allPitchers.filter(_.player.draftkings.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  case class HitterStats(p: Player) {
    val projAtBats: Double = projectedAtBats(p)

    val opposingPitcher: Player = p.opposingPitcher(startingPitchers)

    val hitterTotalAtBats: Int = season.statsByPlayer.get(p.id).map(_.atBats).getOrElse(0)
    val pitcherTotalAtBats: Int = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
    }

    val hitterSeasonStatsFD: Option[PlayerSeasonStats] = hitter2017Stats_FD.get(p).map(_._1)
    val hitterDeviationStatsFD: Option[DeviationStats] = hitter2017Stats_FD.get(p).map(_._2)
    val hitterFptsPerAtBatFD: Option[Double] = hitterSeasonStatsFD.map(_.fptsPerAtBat(FanDuelMLB).toDouble)
    val hitterVsPitcherStatsFD: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, FanDuelMLB)
    val pitcherFptsPerAtBatAllowedFD: Option[Double] = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
    }
    val projFptsFD: Option[Double] = hitterFptsPerAtBatFD.map { fptsPerAB =>
      val hitterWeight = List(200, hitterTotalAtBats).min
      val pitcherWeight = List(200, pitcherTotalAtBats).min
      val hitterWeightedFptsPerAB = (0 to hitterWeight).toList.map(i => fptsPerAB)
      val pitcherWeightedFptsPerAB = (0 to pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedFD.get)
      val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
      mean(combinedWeightedFptsPerAB) * projAtBats
    }
    val projValueFD: Option[Double] = projFptsFD.map(projFpts => (projFpts / p.fanduel.map(_.salary).get) * 1000)

    println(s"$p - FanDuel - FPTS/PA: ${hitterFptsPerAtBatFD.map(_.rounded(2)).getOrElse("-")} in $hitterTotalAtBats PA, Pitcher FPTS/PA allowed: ${pitcherFptsPerAtBatAllowedFD.map(_.rounded(2)).getOrElse("-")} in $pitcherTotalAtBats PA, " +
      s"Projected FPTS: ${projFptsFD.map(_.rounded(2)).getOrElse("-")}, Projected Value: ${projValueFD.map(_.rounded(2)).getOrElse("-")}")

    val hitterSeasonStatsDK: Option[PlayerSeasonStats] = hitter2017Stats_DK.get(p).map(_._1)
    val hitterDeviationStatsDK: Option[DeviationStats] = hitter2017Stats_DK.get(p).map(_._2)
    val hitterFptsPerAtBatDK: Option[Double] = hitterSeasonStatsDK.map(_.fptsPerAtBat(DraftKingsMLB).toDouble)
    val hitterVsPitcherStatsDK: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, DraftKingsMLB)
    val pitcherFptsPerAtBatAllowedDK: Option[Double] = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
    }
    val projFptsDK: Option[Double] = hitterFptsPerAtBatDK.map { fptsPerAB =>
      val hitterWeight = List(200, hitterTotalAtBats).min
      val pitcherWeight = List(200, pitcherTotalAtBats).min
      val hitterWeightedFptsPerAB = (0 to hitterWeight).toList.map(i => fptsPerAB)
      val pitcherWeightedFptsPerAB = (0 to pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedDK.get)
      val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
      mean(combinedWeightedFptsPerAB) * projAtBats
    }
    val projValueDK: Option[Double] = projFptsDK.map(projFpts => (projFpts / p.draftkings.map(_.salary).get) * 1000)

    println(s"$p - DraftKings - FPTS/PA: ${hitterFptsPerAtBatDK.map(_.rounded(2)).getOrElse("-")} in $hitterTotalAtBats PA, Pitcher FPTS/PA allowed: ${pitcherFptsPerAtBatAllowedDK.map(_.rounded(2)).getOrElse("-")} in $pitcherTotalAtBats PA, " +
      s"Projected FPTS: ${projFptsDK.map(_.rounded(2)).getOrElse("-")}, Projected Value: ${projValueDK.map(_.rounded(2)).getOrElse("-")}")
  }

  val startingHitterStats: Map[Player, HitterStats] = startingHitters.map { p => (p, HitterStats(p)) }.toMap

  println("\n\nStarting pitchers: \n" + startingPitchers.sortBy(_.name).map { pitcher =>
    s"$pitcher [${pitcherStatsAllowedToAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("???")} FPTS/AB against (FanDuel), " +
      s"${pitcherStatsAllowedToAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("???")} FPTS/AB against (DraftKings)] vs: \n\t${
        startingHittersByTeam(pitcher.opponent.get).map { hitter =>
          val stats = startingHitterStats.get(hitter).get
          s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
            s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} projected FPTS & " +
            s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}, " +
            s"${stats.projFptsDK.map(_.rounded(2)).getOrElse("???")} projected FPTS & " +
            s"${stats.projValueDK.map(_.rounded(2)).getOrElse("???")} value on DK ${hitter.draftkings.map("($" + _.salary + ")").getOrElse("???")}, "
        }.mkString("\n\t")
      }"
  }.mkString("\n"))

  log("\n**************************************************")
  log("*** Hitter stacks ***")
  log("**************************************************\n")

  log("\n### Pitchers ranked by FPTS given up per plate appearance: ###\n")
  log(toHtmlTable(
    List("Pitcher", "Opponent", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
    startingPitchers
      .sortBy { p => pitcherStatsAllowedToAllHitters.get(p).map(_.fptsPerAtBatAgainst_FD).getOrElse(0.0d) }.reverse
      .map { pitcher =>
        val statsAgainst = pitcherStatsAllowedToAllHitters.get(pitcher)
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
        List(pitcherStatsAllowedToLefties.get(pitcher), pitcherStatsAllowedToRighties.get(pitcher), pitcherStatsAllowedToSwitchHitters.get(pitcher)).flatten
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
    expensiveHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projValueFD.get.rounded(2))
      }))

  log("\n### Top 10 mid-range hitters ranked by value (FanDuel): ###\n")
  log(toHtmlTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Value"),
    midrangeHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projValueFD.get.rounded(2))
      }))

  log("\n### Top 10 cheap hitters ranked by value (FanDuel): ###\n")
  log(toHtmlTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Value"),
    cheapHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projValueFD.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Value hitters - DK ***")
  log("**************************************************\n")

  log("\n### Top 10 expensive hitters ranked by value (DraftKings): ###\n")
  log(toHtmlTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Value"),
    expensiveHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projValueDK.get.rounded(2))
      }))

  log("\n### Top 10 mid-range hitters ranked by value (DraftKings): ###\n")
  log(toHtmlTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Value"),
    midrangeHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projValueDK.get.rounded(2))
      }))

  log("\n### Top 10 cheap hitters ranked by value (DraftKings): ###\n")
  log(toHtmlTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Value"),
    cheapHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projValueDK.get.rounded(2))
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