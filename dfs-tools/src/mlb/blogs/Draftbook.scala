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
import mlb.Players._

object Draftbook extends App {

  import mlb.Season2017Stats.stats._
  mlb.Season2017Stats.stats.logSummary

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

  case class PitcherStats(p: Player) {
    val opposingHitters: List[Player] = p.opposingHitters

    val pitcherAtBatsPerStart: Option[Int] = season.statsByPlayer.get(p.id).map(_.gamesStarted) match {
      case Some(gamesStarted) =>
        if (gamesStarted.nonEmpty)
          Some(gamesStarted.map(_.asInstanceOf[PitcherGameStats].pitchingStats.atBats).sum / gamesStarted.length)
        else None
      case None => None
    }

    val ballparkFactor: Double = p.visitingOrHomeTeam match {
      case Some(vOrH) =>
        if (vOrH == Home) pitcherBallparkFactor_HomeTeam(p.team).forPitcher(p)
        else pitcherBallparkFactor_VisitingTeam(p.opponent.get).forPitcher(p)
      case None => 1.0
    }

    val pitcherFullSeasonStats_FD: Option[PlayerSeasonStats] = pitcher2017Stats_FD.get(p).map(_._1)
    val pitcherBvpStats_FD: Option[BatterVsPitcherStats] = season.pitcherFptsPerAB_vs_Hitters(p, opposingHitters, FanDuelMLB)
    val projFptsFD: Option[Double] = pitcherAtBatsPerStart match {
      case Some(abPerStart) => {
        val fullSeasonAtBats = pitcherFullSeasonStats_FD.map(_.games.collect { case pgs: PitcherGameStats => pgs }
          .map(_.pitchingStats.atBats))
          .map(_.sum).getOrElse(0)
        val fullSeasonFptsPerAB = pitcherFullSeasonStats_FD.map(_.games.collect { case pgs: PitcherGameStats => pgs }
          .map(_.pitchingStats.fantasyPoints(FanDuelMLB).toDouble))
          .map(_.sum / fullSeasonAtBats).getOrElse(0.0)
        val fullSeasonWeight = List(200, fullSeasonAtBats).min
        val bvpWeight = List(200, pitcherBvpStats_FD.map(_.atBats).getOrElse(0)).min
        val fullSeasonWeightedFptsPerAB = (0 to fullSeasonWeight).toList.map(i => fullSeasonFptsPerAB * ballparkFactor)
        val bvpWeightedFptsPerAB = (0 to bvpWeight).toList.map(i => pitcherBvpStats_FD.map(_.fptsPerAB).getOrElse(0.0)) // should park factor apply to BvP?
        val combinedWeightedFptsPerAB = fullSeasonWeightedFptsPerAB ++ bvpWeightedFptsPerAB

        //        println(s"$p - FanDuel - fullSeason AB: $fullSeasonAtBats, fullSeason FPTS/AB: ${fullSeasonFptsPerAB.rounded(2)}, fullSeasonWeight: $fullSeasonWeight \n\t" +
        //          s"bvp AB: ${pitcherBvpStats_FD.map(_.atBats).getOrElse(0)}, bvp FPTS/AB: ${pitcherBvpStats_FD.map(_.fptsPerAB).map(_.rounded(2)).getOrElse(0.0)}, bvpWeight: $bvpWeight \n\t" +
        //          s"Projected FPTS = ${mean(combinedWeightedFptsPerAB).rounded(2)} FPTS/AB * $abPerStart projected AB = ${mean(combinedWeightedFptsPerAB) * abPerStart}")

        Some(mean(combinedWeightedFptsPerAB) * abPerStart)
      }
      case _ => None
    }
    val projValueFD: Option[Double] = (p.fanduel.map(_.salary), projFptsFD) match {
      case (Some(salary), Some(fpts)) => Some((fpts / salary) * 1000)
      case _                          => None
    }

    val pitcherFullSeasonStats_DK: Option[PlayerSeasonStats] = pitcher2017Stats_DK.get(p).map(_._1)
    val pitcherBvpStats_DK: Option[BatterVsPitcherStats] = season.pitcherFptsPerAB_vs_Hitters(p, opposingHitters, DraftKingsMLB)
    val projFptsDK: Option[Double] = pitcherAtBatsPerStart match {
      case Some(abPerStart) => {
        val fullSeasonAtBats = pitcherFullSeasonStats_DK.map(_.games.collect { case pgs: PitcherGameStats => pgs }
          .map(_.pitchingStats.atBats))
          .map(_.sum).getOrElse(0)
        val fullSeasonFptsPerAB = pitcherFullSeasonStats_DK.map(_.games.collect { case pgs: PitcherGameStats => pgs }
          .map(_.pitchingStats.fantasyPoints(DraftKingsMLB).toDouble))
          .map(_.sum / fullSeasonAtBats).getOrElse(0.0)
        val fullSeasonWeight = List(200, fullSeasonAtBats).min
        val bvpWeight = List(200, pitcherBvpStats_DK.map(_.atBats).getOrElse(0)).min
        val fullSeasonWeightedFptsPerAB = (0 to fullSeasonWeight).toList.map(i => fullSeasonFptsPerAB * ballparkFactor)
        val bvpWeightedFptsPerAB = (0 to bvpWeight).toList.map(i => pitcherBvpStats_DK.map(_.fptsPerAB).getOrElse(0.0)) // should park factor apply to BvP?
        val combinedWeightedFptsPerAB = fullSeasonWeightedFptsPerAB ++ bvpWeightedFptsPerAB
        Some(mean(combinedWeightedFptsPerAB) * abPerStart)
      }
      case _ => None
    }
    val projValueDK: Option[Double] = (p.draftkings.map(_.salary), projFptsDK) match {
      case (Some(salary), Some(fpts)) => Some((fpts / salary) * 1000)
      case _                          => None
    }

  }

  case class HitterStats(p: Player) {
    val projAtBats: Double = projectedAtBats(p)

    val opposingPitcher: Player = p.opposingPitcher

    val hitterTotalAtBats: Int = season.hitterStatsAgainstPitcherType(opposingPitcher.throws, p).map(_.atBats).sum
    val pitcherTotalAtBats: Int = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
    }

    val ballparkFactor: Double = p.visitingOrHomeTeam match {
      case Some(vOrH) =>
        if (vOrH == Home) hitterBallparkFactor_HomeTeam(p.team).forHitter(p)
        else hitterBallparkFactor_VisitingTeam(p.opponent.get).forHitter(p)
      case None => 1.0
    }

    val hitterSeasonStatsFD: Option[PlayerSeasonStats] = hitter2017Stats_FD.get(p).map(_._1)
    val hitterDeviationStatsFD: Option[DeviationStats] = hitter2017Stats_FD.get(p).map(_._2)
    val hitterFptsPerAtBatFD: Option[Double] = season.hitterFptsPerAB_vs_PitcherType(opposingPitcher.throws, p, FanDuelMLB).map(_.fptsPerAB)
    val hitterVsPitcherStatsFD: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, FanDuelMLB)
    val pitcherFptsPerAtBatAllowedFD: Option[Double] = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
    }
    val projFptsFD: Option[Double] = hitterFptsPerAtBatFD.map { fptsPerAB =>
      val hitterWeight = List(200, hitterTotalAtBats).min
      val pitcherWeight = List(200, pitcherTotalAtBats).min
      val hitterWeightedFptsPerAB = (0 to hitterWeight).toList.map(i => fptsPerAB * ballparkFactor)
      val pitcherWeightedFptsPerAB = (0 to pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedFD.get) // should park factor apply to pitcher?
      val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
      mean(combinedWeightedFptsPerAB) * projAtBats
    }
    val projValueFD: Option[Double] = p.fanduel.map(_.salary).map(salary => (projFptsFD.getOrElse(0.0) / salary) * 1000)

    //        println(s"$p - FanDuel - FPTS/PA: ${hitterFptsPerAtBatFD.map(_.rounded(2)).getOrElse("-")} in $hitterTotalAtBats PA, Pitcher FPTS/PA allowed: ${pitcherFptsPerAtBatAllowedFD.map(_.rounded(2)).getOrElse("-")} in $pitcherTotalAtBats PA, " +
    //          s"Projected FPTS: ${projFptsFD.map(_.rounded(2)).getOrElse("-")}, Projected Value: ${projValueFD.map(_.rounded(2)).getOrElse("-")}")

    val hitterSeasonStatsDK: Option[PlayerSeasonStats] = hitter2017Stats_DK.get(p).map(_._1)
    val hitterDeviationStatsDK: Option[DeviationStats] = hitter2017Stats_DK.get(p).map(_._2)
    val hitterFptsPerAtBatDK: Option[Double] = season.hitterFptsPerAB_vs_PitcherType(opposingPitcher.throws, p, DraftKingsMLB).map(_.fptsPerAB)
    val hitterVsPitcherStatsDK: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, DraftKingsMLB)
    val pitcherFptsPerAtBatAllowedDK: Option[Double] = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
    }
    val projFptsDK: Option[Double] = hitterFptsPerAtBatDK.map { fptsPerAB =>
      val hitterWeight = List(200, hitterTotalAtBats).min
      val pitcherWeight = List(200, pitcherTotalAtBats).min
      val hitterWeightedFptsPerAB = (0 to hitterWeight).toList.map(i => fptsPerAB * ballparkFactor)
      val pitcherWeightedFptsPerAB = (0 to pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedDK.get) // should park factor apply to pitcher?
      val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
      mean(combinedWeightedFptsPerAB) * projAtBats
    }
    val projValueDK: Option[Double] = p.draftkings.map(_.salary).map(salary => (projFptsDK.getOrElse(0.0) / salary) * 1000)

    //    println(s"$p - DraftKings - FPTS/PA: ${hitterFptsPerAtBatDK.map(_.rounded(2)).getOrElse("-")} in $hitterTotalAtBats PA, Pitcher FPTS/PA allowed: ${pitcherFptsPerAtBatAllowedDK.map(_.rounded(2)).getOrElse("-")} in $pitcherTotalAtBats PA, " +
    //      s"Projected FPTS: ${projFptsDK.map(_.rounded(2)).getOrElse("-")}, Projected Value: ${projValueDK.map(_.rounded(2)).getOrElse("-")}")
  }

  val startingHitterStats: Map[Player, HitterStats] = startingHitters.filter(season.hasStatsFor(_)).map { p => (p, HitterStats(p)) }.toMap

  val startingPitcherStats: Map[Player, PitcherStats] = startingPitchers.filter(season.hasStatsFor(_)).map { p => (p, PitcherStats(p)) }.toMap

  println("\n\nStarting pitchers: \n" + startingPitchers.sortBy(_.name).map { pitcher =>
    s"$pitcher [${pitcherStatsAllowedToAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("???")} FPTS/AB against (FanDuel), " +
      s"${pitcherStatsAllowedToAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("???")} FPTS/AB against (DraftKings)] vs: \n\t${
        startingHittersByTeam(pitcher.opponent.get).map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}, " +
                s"${stats.projFptsDK.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueDK.map(_.rounded(2)).getOrElse("???")} value on DK ${hitter.draftkings.map("($" + _.salary + ")").getOrElse("???")} "
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }

        }.mkString("\n\t")
      }"
  }.mkString("\n"))

  log("\n**************************************************")
  log("*** Hitter stacks ***")
  log("**************************************************\n")

  log("\n### Pitchers ranked by FPTS given up per plate appearance: ###\n")
  log(toTable(
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

  log("\n### Top 10 pitchers ranked by FPTS given up per plate appearance by batter handedness (Minimum 30 PA): ###\n")
  log(toTable(
    List("Pitcher", "Opponent", "Against hitters who bat...", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
    startingPitchers
      .flatMap { pitcher =>
        List(pitcherStatsAllowedToLefties.get(pitcher), pitcherStatsAllowedToRighties.get(pitcher), pitcherStatsAllowedToSwitchHitters.get(pitcher)).flatten
      }
      .sortBy(_.fptsPerAtBatAgainst_FD).reverse
      .filter(_.atBatsAgainst >= 30)
      .take(15)
      .map { stats =>
        List(stats.pitcher,
          stats.pitcher.opponent.get,
          stats.batterHandedness.get.toVerboseString,
          stats.fptsPerAtBatAgainst_FD.rounded(1),
          stats.fptsPerAtBatAgainst_DK.rounded(1),
          stats.atBatsAgainst)
      }))

  log("\n### Top 4-hitter stacks by projected value (FanDuel): ###\n")
  teamsOnSlate.map { team =>
    val stack = startingHittersByTeam(team).sortBy { h => startingHitterStats.get(h).flatMap(_.projValueFD).getOrElse(0.0) }.reverse.take(4)
      .sortBy(_.battingPosition.getOrElse(10))
    val avgValue = mean(stack.map { h => startingHitterStats.get(h).flatMap(_.projValueFD).getOrElse(0.0) })
    (stack -> avgValue)
  }.sortBy(_._2).reverse.take(5).map {
    case (stack, avgValue) =>
      val totalSalary = stack.flatMap { hitter => hitter.fanduel.map(_.salary) }.sum
      val totalFPTS = stack.map { hitter => startingHitterStats.get(hitter).flatMap(_.projFptsFD).getOrElse(0.0) }.sum
      s"${stack.head.team} vs ${stack.head.opposingPitcher} - FPTS: ${totalFPTS.rounded(2)}, Value: ${((totalFPTS / totalSalary) * 1000).rounded(2)}\n\t" +
        stack.map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}, " +
                s"${stats.projFptsDK.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueDK.map(_.rounded(2)).getOrElse("???")} value on DK ${hitter.draftkings.map("($" + _.salary + ")").getOrElse("???")} "
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }
        }.mkString("\n\t")
  }.foreach(log(_))

  log("\n### Top 3-hitter stacks by projected value (FanDuel) --- only includes batting positions 1-7: ###\n")
  teamsOnSlate.map { team =>
    val stack = startingHittersByTeam(team)
      .filter(_.battingPosition.getOrElse(10) <= 7)
      .sortBy { h => startingHitterStats.get(h).flatMap(_.projValueFD).getOrElse(0.0) }.reverse.take(3)
      .sortBy(_.battingPosition.getOrElse(10))
    val avgValue = mean(stack.map { h => startingHitterStats.get(h).flatMap(_.projValueFD).getOrElse(0.0) })
    (stack -> avgValue)
  }.sortBy(_._2).reverse.take(5).map {
    case (stack, avgValue) =>
      val totalSalary = stack.flatMap { hitter => hitter.fanduel.map(_.salary) }.sum
      val totalFPTS = stack.map { hitter => startingHitterStats.get(hitter).flatMap(_.projFptsFD).getOrElse(0.0) }.sum
      s"${stack.head.team} vs ${stack.head.opposingPitcher} - FPTS: ${totalFPTS.rounded(2)}, Value: ${((totalFPTS / totalSalary) * 1000).rounded(2)}\n\t" +
        stack.map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}, " +
                s"${stats.projFptsDK.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueDK.map(_.rounded(2)).getOrElse("???")} value on DK ${hitter.draftkings.map("($" + _.salary + ")").getOrElse("???")} "
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }
        }.mkString("\n\t")
  }.foreach(log(_))

  log("\n**************************************************")
  log("*** Value hitters - FD ***")
  log("**************************************************\n")

  log("\n### Top 10 expensive hitters ranked by value (FanDuel): ###\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    expensiveHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\n### Top 10 mid-range hitters ranked by value (FanDuel): ###\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    midrangeHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\n### Top 10 cheap hitters ranked by value (FanDuel): ###\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    cheapHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Value hitters - DK ***")
  log("**************************************************\n")

  log("\n### Top 10 expensive hitters ranked by value (DraftKings): ###\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    expensiveHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\n### Top 10 mid-range hitters ranked by value (DraftKings): ###\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    midrangeHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\n### Top 10 cheap hitters ranked by value (DraftKings): ###\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    cheapHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Top projected hitters - FD ***")
  log("**************************************************\n")

  log("\n### Top 10 hitters ranked by projected FPTS (FanDuel): ###\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    hitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projFptsFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projFptsFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Pitchers ***")
  log("**************************************************\n")

  log("\n### Starting pitchers ranked by value (FanDuel): ###\n")
  log(toTable(
    List("Pitcher", "FD Salary", "Opponent", "Sample Size (BvP PA)", "Projected FPTS", "Value"),
    startingPitcherStats.toList
      .filter { case (p, stats) => p.fanduel.nonEmpty && season.hasStatsFor(p) }
      .sortBy(_._2.projValueFD.getOrElse(0.0)).reverse
      .map {
        case (p, stats) =>
          List(p, p.fanduel.map(fd => "$" + fd.salary).get, p.opponent.get, stats.pitcherBvpStats_FD.map(_.atBats).getOrElse(0),
            stats.projFptsFD.map(_.rounded(2)).getOrElse("???"), stats.projValueFD.map(_.rounded(2)).getOrElse("???"))
      }))

  log("\n### Starting pitchers ranked by value (DraftKings): ###\n")
  log(toTable(
    List("Pitcher", "DK Salary", "Opponent", "Sample Size (BvP PA)", "Projected FPTS", "Value"),
    startingPitcherStats.toList
      .filter { case (p, stats) => p.draftkings.nonEmpty && season.hasStatsFor(p) }
      .sortBy(_._2.projValueDK.getOrElse(0.0)).reverse
      .map {
        case (p, stats) =>
          List(p, p.draftkings.map(dk => "$" + dk.salary).get, p.opponent.get, stats.pitcherBvpStats_DK.map(_.atBats).getOrElse(0),
            stats.projFptsDK.map(_.rounded(2)).getOrElse("???"), stats.projValueDK.map(_.rounded(2)).getOrElse("???"))
      }))

}