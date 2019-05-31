package mlb

import mlb._
import mlb.model._
import mlb.model.CustomTypes._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._
import utils.DateTimeUtils._

case class HistoricalStats(season: Season) {

  val hitterLeagueAvgPointsPerGameStarted_FD = mean(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints(FanDuelMLB)))
  val hitterLeaguePointsPerGameStartedStdDev_FD = stdDev(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints(FanDuelMLB)))
  val hitterLeagueAvgPointsPerAtBat_FD = season.allHitters.flatMap(_.games).map(_.fantasyPoints(FanDuelMLB).toDouble).sum / season.allHitters.flatMap(_.games).map(_.hittingStats.atBats).sum

  val hitterLeagueAvgPointsPerGameStarted_DK = mean(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints(DraftKingsMLB)))
  val hitterLeaguePointsPerGameStartedStdDev_DK = stdDev(season.allHitters.flatMap(_.gamesStarted).map(_.fantasyPoints(DraftKingsMLB)))
  val hitterLeagueAvgPointsPerAtBat_DK = season.allHitters.flatMap(_.games).map(_.fantasyPoints(DraftKingsMLB).toDouble).sum / season.allHitters.flatMap(_.games).map(_.hittingStats.atBats).sum

  val pitcherLeagueAvgPointsPerGameStarted_FD = mean(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints(FanDuelMLB)))
  val pitcherLeaguePointsPerGameStartedStdDev_FD = stdDev(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints(FanDuelMLB)))

  val pitcherLeagueAvgPointsPerGameStarted_DK = mean(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints(DraftKingsMLB)))
  val pitcherLeaguePointsPerGameStartedStdDev_DK = stdDev(season.allPitchers.flatMap(_.gamesStarted).map(_.fantasyPoints(DraftKingsMLB)))

  val pitcherStatsAllowedToAllHitters: Map[Player, HittingStatsAllowed] = {
    season.allPitchers.map(_.player).map { pitcher =>
      val gamesPitched = season.games.flatMap(_.statsFor(pitcher)).flatMap {
        _ match {
          case pitcherStats: PitcherGameStats => Some(pitcherStats)
          case hitterStats: HitterGameStats   => None
        }
      }

      if (Configs.overweightRecent && Configs.recentDaysToWeightHigher > 0) {
        val (latestGames, recentGames, oldGames) = {
          val (notOld, old) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHigher)))
          val (latest, recent) = notOld.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHighest)))
          (latest, recent, old)
        }

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
        val weightedAtBatsAgainst = latestGames.map(_.atBatsAgainst() * Configs.highestWeight).sum + recentGames.map(_.atBatsAgainst() * Configs.higherWeight).sum + oldGames.map(_.atBatsAgainst()).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val weightedFptsAgainst_FD = latestGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val weightedFptsAgainst_DK = latestGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val fptsPerAtBatAgainst_DK = weightedFptsAgainst_DK / weightedAtBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, None))
      } else {
        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, None))
      }
    }.toMap
  }

  val pitcherStatsAllowedToLefties: Map[Player, HittingStatsAllowed] = {
    season.allPitchers.map(_.player).map { pitcher =>
      val gamesPitched = season.games.flatMap(_.statsFor(pitcher)).flatMap {
        _ match {
          case pitcherStats: PitcherGameStats => Some(pitcherStats)
          case hitterStats: HitterGameStats   => None
        }
      }

      if (Configs.overweightRecent && Configs.recentDaysToWeightHigher > 0) {
        val (latestGames, recentGames, oldGames) = {
          val (notOld, old) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHigher)))
          val (latest, recent) = notOld.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHighest)))
          (latest, recent, old)
        }

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Left))).sum
        val weightedAtBatsAgainst = latestGames.map(_.atBatsAgainst(Some(Left)) * Configs.highestWeight).sum + recentGames.map(_.atBatsAgainst(Some(Left)) * Configs.higherWeight).sum + oldGames.map(_.atBatsAgainst(Some(Left))).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble).sum
        val weightedFptsAgainst_FD = latestGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble).sum
        val weightedFptsAgainst_DK = latestGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble).sum
        val fptsPerAtBatAgainst_DK = weightedFptsAgainst_DK / weightedAtBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Left)))
      } else {
        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Left))).sum
        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble).sum
        val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble).sum
        val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Left)))
      }
    }.toMap
  }

  val pitcherStatsAllowedToRighties: Map[Player, HittingStatsAllowed] = {
    season.allPitchers.map(_.player).map { pitcher =>
      val gamesPitched = season.games.flatMap(_.statsFor(pitcher)).flatMap {
        _ match {
          case pitcherStats: PitcherGameStats => Some(pitcherStats)
          case hitterStats: HitterGameStats   => None
        }
      }

      if (Configs.overweightRecent && Configs.recentDaysToWeightHigher > 0) {
        val (latestGames, recentGames, oldGames) = {
          val (notOld, old) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHigher)))
          val (latest, recent) = notOld.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHighest)))
          (latest, recent, old)
        }

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Right))).sum
        val weightedAtBatsAgainst = latestGames.map(_.atBatsAgainst(Some(Right)) * Configs.highestWeight).sum + recentGames.map(_.atBatsAgainst(Some(Right)) * Configs.higherWeight).sum + oldGames.map(_.atBatsAgainst(Some(Right))).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble).sum
        val weightedFptsAgainst_FD = latestGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble).sum
        val weightedFptsAgainst_DK = latestGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble).sum
        val fptsPerAtBatAgainst_DK = weightedFptsAgainst_DK / weightedAtBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Right)))
      } else {
        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Right))).sum
        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble).sum
        val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble).sum
        val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Right)))
      }
    }.toMap
  }

  val pitcherStatsAllowedToSwitchHitters: Map[Player, HittingStatsAllowed] = {
    season.allPitchers.map(_.player).map { pitcher =>
      val gamesPitched = season.games.flatMap(_.statsFor(pitcher)).flatMap {
        _ match {
          case pitcherStats: PitcherGameStats => Some(pitcherStats)
          case hitterStats: HitterGameStats   => None
        }
      }

      if (Configs.overweightRecent && Configs.recentDaysToWeightHigher > 0) {
        val (latestGames, recentGames, oldGames) = {
          val (notOld, old) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHigher)))
          val (latest, recent) = notOld.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHighest)))
          (latest, recent, old)
        }

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Switch))).sum
        val weightedAtBatsAgainst = latestGames.map(_.atBatsAgainst(Some(Switch)) * Configs.highestWeight).sum + recentGames.map(_.atBatsAgainst(Some(Switch)) * Configs.higherWeight).sum + oldGames.map(_.atBatsAgainst(Some(Switch))).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble).sum
        val weightedFptsAgainst_FD = latestGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble).sum
        val weightedFptsAgainst_DK = latestGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble).sum
        val fptsPerAtBatAgainst_DK = weightedFptsAgainst_DK / weightedAtBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Switch)))
      } else {
        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Switch))).sum
        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble).sum
        val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble).sum
        val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
        (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Switch)))
      }
    }.toMap
  }

  val bullpenStatsAllowedToAllHitters: Map[Team, HittingStatsAllowed] = {
    Teams.allTeams.map { team =>
      val gamesPitched = season.reliefPitchingStatsByTeam(team)

      if (Configs.overweightRecent && Configs.recentDaysToWeightHigher > 0) {
        val (latestGames, recentGames, oldGames) = {
          val (notOld, old) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHigher)))
          val (latest, recent) = notOld.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHighest)))
          (latest, recent, old)
        }

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
        val weightedAtBatsAgainst = latestGames.map(_.atBatsAgainst() * Configs.highestWeight).sum + recentGames.map(_.atBatsAgainst() * Configs.higherWeight).sum + oldGames.map(_.atBatsAgainst()).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val weightedFptsAgainst_FD = latestGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val weightedFptsAgainst_DK = latestGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble * Configs.highestWeight).sum + recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble * Configs.higherWeight).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val fptsPerAtBatAgainst_DK = weightedFptsAgainst_DK / weightedAtBatsAgainst
        (team -> HittingStatsAllowed(null, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, None))
      } else {
        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
        (team -> HittingStatsAllowed(null, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, None))
      }
    }.toMap
  }
  //  log(s"bullpenStatsAllowedToAllHitters:\n\t${
  //    bullpenStatsAllowedToAllHitters.map {
  //      case (t, s) =>
  //        s"$t - PA: ${s.atBatsAgainst}, FPTS/PA (FD): ${s.fptsPerAtBatAgainst_FD.rounded(2)}, FPTS/PA (DK): ${s.fptsPerAtBatAgainst_DK.rounded(2)}"
  //    }.mkString("\n\t")
  //  }")

  val numberOfGames = season.games.length

  val leagueAvgStatsByBattingPosition: Map[BattingPosition, BattingPositionStats] = season.allPlayers.flatMap(_.games).groupBy(_.battingPosition).map {
    case (bp, games) =>
      val totalAtBats = games.map(_.hittingStats.atBats).sum
      val totalFpts = games.map(_.hittingStats.fantasyPoints().toDouble).sum
      (bp ->
        BattingPositionStats(numberOfGames * 2, totalAtBats,
          totalAtBats.toDouble / (numberOfGames * 2),
          totalFpts / totalAtBats,
          totalFpts / (numberOfGames * 2)))
  }

  val leagueAvgStatsByBattingPosition_VisitingTeam: Map[BattingPosition, BattingPositionStats] = season.allPlayers.flatMap(_.games)
    .filter(pgs => pgs.game.get.visitingTeamPlayerStats.contains(pgs))
    .groupBy(_.battingPosition).map {
      case (bp, games) =>
        val totalAtBats = games.map(_.hittingStats.atBats).sum
        val totalFpts = games.map(_.hittingStats.fantasyPoints().toDouble).sum
        (bp ->
          BattingPositionStats(numberOfGames, totalAtBats,
            totalAtBats.toDouble / numberOfGames,
            totalFpts / totalAtBats,
            totalFpts / numberOfGames))
    }

  val leagueAvgStatsByBattingPosition_HomeTeam: Map[BattingPosition, BattingPositionStats] = season.allPlayers.flatMap(_.games)
    .filter(pgs => pgs.game.get.homeTeamPlayerStats.contains(pgs))
    .groupBy(_.battingPosition).map {
      case (bp, games) =>
        val totalAtBats = games.map(_.hittingStats.atBats).sum
        val totalFpts = games.map(_.hittingStats.fantasyPoints().toDouble).sum
        (bp ->
          BattingPositionStats(numberOfGames, totalAtBats,
            totalAtBats.toDouble / numberOfGames,
            totalFpts / totalAtBats,
            totalFpts / numberOfGames))
    }

  val leagueAvgStatsByBattingPositionAndOpposingPitcher_VisitingTeam: Map[(BattingPosition, Player), BattingPositionStats] = Players.startingPitchers.flatMap {
    pitcher =>
      val games = season.gamesWithStartingPitcher(pitcher).filter(_.homeTeamStartingPitcher == pitcher)
      games.flatMap(_.visitingTeamPlayerStats).groupBy(_.battingPosition).map {
        case (bp, pgs) =>
          val totalAtBats = pgs.map(_.hittingStats.atBats).sum
          val totalFpts = pgs.map(_.hittingStats.fantasyPoints().toDouble).sum
          ((bp, pitcher) ->
            BattingPositionStats(games.length, totalAtBats,
              totalAtBats.toDouble / games.length,
              totalFpts / totalAtBats,
              totalFpts / games.length))
      }
  }.toMap

  val leagueAvgStatsByBattingPositionAndOpposingPitcher_HomeTeam: Map[(BattingPosition, Player), BattingPositionStats] = Players.startingPitchers.flatMap {
    pitcher =>
      val games = season.gamesWithStartingPitcher(pitcher).filter(_.visitingTeamStartingPitcher == pitcher)
      games.flatMap(_.homeTeamPlayerStats).groupBy(_.battingPosition).map {
        case (bp, pgs) =>
          val totalAtBats = pgs.map(_.hittingStats.atBats).sum
          val totalFpts = pgs.map(_.hittingStats.fantasyPoints().toDouble).sum
          ((bp, pitcher) ->
            BattingPositionStats(games.length, totalAtBats,
              totalAtBats.toDouble / games.length,
              totalFpts / totalAtBats,
              totalFpts / games.length))
      }
  }.toMap

  //  log(s"### PA per batting position vs pitcher - Visiting Team: ###\n\t" + leagueAvgStatsByBattingPositionAndOpposingPitcher_VisitingTeam.toList
  //    .sortBy { case ((bp, pitcher), stats) => s"${pitcher.id}$bp" }
  //    .map { case ((bp, pitcher), stats) => s"${bp}) vs $pitcher - ${stats.atBatsPerGame} (${stats.numberOfGames} games)" }.mkString("\n\t"))
  //  
  //  log(s"### PA per batting position vs pitcher - Home Team: ###\n\t" + leagueAvgStatsByBattingPositionAndOpposingPitcher_HomeTeam.toList
  //    .sortBy { case ((bp, pitcher), stats) => s"${pitcher.id}$bp" }
  //    .map { case ((bp, pitcher), stats) => s"${bp}) vs $pitcher - ${stats.atBatsPerGame} (${stats.numberOfGames} games)" }.mkString("\n\t"))

  case class BallparkStats(homeTeam: Team, totalAtBats_Visitors: Int, totalFpts_Visitors: Double, totalAtBats_Home: Int, totalFpts_Home: Double,
                           pitchingFptsPerGame_Visitors: Double, pitchingFptsPerGame_Home: Double) {
    val fptsPerAtBat_Visitors: Double = totalFpts_Visitors / totalAtBats_Visitors
    val fptsPerAtBat_Home: Double = totalFpts_Home / totalAtBats_Home
  }

  // key is home team, which is a proxy for ballpark
  val leagueAvgStatsByBallpark: Map[Team, BallparkStats] = season.games.groupBy(_.homeTeam).map {
    case (homeTeam, games) =>
      val visitorStats = games.flatMap(_.visitingTeamPlayerStats)
      val homeStats = games.flatMap(_.homeTeamPlayerStats)
      val ballparkStats = BallparkStats(homeTeam,
        visitorStats.filter(_.isInstanceOf[HitterGameStats]).map(_.hittingStats.atBats).sum, visitorStats.filter(_.isInstanceOf[HitterGameStats]).map(_.fantasyPoints()).sum,
        homeStats.filter(_.isInstanceOf[HitterGameStats]).map(_.hittingStats.atBats).sum, homeStats.filter(_.isInstanceOf[HitterGameStats]).map(_.fantasyPoints()).sum,
        visitorStats.filter(_.isInstanceOf[PitcherGameStats]).map(_.fantasyPoints()).sum / games.length,
        homeStats.filter(_.isInstanceOf[PitcherGameStats]).map(_.fantasyPoints()).sum / games.length)
      (homeTeam, ballparkStats)
  }

  // FPTS/PA projection for visiting team hitters should be multiplied by this number --- key is home team, which is a proxy for ballpark
  val hitterBallparkFactor_VisitingTeam: Map[Team, HitterBallparkFactor] = season.games.groupBy(_.homeTeam).map {
    case (team, homeGames) =>
      val hittingStatsInThisPark = homeGames.flatMap(_.visitingTeamPlayerStats).filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
      val hittingStatsInThisPark_Lefties = hittingStatsInThisPark.filter(_.player.bats == Left)
      val hittingStatsInThisPark_Switch = hittingStatsInThisPark.filter(_.player.bats == Switch)
      val hittingStatsInThisPark_Righties = hittingStatsInThisPark.filter(_.player.bats == Right)
      val thisParkFptsPerABForLefties = hittingStatsInThisPark_Lefties.map(_.fantasyPoints().toDouble).sum / hittingStatsInThisPark_Lefties.map(_.hittingStats.atBats).sum
      val thisParkFptsPerABForSwitch = hittingStatsInThisPark_Switch.map(_.fantasyPoints().toDouble).sum / hittingStatsInThisPark_Switch.map(_.hittingStats.atBats).sum
      val thisParkFptsPerABForRighties = hittingStatsInThisPark_Righties.map(_.fantasyPoints().toDouble).sum / hittingStatsInThisPark_Righties.map(_.hittingStats.atBats).sum

      //println(s"Visiting @ $team - ${homeGames.length} home games - PA: L: ${hittingStatsInThisPark_Lefties.map(_.hittingStats.atBats).sum}, S: ${hittingStatsInThisPark_Switch.map(_.hittingStats.atBats).sum}, R: ${hittingStatsInThisPark_Righties.map(_.hittingStats.atBats).sum} ")

      val allGames = season.games.filter(_.involvesTeam(team))
      val hittingStatsInAllParks = allGames.flatMap { game =>
        if (game.isHomeGameFor(team)) game.visitingTeamPlayerStats.filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
        else if (game.visitingTeam == team) game.homeTeamPlayerStats.filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
        else throw new Exception("Something unexpected happened!")
      }
      val hittingStatsInAllParks_Lefties = hittingStatsInAllParks.filter(_.player.bats == Left)
      val hittingStatsInAllParks_Switch = hittingStatsInAllParks.filter(_.player.bats == Switch)
      val hittingStatsInAllParks_Righties = hittingStatsInAllParks.filter(_.player.bats == Right)
      val allParksFptsPerABForLefties = hittingStatsInAllParks_Lefties.map(_.fantasyPoints().toDouble).sum / hittingStatsInAllParks_Lefties.map(_.hittingStats.atBats).sum
      val allParksFptsPerABForSwitch = hittingStatsInAllParks_Switch.map(_.fantasyPoints().toDouble).sum / hittingStatsInAllParks_Switch.map(_.hittingStats.atBats).sum
      val allParksFptsPerABForRighties = hittingStatsInAllParks_Righties.map(_.fantasyPoints().toDouble).sum / hittingStatsInAllParks_Righties.map(_.hittingStats.atBats).sum

      val leftyFactor = thisParkFptsPerABForLefties / allParksFptsPerABForLefties
      val rightyFactor = thisParkFptsPerABForRighties / allParksFptsPerABForRighties
      val switchFactor = {
        if (hittingStatsInThisPark_Switch.map(_.hittingStats.atBats).sum > 300) thisParkFptsPerABForSwitch / allParksFptsPerABForSwitch
        else List(leftyFactor, rightyFactor).max
      }

      (team, HitterBallparkFactor(leftyFactor, switchFactor, rightyFactor))
  }

  // FPTS/PA projection for home team hitters should be multiplied by this number --- key is home team, which is a proxy for ballpark
  val hitterBallparkFactor_HomeTeam: Map[Team, HitterBallparkFactor] = season.games.groupBy(_.homeTeam).map {
    case (team, homeGames) =>
      val hittingStatsInThisPark = homeGames.flatMap(_.homeTeamPlayerStats).filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
      val hittingStatsInThisPark_Lefties = hittingStatsInThisPark.filter(_.player.bats == Left)
      val hittingStatsInThisPark_Switch = hittingStatsInThisPark.filter(_.player.bats == Switch)
      val hittingStatsInThisPark_Righties = hittingStatsInThisPark.filter(_.player.bats == Right)
      val thisParkFptsPerABForLefties = hittingStatsInThisPark_Lefties.map(_.fantasyPoints().toDouble).sum / hittingStatsInThisPark_Lefties.map(_.hittingStats.atBats).sum
      val thisParkFptsPerABForSwitch = hittingStatsInThisPark_Switch.map(_.fantasyPoints().toDouble).sum / hittingStatsInThisPark_Switch.map(_.hittingStats.atBats).sum
      val thisParkFptsPerABForRighties = hittingStatsInThisPark_Righties.map(_.fantasyPoints().toDouble).sum / hittingStatsInThisPark_Righties.map(_.hittingStats.atBats).sum

      //println(s"Home @ $team - ${homeGames.length} home games - PA: L: ${hittingStatsInThisPark_Lefties.map(_.hittingStats.atBats).sum}, S: ${hittingStatsInThisPark_Switch.map(_.hittingStats.atBats).sum}, R: ${hittingStatsInThisPark_Righties.map(_.hittingStats.atBats).sum} ")

      val allGames = season.games.filter(_.involvesTeam(team))
      val hittingStatsInAllParks = allGames.flatMap { game =>
        if (game.isHomeGameFor(team)) game.homeTeamPlayerStats.filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
        else if (game.visitingTeam == team) game.visitingTeamPlayerStats.filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
        else throw new Exception("Something unexpected happened!")
      }
      val hittingStatsInAllParks_Lefties = hittingStatsInAllParks.filter(_.player.bats == Left)
      val hittingStatsInAllParks_Switch = hittingStatsInAllParks.filter(_.player.bats == Switch)
      val hittingStatsInAllParks_Righties = hittingStatsInAllParks.filter(_.player.bats == Right)
      val allParksFptsPerABForLefties = hittingStatsInAllParks_Lefties.map(_.fantasyPoints().toDouble).sum / hittingStatsInAllParks_Lefties.map(_.hittingStats.atBats).sum
      val allParksFptsPerABForSwitch = hittingStatsInAllParks_Switch.map(_.fantasyPoints().toDouble).sum / hittingStatsInAllParks_Switch.map(_.hittingStats.atBats).sum
      val allParksFptsPerABForRighties = hittingStatsInAllParks_Righties.map(_.fantasyPoints().toDouble).sum / hittingStatsInAllParks_Righties.map(_.hittingStats.atBats).sum

      val leftyFactor = thisParkFptsPerABForLefties / allParksFptsPerABForLefties
      val rightyFactor = thisParkFptsPerABForRighties / allParksFptsPerABForRighties
      val switchFactor = {
        if (hittingStatsInThisPark_Switch.map(_.hittingStats.atBats).sum > 300) thisParkFptsPerABForSwitch / allParksFptsPerABForSwitch
        else List(leftyFactor, rightyFactor).max
      }

      (team, HitterBallparkFactor(leftyFactor, switchFactor, rightyFactor))
  }

  //  log("\nHitter ballpark factors:\n\t" + hitterBallparkFactor_VisitingTeam.toList.sortBy { case (team, bpf) => bpf.leftyMultiplier + bpf.rightyMultiplier }.reverse.map {
  //    case (homeTeam, factor) =>
  //      s"$homeTeam ballpark\t - L: ${factor.leftyMultiplier.rounded(2)}, S: ${factor.switchMultiplier.rounded(2)}, R: ${factor.rightyMultiplier.rounded(2)} for visiting hitters\t" +
  //        s"L: ${hitterBallparkFactor_HomeTeam(homeTeam).leftyMultiplier.rounded(2)}, S: ${hitterBallparkFactor_HomeTeam(homeTeam).switchMultiplier.rounded(2)}, R: ${hitterBallparkFactor_HomeTeam(homeTeam).rightyMultiplier.rounded(2)} for home team hitters"
  //  }.mkString("\n\t"))

  // FPTS/game projection for visiting team pitchers should be multiplied by this number --- key is home team, which is a proxy for ballpark
  val pitcherBallparkFactor_VisitingTeam: Map[Team, PitcherBallparkFactor] = season.games.groupBy(_.homeTeam).map {
    case (team, homeGames) =>
      val pitchingStatsInThisPark = homeGames.flatMap(_.visitingTeamPlayerStats).filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
      val pitchingStatsInThisPark_Lefties = pitchingStatsInThisPark.filter(_.player.throws == Left)
      val pitchingStatsInThisPark_Righties = pitchingStatsInThisPark.filter(_.player.throws == Right)
      val thisParkFptsPerABForLefties = pitchingStatsInThisPark_Lefties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInThisPark_Lefties.map(_.pitchingStats.atBats).sum
      val thisParkFptsPerABForRighties = pitchingStatsInThisPark_Righties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInThisPark_Righties.map(_.pitchingStats.atBats).sum

      //println(s"Visiting @ $team - ${homeGames.length} home games - PA: L: ${pitchingStatsInThisPark_Lefties.map(_.pitchingStats.atBats).sum}, R: ${pitchingStatsInThisPark_Righties.map(_.pitchingStats.atBats).sum} ")

      val allGames = season.games.filter(_.involvesTeam(team))
      val pitchingStatsInAllParks = allGames.flatMap { game =>
        if (game.isHomeGameFor(team)) game.visitingTeamPlayerStats.filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
        else if (game.visitingTeam == team) game.homeTeamPlayerStats.filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
        else throw new Exception("Something unexpected happened!")
      }
      val pitchingStatsInAllParks_Lefties = pitchingStatsInAllParks.filter(_.player.throws == Left)
      val pitchingStatsInAllParks_Righties = pitchingStatsInAllParks.filter(_.player.throws == Right)
      val allParksFptsPerABForLefties = pitchingStatsInAllParks_Lefties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInAllParks_Lefties.map(_.pitchingStats.atBats).sum
      val allParksFptsPerABForRighties = pitchingStatsInAllParks_Righties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInAllParks_Righties.map(_.pitchingStats.atBats).sum

      val leftyFactor = thisParkFptsPerABForLefties / allParksFptsPerABForLefties
      val rightyFactor = thisParkFptsPerABForRighties / allParksFptsPerABForRighties
      val switchFactor = List(leftyFactor, rightyFactor).max // just a placeholder --- don't expect any switch pitchers

      (team, PitcherBallparkFactor(leftyFactor, switchFactor, rightyFactor))
  }

  // FPTS/game projection for visiting team pitchers should be multiplied by this number --- key is home team, which is a proxy for ballpark
  val pitcherBallparkFactor_HomeTeam: Map[Team, PitcherBallparkFactor] = season.games.groupBy(_.homeTeam).map {
    case (team, homeGames) =>
      val pitchingStatsInThisPark = homeGames.flatMap(_.homeTeamPlayerStats).filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
      val pitchingStatsInThisPark_Lefties = pitchingStatsInThisPark.filter(_.player.throws == Left)
      val pitchingStatsInThisPark_Righties = pitchingStatsInThisPark.filter(_.player.throws == Right)
      val thisParkFptsPerABForLefties = pitchingStatsInThisPark_Lefties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInThisPark_Lefties.map(_.pitchingStats.atBats).sum
      val thisParkFptsPerABForRighties = pitchingStatsInThisPark_Righties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInThisPark_Righties.map(_.pitchingStats.atBats).sum

      //println(s"Home @ $team - ${homeGames.length} home games - PA: L: ${pitchingStatsInThisPark_Lefties.map(_.pitchingStats.atBats).sum}, R: ${pitchingStatsInThisPark_Righties.map(_.pitchingStats.atBats).sum} ")

      val allGames = season.games.filter(_.involvesTeam(team))
      val pitchingStatsInAllParks = allGames.flatMap { game =>
        if (game.isHomeGameFor(team)) game.homeTeamPlayerStats.filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
        else if (game.visitingTeam == team) game.visitingTeamPlayerStats.filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
        else throw new Exception("Something unexpected happened!")
      }
      val pitchingStatsInAllParks_Lefties = pitchingStatsInAllParks.filter(_.player.throws == Left)
      val pitchingStatsInAllParks_Righties = pitchingStatsInAllParks.filter(_.player.throws == Right)
      val allParksFptsPerABForLefties = pitchingStatsInAllParks_Lefties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInAllParks_Lefties.map(_.pitchingStats.atBats).sum
      val allParksFptsPerABForRighties = pitchingStatsInAllParks_Righties.map(_.fantasyPoints().toDouble).sum / pitchingStatsInAllParks_Righties.map(_.pitchingStats.atBats).sum

      val leftyFactor = thisParkFptsPerABForLefties / allParksFptsPerABForLefties
      val rightyFactor = thisParkFptsPerABForRighties / allParksFptsPerABForRighties
      val switchFactor = List(leftyFactor, rightyFactor).max // just a placeholder --- don't expect any switch pitchers

      (team, PitcherBallparkFactor(leftyFactor, switchFactor, rightyFactor))
  }

  //  log("\nPitcher ballpark factors:\n\t" + pitcherBallparkFactor_VisitingTeam.toList.sortBy { case (team, bpf) => bpf.leftyMultiplier + bpf.rightyMultiplier }.reverse.map {
  //    case (homeTeam, factor) =>
  //      s"$homeTeam ballpark\t - L: ${factor.leftyMultiplier.rounded(2)}, R: ${factor.rightyMultiplier.rounded(2)} for visiting pitchers\t" +
  //        s"L: ${pitcherBallparkFactor_HomeTeam(homeTeam).leftyMultiplier.rounded(2)}, R: ${pitcherBallparkFactor_HomeTeam(homeTeam).rightyMultiplier.rounded(2)} for home team pitchers"
  //  }.mkString("\n\t"))

  def projectedAtBats(hitter: Player, pitcher: Player): Double = hitter.visitingOrHomeTeam match {
    case Some(vh) => vh match {
      case Visiting => hitter.battingPosition match {
        case Some(bp) =>
          val leagueAvg = leagueAvgStatsByBattingPosition_VisitingTeam.get(bp).map(_.atBatsPerGame).getOrElse(0.0)
          val vsPitcher = leagueAvgStatsByBattingPositionAndOpposingPitcher_VisitingTeam.get((bp, pitcher))
          weightedAvg((leagueAvg, 20), (vsPitcher.map(_.atBatsPerGame).getOrElse(0.0), vsPitcher.map(_.numberOfGames).getOrElse(0)))
        case None => 0.0
      }
      case Home => hitter.battingPosition match {
        case Some(bp) =>
          val leagueAvg = leagueAvgStatsByBattingPosition_HomeTeam.get(bp).map(_.atBatsPerGame).getOrElse(0.0)
          val vsPitcher = leagueAvgStatsByBattingPositionAndOpposingPitcher_HomeTeam.get((bp, pitcher))
          weightedAvg((leagueAvg, 20), (vsPitcher.map(_.atBatsPerGame).getOrElse(0.0), vsPitcher.map(_.numberOfGames).getOrElse(0)))
        case None => 0.0
      }
    }
    case None => 0.0
  }

  val hitterStats_FD: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allHitters.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_FD))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val pitcherStats_FD: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allPitchers.filter(_.player.fanduel.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(FanDuelMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_FD + pitcherLeaguePointsPerGameStartedStdDev_FD))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val hitterStats_DK: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allHitters.filter(_.player.draftkings.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), hitterLeagueAvgPointsPerGameStarted_DK))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val pitcherStats_DK: Map[Player, (PlayerSeasonStats, DeviationStats)] = season.allPitchers.filter(_.player.draftkings.nonEmpty)
    .map(p => (p, DeviationStats(stdDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB))),
      downsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK),
      upsideDev(p.gamesStarted.map(_.fantasyPoints(DraftKingsMLB).toDouble), pitcherLeagueAvgPointsPerGameStarted_DK + pitcherLeaguePointsPerGameStartedStdDev_DK))))
    .map {
      case (pss, stats) => (pss.player, (pss, stats))
    }.toMap

  val hittersAvgNetUpsideDev_FD = mean(hitterStats_FD.values.map(_._2.netUpsideDev))
  val hittersAvgNetUpsideDev_DK = mean(hitterStats_DK.values.map(_._2.netUpsideDev))

  log(s"hittersAvgNetUpsideDev_FD: ${hittersAvgNetUpsideDev_FD.rounded(2)}")
  log(s"hittersAvgNetUpsideDev_DK: ${hittersAvgNetUpsideDev_DK.rounded(2)}")

  case class PitcherStats(p: Player, opposingHitters: List[Player]) {

    val pitcherAtBatsPerStart: Option[Double] = season.statsByPlayer.get(p.id).map(_.gamesStarted) match {
      case Some(gamesStarted) =>
        if (gamesStarted.nonEmpty)
          Some(gamesStarted.filter(_.isInstanceOf[PitcherGameStats])
            .map(_.asInstanceOf[PitcherGameStats].pitchingStats.atBats).sum.toDouble / gamesStarted.length)
        else None
      case None => None
    }

    val ballparkFactor: Double = p.visitingOrHomeTeam match {
      case Some(vOrH) =>
        if (vOrH == Home) pitcherBallparkFactor_HomeTeam(p.team).forPitcher(p)
        else pitcherBallparkFactor_VisitingTeam(p.opponent.get).forPitcher(p)
      case None => 1.0
    }

    val pitcherFullSeasonStats_FD: Option[PlayerSeasonStats] = pitcherStats_FD.get(p).map(_._1)
    val pitcherBvpStats_FD: Option[BatterVsPitcherStats] = season.pitcherFptsPerAB_vs_Hitters(p, opposingHitters, FanDuelMLB)
    val allPitchersStatsVsOpposingHitters_FD: Option[BatterVsPitcherStats] = season.allPitchersFptsPerAB_vs_Hitters(opposingHitters, FanDuelMLB)
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
        val allPitchersWeight = List(50, allPitchersStatsVsOpposingHitters_FD.map(_.atBats).getOrElse(0)).min
        val fullSeasonWeightedFptsPerAB = (0 until fullSeasonWeight).toList.map(i => fullSeasonFptsPerAB * ballparkFactor)
        val bvpWeightedFptsPerAB = (0 until bvpWeight).toList.map(i => pitcherBvpStats_FD.map(_.fptsPerAB).getOrElse(0.0) * ballparkFactor)
        val allPitchersWeightedFptsPerAB = (0 until allPitchersWeight).toList.map(i => allPitchersStatsVsOpposingHitters_FD.map(_.fptsPerAB).getOrElse(0.0) * ballparkFactor)
        val combinedWeightedFptsPerAB = fullSeasonWeightedFptsPerAB ++ bvpWeightedFptsPerAB ++ allPitchersWeightedFptsPerAB

        //        log(s"$p:\n\tfull season: ${fullSeasonFptsPerAB.rounded(2)} FPTS/PA based on $fullSeasonAtBats PA" +
        //            s"\n\tbvp: ${pitcherBvpStats_FD.map(_.fptsPerAB).getOrElse(0.0).rounded(2)} FPTS/PA based on ${pitcherBvpStats_FD.map(_.atBats).getOrElse(0)} PA" +
        //            s"\n\tall pitchers vs ${opposingHitters.head.team} hitters: ${allPitchersStatsVsOpposingHitters_FD.map(_.fptsPerAB).getOrElse(0.0).rounded(2)} FPTS/PA based on ${allPitchersStatsVsOpposingHitters_FD.map(_.atBats).getOrElse(0)} PA" +
        //            s"\n\tcombinedWeightedFptsPerAB: ${mean(combinedWeightedFptsPerAB).rounded(2)} FPTS/PA")

        Some(mean(combinedWeightedFptsPerAB) * abPerStart)
      }
      case _ => None
    }
    val projValueFD: Option[Double] = (p.fanduel.map(_.salary), projFptsFD) match {
      case (Some(salary), Some(fpts)) => Some((fpts / salary) * 1000)
      case _                          => None
    }

    val pitcherFullSeasonStats_DK: Option[PlayerSeasonStats] = pitcherStats_DK.get(p).map(_._1)
    val pitcherBvpStats_DK: Option[BatterVsPitcherStats] = season.pitcherFptsPerAB_vs_Hitters(p, opposingHitters, DraftKingsMLB)
    val allPitchersStatsVsOpposingHitters_DK: Option[BatterVsPitcherStats] = season.allPitchersFptsPerAB_vs_Hitters(opposingHitters, DraftKingsMLB)
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
        val allPitchersWeight = List(50, allPitchersStatsVsOpposingHitters_DK.map(_.atBats).getOrElse(0)).min
        val fullSeasonWeightedFptsPerAB = (0 until fullSeasonWeight).toList.map(i => fullSeasonFptsPerAB * ballparkFactor)
        val bvpWeightedFptsPerAB = (0 until bvpWeight).toList.map(i => pitcherBvpStats_DK.map(_.fptsPerAB).getOrElse(0.0) * ballparkFactor)
        val allPitchersWeightedFptsPerAB = (0 until allPitchersWeight).toList.map(i => allPitchersStatsVsOpposingHitters_DK.map(_.fptsPerAB).getOrElse(0.0) * ballparkFactor)
        val combinedWeightedFptsPerAB = fullSeasonWeightedFptsPerAB ++ bvpWeightedFptsPerAB ++ allPitchersWeightedFptsPerAB

        Some(mean(combinedWeightedFptsPerAB) * abPerStart)
      }
      case _ => None
    }
    val projValueDK: Option[Double] = (p.draftkings.map(_.salary), projFptsDK) match {
      case (Some(salary), Some(fpts)) => Some((fpts / salary) * 1000)
      case _                          => None
    }

  }

  val startingPitcherStats: Map[Player, PitcherStats] = Players.startingPitchers.filter(season.hasStatsFor(_)).map { p => (p, PitcherStats(p, p.opposingHitters)) }.toMap

  case class HitterStats(p: Player, opposingPitcher: Player) {

    val projAtBats: Double = projectedAtBats(p, opposingPitcher)
    val projAtBatsVsOpposingPitcher: Double = {
      val pitcherStats = startingPitcherStats.get(opposingPitcher)
      pitcherStats.flatMap(_.pitcherAtBatsPerStart).getOrElse(24.0) / 9.0
    }
    val projAtBatsVsBullpen: Double = if (projAtBatsVsOpposingPitcher >= projAtBats) 0 else projAtBats - projAtBatsVsOpposingPitcher

    val hitterTotalAtBats: Int = season.hitterStatsAgainstPitcherType(opposingPitcher.throws, p).map(_.atBats).sum
    val pitcherTotalAtBats: Int = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
    }

    // for hitter FPTS/PA --- accounts for ballpark shift
    val ballparkFactor: Double = p.visitingOrHomeTeam match {
      case Some(vOrH) =>
        if (vOrH == Home)
          hitterBallparkFactor_HomeTeam(p.team).forHitter(p) / ((1.0 + hitterBallparkFactor_HomeTeam(p.team).forHitter(p)) / 2.0)
        else
          hitterBallparkFactor_VisitingTeam(p.opponent.get).forHitter(p) / ((1.0 + hitterBallparkFactor_HomeTeam(p.team).forHitter(p)) / 2.0)
      case None => 1.0
    }

    // for opposing pitcher FPTS/PA allowed --- accounts for ballpark shift
    val ballparkFactor_vsOppPitcher: Double = p.visitingOrHomeTeam match {
      case Some(vOrH) =>
        if (vOrH == Home)
          hitterBallparkFactor_HomeTeam(p.team).forHitter(p) / ((1.0 + hitterBallparkFactor_HomeTeam(opposingPitcher.team).forHitter(p)) / 2.0)
        else
          hitterBallparkFactor_VisitingTeam(p.opponent.get).forHitter(p) / ((1.0 + hitterBallparkFactor_HomeTeam(opposingPitcher.team).forHitter(p)) / 2.0)
      case None => 1.0
    }

    val hitterSeasonStatsFD: Option[PlayerSeasonStats] = hitterStats_FD.get(p).map(_._1)
    val hitterDeviationStatsFD: Option[DeviationStats] = hitterStats_FD.get(p).map(_._2)
    val hitterFptsPerAtBatFD: Option[Double] = season.hitterFptsPerAB_vs_PitcherType(opposingPitcher.throws, p, FanDuelMLB).map(_.fptsPerAB)
    val hitterVsPitcherStatsFD: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, FanDuelMLB)
    val pitcherFptsPerAtBatAllowedFD: Option[Double] = p.bats match {
      case Left  => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Right => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Switch =>
        // use weighted avg of pitcher FPTS/PA against switch hitters and max(righties, lefties) due to small sample sizes vs switch hitters
        pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map { switchStats =>
          val rightLeftStats = if (pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD).get >
            pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD).get) {
            pitcherStatsAllowedToLefties.get(opposingPitcher).get
          } else pitcherStatsAllowedToRighties.get(opposingPitcher).get

          weightedAvg((switchStats.fptsPerAtBatAgainst_FD, switchStats.atBatsAgainst),
            (rightLeftStats.fptsPerAtBatAgainst_FD, rightLeftStats.atBatsAgainst))
        }
    }
    val bullpenFptsPerAtBatAllowedFD: Double = bullpenStatsAllowedToAllHitters(opposingPitcher.team).fptsPerAtBatAgainst_FD
    val projFptsFD: Option[Double] = hitterFptsPerAtBatFD.map { fptsPerAB =>
      val hitterWeight = List(200, hitterTotalAtBats).min
      val pitcherWeight = List(200, pitcherTotalAtBats).min
      val hitterWeightedFptsPerAB = (0 until hitterWeight).toList.map(i => fptsPerAB * ballparkFactor)
      val pitcherWeightedFptsPerAB = if (pitcherTotalAtBats == 0) Nil else (0 until pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedFD.get * ballparkFactor_vsOppPitcher)
      val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
      val fptsVsStarter = mean(combinedWeightedFptsPerAB) * projAtBatsVsOpposingPitcher

      val bullpenWeightedFptsPerAB = (0 until 200).toList.map(i => bullpenFptsPerAtBatAllowedFD * ballparkFactor_vsOppPitcher)
      val combinedWeightedFptsPerABVsBullpen = hitterWeightedFptsPerAB ++ bullpenWeightedFptsPerAB
      val fptsVsBullpen = mean(combinedWeightedFptsPerABVsBullpen) * projAtBatsVsBullpen

      val netUpsideDeviationModifier = hitterDeviationStatsFD match {
        case Some(stats) => (stats.netUpsideDev - hittersAvgNetUpsideDev_FD) * 0.2
        case None        => 0.0
      }

      //      if (opposingPitcher.id == "429719") {
      //        log(s"$p vs $opposingPitcher:\n\tprojAtBats: ${projAtBats.rounded(2)}\n\tprojAtBatsVsOpposingPitcher: ${projAtBatsVsOpposingPitcher.rounded(2)}" +
      //          s"\n\tprojAtBatsVsBullpen: ${projAtBatsVsBullpen.rounded(2)}\n\thitterTotalAtBats: ${hitterTotalAtBats.rounded(2)}\n\tpitcherTotalAtBats: ${pitcherTotalAtBats.rounded(2)}" +
      //          s"\n\tballparkFactor: ${ballparkFactor.rounded(2)}\n\tballparkFactor_vsOppPitcher: ${ballparkFactor_vsOppPitcher.rounded(2)}\n\thitterFptsPerAtBatFD: ${hitterFptsPerAtBatFD.map(_.rounded(2)).getOrElse("???")}" +
      //          s"\n\tpitcherFptsPerAtBatAllowedFD ${pitcherFptsPerAtBatAllowedFD.map(_.rounded(2)).getOrElse("???")}\n\tbullpenFptsPerAtBatAllowedFD: ${bullpenFptsPerAtBatAllowedFD.rounded(2)}" +
      //          s"\n\thitterWeight: ${hitterWeight}\n\tpitcherWeight: ${pitcherWeight}\n\thitterWeightedFptsPerAB size: ${hitterWeightedFptsPerAB.size}" +
      //          s"\n\tpitcherWeightedFptsPerAB size: ${pitcherWeightedFptsPerAB.size}\n\tfptsVsStarter: ${fptsVsStarter.rounded(2)}" +
      //          s"\n\tbullpenWeightedFptsPerAB size: ${bullpenWeightedFptsPerAB.size}\n\tfptsVsBullpen: ${fptsVsBullpen.rounded(2)}" +
      //          s"\n\tnetUpsideDeviationModifier: ${netUpsideDeviationModifier.rounded(2)}")
      //      }

      fptsVsStarter + fptsVsBullpen + netUpsideDeviationModifier
    }
    val projValueFD: Option[Double] = p.fanduel.map(_.salary).map(salary => (projFptsFD.getOrElse(0.0) / salary) * 1000)
    val valueScoreFD: Option[Double] = projValueFD.map(value => (projFptsFD.getOrElse(0.0) * 0.75) + value)

    val hitterSeasonStatsDK: Option[PlayerSeasonStats] = hitterStats_DK.get(p).map(_._1)
    val hitterDeviationStatsDK: Option[DeviationStats] = hitterStats_DK.get(p).map(_._2)
    val hitterFptsPerAtBatDK: Option[Double] = season.hitterFptsPerAB_vs_PitcherType(opposingPitcher.throws, p, DraftKingsMLB).map(_.fptsPerAB)
    val hitterVsPitcherStatsDK: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, DraftKingsMLB)
    val pitcherFptsPerAtBatAllowedDK: Option[Double] = p.bats match {
      case Left  => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
      case Right => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK)
      case Switch =>
        // use weighted avg of pitcher FPTS/PA against switch hitters and max(righties, lefties) due to small sample sizes vs switch hitters
        pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map { switchStats =>
          val rightLeftStats = if (pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK).get >
            pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_DK).orElse(hitterFptsPerAtBatDK).get) {
            pitcherStatsAllowedToLefties.get(opposingPitcher).get
          } else pitcherStatsAllowedToRighties.get(opposingPitcher).get

          weightedAvg((switchStats.fptsPerAtBatAgainst_DK, switchStats.atBatsAgainst),
            (rightLeftStats.fptsPerAtBatAgainst_DK, rightLeftStats.atBatsAgainst))
        }
    }
    val bullpenFptsPerAtBatAllowedDK: Double = bullpenStatsAllowedToAllHitters(opposingPitcher.team).fptsPerAtBatAgainst_DK
    val projFptsDK: Option[Double] = hitterFptsPerAtBatDK.map { fptsPerAB =>
      val hitterWeight = List(200, hitterTotalAtBats).min
      val pitcherWeight = List(200, pitcherTotalAtBats).min
      val hitterWeightedFptsPerAB = (0 until hitterWeight).toList.map(i => fptsPerAB * ballparkFactor)
      val pitcherWeightedFptsPerAB = if (pitcherTotalAtBats == 0) Nil else (0 until pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedDK.get * ballparkFactor_vsOppPitcher)
      val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
      val fptsVsStarter = mean(combinedWeightedFptsPerAB) * projAtBatsVsOpposingPitcher

      val bullpenWeightedFptsPerAB = (0 until 200).toList.map(i => bullpenFptsPerAtBatAllowedDK * ballparkFactor_vsOppPitcher)
      val combinedWeightedFptsPerABVsBullpen = hitterWeightedFptsPerAB ++ bullpenWeightedFptsPerAB
      val fptsVsBullpen = mean(combinedWeightedFptsPerABVsBullpen) * projAtBatsVsBullpen

      val netUpsideDeviationModifier = hitterDeviationStatsDK match {
        case Some(stats) => (stats.netUpsideDev - hittersAvgNetUpsideDev_DK) * 0.2
        case None        => 0.0
      }

      fptsVsStarter + fptsVsBullpen + netUpsideDeviationModifier
    }
    val projValueDK: Option[Double] = p.draftkings.map(_.salary).map(salary => (projFptsDK.getOrElse(0.0) / salary) * 1000)
    val valueScoreDK: Option[Double] = projValueDK.map(value => (projFptsDK.getOrElse(0.0) * 0.75) + value)

  }

  val startingHitterStats: Map[Player, HitterStats] = Players.startingHitters.filter(season.hasStatsFor(_)).map { p => (p, HitterStats(p, p.opposingPitcher)) }.toMap

  def logSummary: Unit = {
    log(s"*************** ${season.label} Season Summary --- All players ***************")
    log(s"FanDuel - \n\tLeague avg PPG for hitters: ${hitterLeagueAvgPointsPerGameStarted_FD.rounded(2)}, \n\t" +
      s"std deviation: ${hitterLeaguePointsPerGameStartedStdDev_FD.rounded(2)}\n\tLeague avg FPTS per plate appearance for hitters: ${hitterLeagueAvgPointsPerAtBat_FD.rounded(2)}")
    log(s"DraftKings - \n\tLeague avg PPG for hitters: ${hitterLeagueAvgPointsPerGameStarted_DK.rounded(2)}, \n\t" +
      s"std deviation: ${hitterLeaguePointsPerGameStartedStdDev_DK.rounded(2)}\n\tLeague avg FPTS per plate appearance for hitters: ${hitterLeagueAvgPointsPerAtBat_DK.rounded(2)}")
    log(s"FanDuel - League avg PPG for pitchers: ${pitcherLeagueAvgPointsPerGameStarted_FD.rounded(2)}, std deviation: ${pitcherLeaguePointsPerGameStartedStdDev_FD.rounded(2)}")
    log(s"DraftKings - League avg PPG for pitchers: ${pitcherLeagueAvgPointsPerGameStarted_DK.rounded(2)}, std deviation: ${pitcherLeaguePointsPerGameStartedStdDev_DK.rounded(2)}")
    log("*******************************************************************")
  }

}

case class DeviationStats(stdDev: Double, downsideDev: Double, upsideDev: Double) {
  val netUpsideDev: Double = upsideDev - downsideDev
}

case class BattingPositionStats(numberOfGames: Int, totalAtBats: Int, atBatsPerGame: Double, fptsPerAtBat: Double, fptsPerGame: Double)

case class HitterBallparkFactor(leftyMultiplier: Double, switchMultiplier: Double, rightyMultiplier: Double) {
  def forHitter(player: Player): Double = player.bats match {
    case Left   => leftyMultiplier
    case Right  => rightyMultiplier
    case Switch => switchMultiplier
  }
}

case class PitcherBallparkFactor(leftyMultiplier: Double, switchMultiplier: Double, rightyMultiplier: Double) {
  def forPitcher(player: Player): Double = player.throws match {
    case Left   => leftyMultiplier
    case Right  => rightyMultiplier
    case Switch => switchMultiplier
  }
}