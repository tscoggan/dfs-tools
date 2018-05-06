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

      if (Configs.overweightRecent && Configs.recentDaysToOverweight > 0) {
        val (recentGames, oldGames) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToOverweight)))

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
        val weightedAtBatsAgainst = recentGames.map(_.atBatsAgainst() * 2).sum + oldGames.map(_.atBatsAgainst()).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val weightedFptsAgainst_FD = recentGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val weightedFptsAgainst_DK = recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
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

      if (Configs.overweightRecent && Configs.recentDaysToOverweight > 0) {
        val (recentGames, oldGames) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToOverweight)))

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Left))).sum
        val weightedAtBatsAgainst = recentGames.map(_.atBatsAgainst(Some(Left)) * 2).sum + oldGames.map(_.atBatsAgainst(Some(Left))).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble).sum
        val weightedFptsAgainst_FD = recentGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble).sum
        val weightedFptsAgainst_DK = recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble).sum
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

      if (Configs.overweightRecent && Configs.recentDaysToOverweight > 0) {
        val (recentGames, oldGames) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToOverweight)))

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Right))).sum
        val weightedAtBatsAgainst = recentGames.map(_.atBatsAgainst(Some(Right)) * 2).sum + oldGames.map(_.atBatsAgainst(Some(Right))).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble).sum
        val weightedFptsAgainst_FD = recentGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble).sum
        val weightedFptsAgainst_DK = recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble).sum
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

      if (Configs.overweightRecent && Configs.recentDaysToOverweight > 0) {
        val (recentGames, oldGames) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToOverweight)))

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Switch))).sum
        val weightedAtBatsAgainst = recentGames.map(_.atBatsAgainst(Some(Switch)) * 2).sum + oldGames.map(_.atBatsAgainst(Some(Switch))).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble).sum
        val weightedFptsAgainst_FD = recentGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble).sum
        val weightedFptsAgainst_DK = recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble).sum
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

      if (Configs.overweightRecent && Configs.recentDaysToOverweight > 0) {
        val (recentGames, oldGames) = gamesPitched.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToOverweight)))

        val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
        val weightedAtBatsAgainst = recentGames.map(_.atBatsAgainst() * 2).sum + oldGames.map(_.atBatsAgainst()).sum

        val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val weightedFptsAgainst_FD = recentGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
        val fptsPerAtBatAgainst_FD = weightedFptsAgainst_FD / weightedAtBatsAgainst

        val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
        val weightedFptsAgainst_DK = recentGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble * 2).sum + oldGames.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
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
  log(s"bullpenStatsAllowedToAllHitters:\n\t${
    bullpenStatsAllowedToAllHitters.map {
      case (t, s) =>
        s"$t - PA: ${s.atBatsAgainst}, FPTS/PA (FD): ${s.fptsPerAtBatAgainst_FD.rounded(2)}, FPTS/PA (DK): ${s.fptsPerAtBatAgainst_DK.rounded(2)}"
    }.mkString("\n\t")
  }")

  val numberOfGames = season.games.length

  val leagueAvgStatsByBattingPosition: Map[BattingPosition, BattingPositionStats] = season.allPlayers.flatMap(_.games).groupBy(_.battingPosition).map {
    case (bp, games) =>
      (bp ->
        BattingPositionStats(games.map(_.hittingStats.atBats).sum,
          games.map(_.hittingStats.atBats.toDouble).sum / (numberOfGames * 2),
          games.map(_.hittingStats.fantasyPoints().toDouble).sum / games.map(_.hittingStats.atBats).sum,
          games.map(_.hittingStats.fantasyPoints().toDouble).sum / (numberOfGames * 2)))
  }

  val leagueAvgStatsByBattingPosition_VisitingTeam: Map[BattingPosition, BattingPositionStats] = season.allPlayers.flatMap(_.games)
    .filter(pgs => pgs.game.get.visitingTeamPlayerStats.contains(pgs))
    .groupBy(_.battingPosition).map {
      case (bp, games) =>
        (bp ->
          BattingPositionStats(games.map(_.hittingStats.atBats).sum,
            games.map(_.hittingStats.atBats.toDouble).sum / numberOfGames,
            games.map(_.hittingStats.fantasyPoints().toDouble).sum / games.map(_.hittingStats.atBats).sum,
            games.map(_.hittingStats.fantasyPoints().toDouble).sum / numberOfGames))
    }

  val leagueAvgStatsByBattingPosition_HomeTeam: Map[BattingPosition, BattingPositionStats] = season.allPlayers.flatMap(_.games)
    .filter(pgs => pgs.game.get.homeTeamPlayerStats.contains(pgs))
    .groupBy(_.battingPosition).map {
      case (bp, games) =>
        (bp ->
          BattingPositionStats(games.map(_.hittingStats.atBats).sum,
            games.map(_.hittingStats.atBats.toDouble).sum / numberOfGames,
            games.map(_.hittingStats.fantasyPoints().toDouble).sum / games.map(_.hittingStats.atBats).sum,
            games.map(_.hittingStats.fantasyPoints().toDouble).sum / numberOfGames))
    }

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

case class BattingPositionStats(totalAtBats: Int, atBatsPerGame: Double, fptsPerAtBat: Double, fptsPerGame: Double)

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