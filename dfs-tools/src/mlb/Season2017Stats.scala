package mlb

import mlb._
import mlb.model._
import mlb.model.CustomTypes._
import mlb.retrosheet._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._

object Season2017Stats {
  val allStarGameDate = "2017-07-11".toDate("yyyy-MM-dd")

  val games = FileUtils.getListOfFiles(Configs.Retrosheet.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

  val season = Season(2017, games)

  val season1stHalf = Season(2017, games.filter(_.date.before(allStarGameDate)))

  val season2ndHalf = Season(2017, games.filter(_.date.after(allStarGameDate)))

  log(s"Finished loading 2017 season: ${games.length} games --- ${season2ndHalf.games.length} games after All-Star break")

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

  case class DeviationStats(stdDev: Double, downsideDev: Double, upsideDev: Double) {
    val netUpsideDev: Double = upsideDev - downsideDev
  }

  val pitcherStatsAllowedToAllHitters: Map[Player, HittingStatsAllowed] = {
    season.allPitchers.map(_.player).map { pitcher =>
      val gamesPitched = season.games.flatMap(_.statsFor(pitcher)).flatMap {
        _ match {
          case pitcherStats: PitcherGameStats => Some(pitcherStats)
          case hitterStats: HitterGameStats   => None
        }
      }
      val atBatsAgainst = gamesPitched.map(_.atBatsAgainst()).sum
      val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB).toDouble).sum
      val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
      val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB).toDouble).sum
      val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
      (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, None))
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
      val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Left))).sum
      val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Left)).toDouble).sum
      val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
      val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Left)).toDouble).sum
      val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
      (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Left)))
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
      val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Right))).sum
      val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Right)).toDouble).sum
      val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
      val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Right)).toDouble).sum
      val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
      (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Right)))
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
      val atBatsAgainst = gamesPitched.map(_.atBatsAgainst(Some(Switch))).sum
      val fptsAgainst_FD = gamesPitched.map(_.fantasyPointsAgainst(FanDuelMLB, Some(Switch)).toDouble).sum
      val fptsPerAtBatAgainst_FD = fptsAgainst_FD / atBatsAgainst
      val fptsAgainst_DK = gamesPitched.map(_.fantasyPointsAgainst(DraftKingsMLB, Some(Switch)).toDouble).sum
      val fptsPerAtBatAgainst_DK = fptsAgainst_DK / atBatsAgainst
      (pitcher -> HittingStatsAllowed(pitcher, atBatsAgainst, fptsAgainst_FD, fptsPerAtBatAgainst_FD, fptsAgainst_DK, fptsPerAtBatAgainst_DK, Some(Switch)))
    }.toMap
  }

  case class BattingPositionStats(totalAtBats: Int, atBatsPerGame: Double, fptsPerAtBat: Double, fptsPerGame: Double)

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

  def logSummary: Unit = {
    log("*************** 2017 Season Summary --- All players ***************")
    log(s"FanDuel - \n\tLeague avg PPG for hitters: ${hitterLeagueAvgPointsPerGameStarted_FD.rounded(2)}, \n\t" +
      s"std deviation: ${hitterLeaguePointsPerGameStartedStdDev_FD.rounded(2)}\n\tLeague avg FPTS per plate appearance for hitters: ${hitterLeagueAvgPointsPerAtBat_FD.rounded(2)}")
    log(s"DraftKings - \n\tLeague avg PPG for hitters: ${hitterLeagueAvgPointsPerGameStarted_DK.rounded(2)}, \n\t" +
      s"std deviation: ${hitterLeaguePointsPerGameStartedStdDev_DK.rounded(2)}\n\tLeague avg FPTS per plate appearance for hitters: ${hitterLeagueAvgPointsPerAtBat_DK.rounded(2)}")
    log(s"FanDuel - League avg PPG for pitchers: ${pitcherLeagueAvgPointsPerGameStarted_FD.rounded(2)}, std deviation: ${pitcherLeaguePointsPerGameStartedStdDev_FD.rounded(2)}")
    log(s"DraftKings - League avg PPG for pitchers: ${pitcherLeagueAvgPointsPerGameStarted_DK.rounded(2)}, std deviation: ${pitcherLeaguePointsPerGameStartedStdDev_DK.rounded(2)}")
    log("*******************************************************************")
  }

}