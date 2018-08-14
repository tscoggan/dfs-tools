package mlb.model

import CustomTypes._
import mlb._
import utils.DateTimeUtils._

case class Season(label: String, games: List[Game]) {

  val statsByPlayer: Map[MLBPlayerID, PlayerSeasonStats] = {
    games.flatMap { game =>
      val aps = game.allPlayerStats
      aps.foreach { pgs => pgs.game = Some(game) }
      aps
    }.groupBy(_.player).map {
      case (player, games) => (player.id, PlayerSeasonStats(player, games.sortBy(_.gameDate)))
    }
  }

  def hasStatsFor(p: Player): Boolean = statsByPlayer.contains(p.id)

  lazy val allPlayers: List[PlayerSeasonStats] = statsByPlayer.values.toList
  lazy val allHitters: List[PlayerSeasonStats] = allPlayers.filterNot(_.isPitcher)
  lazy val allPitchers: List[PlayerSeasonStats] = allPlayers.filter(_.isPitcher)

  def gamesWithStartingPitcher(pitcher: Player): List[Game] = games.filter { g => g.visitingTeamStartingPitcher == pitcher || g.homeTeamStartingPitcher == pitcher }

  lazy val reliefPitchingStatsByTeam: Map[Team, List[PitcherGameStats]] = allPitchers.groupBy(_.player.team).map {
    case (team, pss) =>
      (team, pss.flatMap(_.gamesNotStarted).flatMap {
        _ match {
          case hgs: HitterGameStats  => None
          case pgs: PitcherGameStats => Some(pgs)
        }
      })
  }

  def pitcherStatsAgainstHitter(pitcher: Player, hitter: Player): List[PitchingStats] = statsByPlayer(pitcher.id).games.flatMap { gameStats =>
    gameStats match {
      case pgs: PitcherGameStats => pgs.pitchingStatsByHitter.get(hitter)
      case _                     => None
    }
  }

  def pitcherFptsPerAB_vs_Hitter(pitcher: Player, hitter: Player, scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAgainstHitter(pitcher, hitter), scoringSystem)
  }

  def pitcherStatsAgainstHitters(pitcher: Player, hitters: List[Player]): List[PitchingStats] = statsByPlayer(pitcher.id).games.flatMap { gameStats =>
    gameStats match {
      case pgs: PitcherGameStats => hitters.flatMap { hitter => pgs.pitchingStatsByHitter.get(hitter) }
      case _                     => None
    }
  }

  def pitcherFptsPerAB_vs_Hitters(pitcher: Player, hitters: List[Player], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAgainstHitters(pitcher, hitters), scoringSystem)
  }

  def pitcherStatsAllowedToHitter(pitcher: Player, hitter: Player): List[HittingStats] = statsByPlayer.get(pitcher.id) match {
    case Some(stats) => stats.games.flatMap { gameStats =>
      gameStats match {
        case pgs: PitcherGameStats => pgs.hittingStatsAllowedByHitter.get(hitter)
        case _                     => None
      }
    }
    case None => Nil
  }

  def hitterFptsPerAB_vs_Pitcher(pitcher: Player, hitter: Player, scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAllowedToHitter(pitcher, hitter), scoringSystem)
  }

  def hitterFptsPerAB_vs_Pitchers(pitchers: List[Player], hitter: Player, scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitchers.flatMap { pitcher => pitcherStatsAllowedToHitter(pitcher, hitter) }, scoringSystem)
  }

  def hitterStatsAgainstPitcherType(throws: Handedness, hitter: Player): List[HittingStats] = {
    allPitchers.map(_.player).filter(_.throws == throws).flatMap { pitcher => pitcherStatsAllowedToHitter(pitcher, hitter) }
  }

  def hitterFptsPerAB_vs_PitcherType(throws: Handedness, hitter: Player, scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(hitterStatsAgainstPitcherType(throws, hitter), scoringSystem)
  }

  def pitcherStatsAllowedToHitters(pitcher: Player, hitters: List[Player]): List[HittingStats] = statsByPlayer(pitcher.id).games.flatMap { gameStats =>
    gameStats match {
      case pgs: PitcherGameStats => hitters.map { hitter => pgs.hittingStatsAllowedByHitter.get(hitter) }
      case _                     => None
    }
  }.flatten

  def hittersFptsPerAB_vs_Pitcher(pitcher: Player, hitters: List[Player], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAllowedToHitters(pitcher, hitters), scoringSystem)
  }

  def allPitchingStatsVsHitters(hitters: List[Player]): List[PitchingStats] = hitters.flatMap { hitter =>
    statsByPlayer.get(hitter.id) match {
      case Some(stats) => stats.games.flatMap { gameStats =>
        gameStats match {
          case hgs: HitterGameStats => hgs.pitchingStatsAgainst
          case _                    => None
        }
      }
      case None => None
    }
  }

  def allPitchersFptsPerAB_vs_Hitters(hitters: List[Player], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(allPitchingStatsVsHitters(hitters), scoringSystem)
  }

  private def batterVsPitcherStatsHelper(stats: List[PlayerStats], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    if (Configs.overweightRecent && Configs.recentDaysToWeightHigher > 0) {
      val (latestGames, recentGames, oldGames) = {
        val (notOld, old) = stats.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHigher)))
        val (latest, recent) = notOld.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToWeightHighest)))
        (latest, recent, old)
      }

      val (latestFptsPerGame, latestAtBatsPerGame) = latestGames.map { case game => (game.fantasyPoints(scoringSystem).toDouble * Configs.highestWeight, game.atBats) }.unzip
      val (recentFptsPerGame, recentAtBatsPerGame) = recentGames.map { case game => (game.fantasyPoints(scoringSystem).toDouble * Configs.higherWeight, game.atBats) }.unzip
      val (oldFptsPerGame, oldAtBatsPerGame) = oldGames.map { case game => (game.fantasyPoints(scoringSystem).toDouble, game.atBats) }.unzip

      val weightedTotalAtBats = latestAtBatsPerGame.map(_ * Configs.highestWeight).sum + recentAtBatsPerGame.map(_ * Configs.higherWeight).sum + oldAtBatsPerGame.sum
      val totalAtBats = latestAtBatsPerGame.sum + recentAtBatsPerGame.sum + oldAtBatsPerGame.sum

      //      if (logstuff) {
      //        if (stats.isEmpty) println(s"~~~stats: -") else println(s"~~~stats: ${stats.size}")
      //        stats.groupBy(_.gameDate).toList.sortBy(_._1).foreach{case (date, s) => println(s"\t\t${date.print()} --> ${s.length} games: \n\t\t\t${s.mkString("\n\t\t\t")}")}
      //        if (latestGames.isEmpty) println(s"~~~latestGames: -") else println(s"~~~latestGames: ${latestGames.size} since ${latestGames.map(_.gameDate).min.print()}")
      //        if (recentGames.isEmpty) println(s"~~~recentGames: -") else println(s"~~~recentGames: ${recentGames.size} between ${recentGames.map(_.gameDate).min.print()} & ${recentGames.map(_.gameDate).max.print()}")
      //        if (oldGames.isEmpty) println(s"~~~oldGames: -") else println(s"~~~oldGames: ${oldGames.size} between ${oldGames.map(_.gameDate).min.print()} & ${oldGames.map(_.gameDate).max.print()}")
      //        println(s"~~~weightedTotalAtBats: ${latestAtBatsPerGame.map(_ * 3).sum} + ${recentAtBatsPerGame.map(_ * 2).sum} + ${oldAtBatsPerGame.sum}")
      //        println(s"~~~weighted FPTS/game: ${latestFptsPerGame.sum / latestAtBatsPerGame.map(_ * 3).sum} + ${recentFptsPerGame.sum / recentAtBatsPerGame.map(_ * 2).sum} + ${oldFptsPerGame.sum / oldAtBatsPerGame.sum}")
      //      }

      if (totalAtBats > 0) Some(BatterVsPitcherStats(totalAtBats, (latestFptsPerGame ++ recentFptsPerGame ++ oldFptsPerGame).sum / weightedTotalAtBats)) else None
    } else {
      val (fptsPerGame, atBatsPerGame) = stats.map { case game => (game.fantasyPoints(scoringSystem).toDouble, game.atBats) }.unzip
      val totalAtBats = atBatsPerGame.sum
      if (totalAtBats > 0) Some(BatterVsPitcherStats(totalAtBats, fptsPerGame.sum / totalAtBats)) else None
    }
  }

}

case class BatterVsPitcherStats(atBats: Int, fptsPerAB: Double)