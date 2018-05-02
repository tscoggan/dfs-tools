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

  def pitcherStatsAgainstHitter(pitcher: Player, hitter: Player): List[PitchingStats] = statsByPlayer(pitcher.id).games.collect { gameStats =>
    gameStats match {
      case pgs: PitcherGameStats => pgs.pitchingStatsAgainst(hitter)
    }
  }

  def pitcherFptsPerAB_vs_Hitter(pitcher: Player, hitter: Player, scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAgainstHitter(pitcher, hitter), scoringSystem)
  }

  def pitcherStatsAgainstHitters(pitcher: Player, hitters: List[Player]): List[PitchingStats] = statsByPlayer(pitcher.id).games.collect { gameStats =>
    gameStats match {
      case pgs: PitcherGameStats => hitters.map { hitter => pgs.pitchingStatsAgainst(hitter) }
    }
  }.flatten

  def pitcherFptsPerAB_vs_Hitters(pitcher: Player, hitters: List[Player], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAgainstHitters(pitcher, hitters), scoringSystem)
  }

  def pitcherStatsAllowedToHitter(pitcher: Player, hitter: Player): List[HittingStats] = statsByPlayer.get(pitcher.id) match {
    case Some(stats) => stats.games.collect { gameStats =>
      gameStats match {
        case pgs: PitcherGameStats => pgs.hittingStatsAllowedTo(hitter)
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

  def pitcherStatsAllowedToHitters(pitcher: Player, hitters: List[Player]): List[HittingStats] = statsByPlayer(pitcher.id).games.collect { gameStats =>
    gameStats match {
      case pgs: PitcherGameStats => hitters.map { hitter => pgs.hittingStatsAllowedTo(hitter) }
    }
  }.flatten

  def hittersFptsPerAB_vs_Pitcher(pitcher: Player, hitters: List[Player], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    batterVsPitcherStatsHelper(pitcherStatsAllowedToHitters(pitcher, hitters), scoringSystem)
  }

  private def batterVsPitcherStatsHelper(stats: List[PlayerStats], scoringSystem: DFSScoringSystem): Option[BatterVsPitcherStats] = {
    if (Configs.overweightRecent && Configs.recentDaysToOverweight > 0) {
      val (recentGames, oldGames) = stats.partition(_.gameDate.after(today.minusDays(Configs.recentDaysToOverweight)))

      val (recentFptsPerGame, recentAtBatsPerGame) = recentGames.map { case game => (game.fantasyPoints(scoringSystem).toDouble * 2, game.atBats) }.unzip
      val (oldFptsPerGame, oldAtBatsPerGame) = oldGames.map { case game => (game.fantasyPoints(scoringSystem).toDouble, game.atBats) }.unzip

      val weightedTotalAtBats = recentAtBatsPerGame.map(_ * 2).sum + oldAtBatsPerGame.sum
      val totalAtBats = recentAtBatsPerGame.sum + oldAtBatsPerGame.sum
      if (totalAtBats > 0) Some(BatterVsPitcherStats(totalAtBats, (recentFptsPerGame ++ oldFptsPerGame).sum / weightedTotalAtBats)) else None
    } else {
      val (fptsPerGame, atBatsPerGame) = stats.map { case game => (game.fantasyPoints(scoringSystem).toDouble, game.atBats) }.unzip
      val totalAtBats = atBatsPerGame.sum
      if (totalAtBats > 0) Some(BatterVsPitcherStats(totalAtBats, fptsPerGame.sum / totalAtBats)) else None
    }
  }

}

case class BatterVsPitcherStats(atBats: Int, fptsPerAB: Double)