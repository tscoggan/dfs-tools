package mlb.model

import CustomTypes._
import mlb._

case class Season(year: Int, games: List[Game]) {

  val statsByPlayer: Map[PlayerID, PlayerSeasonStats] = {
    games.flatMap { game =>
      val aps = game.allPlayerStats
      aps.foreach { pgs => pgs.game = Some(game) }
      aps
    }.groupBy(_.player).map {
      case (player, games) => (player.id, PlayerSeasonStats(player, games.sortBy(_.gameDate)))
    }
  }

  lazy val allPlayers: List[PlayerSeasonStats] = statsByPlayer.values.toList
  lazy val allHitters: List[PlayerSeasonStats] = allPlayers.filterNot(_.isPitcher)
  lazy val allPitchers: List[PlayerSeasonStats] = allPlayers.filter(_.isPitcher)

}