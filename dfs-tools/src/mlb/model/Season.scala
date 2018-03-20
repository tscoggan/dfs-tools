package mlb.model

import CustomTypes._
import mlb._

class Season(year: Int, games: List[Game]) {

  val statsByPlayer: Map[PlayerID, PlayerSeasonStats] = games.flatMap(_.allPlayerStats).groupBy(_.player).map {
    case (player, games) => (player.id, PlayerSeasonStats(player, games))
  }

  lazy val allPlayers: List[PlayerSeasonStats] = statsByPlayer.values.toList
  lazy val allHitters: List[PlayerSeasonStats] = allPlayers.filterNot(_.isPitcher)
  lazy val allPitchers: List[PlayerSeasonStats] = allPlayers.filter(_.isPitcher)

}