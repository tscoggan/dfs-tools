package mlb.model

import CustomTypes._
import mlb._

class Season(year: Int, games: List[Game]) {

  lazy val playerGameStats: Map[Player, List[PlayerGameStats]] = games.flatMap(_.allPlayerStats).groupBy(_.player)

  lazy val starterGameStats: Map[Player, List[PlayerGameStats]] = games.flatMap(_.starterStats).groupBy(_.player)

  def allStatsFor(playerID: PlayerID): List[PlayerGameStats] = playerGameStats.getOrElse(Players.get(playerID), Nil)

  def starterStatsFor(playerID: PlayerID): List[PlayerGameStats] = starterGameStats.getOrElse(Players.get(playerID), Nil)

}