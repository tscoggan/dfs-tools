package mlb.model

import mlb.Configs
import CustomTypes._

case class PlayerSeasonStats(player: Player, games: List[PlayerGameStats]) {
  val playerID: PlayerID = player.id

  lazy val isPitcher: Boolean = player.position == Pitcher

  lazy val gamesStarted: List[PlayerGameStats] = games.filter(_.isStarter)

  val numberOfGames: Int = games.length
  lazy val numberOfGamesStarted: Int = gamesStarted.length

  val atBats: Int = games.map(_.atBats).sum
  lazy val atBatsAsStarter: Int = gamesStarted.map(_.atBats).sum

  def fptsPerAtBat(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = games.map(_.fantasyPoints(scoringSystem)).sum / atBats
  def fptsPerAtBatAsStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStarted.map(_.fantasyPoints(scoringSystem)).sum / atBatsAsStarter
  def fptsPerGame(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = games.map(_.fantasyPoints(scoringSystem)).sum / numberOfGames
  def fptsPerGameAsStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStarted.map(_.fantasyPoints(scoringSystem)).sum / numberOfGamesStarted

}