package mlb.model

import mlb.Configs
import CustomTypes._

case class PlayerSeasonStats(player: Player, games: List[PlayerGameStats]) {
  val playerID: MLBPlayerID = player.id

  lazy val isPitcher: Boolean = player.position == Pitcher

  lazy val gamesStarted: List[PlayerGameStats] = games.filter(_.isStarter).sortBy(_.gameDate)
  lazy val gamesNotStarted: List[PlayerGameStats] = games.filterNot(_.isStarter).sortBy(_.gameDate)

  lazy val gamesAsHitter: List[HitterGameStats] = games.filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])
  lazy val gamesStartedAsHitter: List[HitterGameStats] = gamesStarted.filter(_.isInstanceOf[HitterGameStats]).map(_.asInstanceOf[HitterGameStats])

  lazy val gamesAsPitcher: List[PitcherGameStats] = games.filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])
  lazy val gamesStartedAsPitcher: List[PitcherGameStats] = gamesStarted.filter(_.isInstanceOf[PitcherGameStats]).map(_.asInstanceOf[PitcherGameStats])

  val numberOfGames: Int = games.length
  lazy val numberOfGamesStarted: Int = gamesStarted.length

  val hittingPA: Int = gamesAsHitter.map(_.hittingStats.atBats).sum
  lazy val hittingPA_asStarter: Int = gamesStartedAsHitter.map(_.hittingStats.atBats).sum

  val pitchingPA: Int = gamesAsPitcher.map(_.pitchingStats.atBats).sum
  lazy val pitchingPA_asStarter: Int = gamesStartedAsPitcher.map(_.pitchingStats.atBats).sum

  def hitterFptsPerPA(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesAsHitter.map(_.fantasyPoints(scoringSystem)).sum / hittingPA
  def hitterFptsPerPA_asStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStartedAsHitter.map(_.fantasyPoints(scoringSystem)).sum / hittingPA_asStarter
  def hitterFptsPerGame(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesAsHitter.map(_.fantasyPoints(scoringSystem)).sum / gamesAsHitter.length
  def hitterFptsPerGameAsStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStartedAsHitter.map(_.fantasyPoints(scoringSystem)).sum / gamesStartedAsHitter.length

  def pitcherFptsPerPA(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesAsPitcher.map(_.fantasyPoints(scoringSystem)).sum / pitchingPA
  def pitcherFptsPerPA_asStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStartedAsPitcher.map(_.fantasyPoints(scoringSystem)).sum / pitchingPA_asStarter
  def pitcherFptsPerGame(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesAsPitcher.map(_.fantasyPoints(scoringSystem)).sum / gamesAsPitcher.length
  def pitcherFptsPerGameAsStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStartedAsPitcher.map(_.fantasyPoints(scoringSystem)).sum / gamesStartedAsPitcher.length

  def pitcherFptsAllowedPerPA(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesAsPitcher.map(_.fantasyPointsAgainst(scoringSystem)).sum / pitchingPA
  def pitcherFptsAllowedPerPA_asStarter(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = gamesStartedAsPitcher.map(_.fantasyPointsAgainst(scoringSystem)).sum / pitchingPA_asStarter

}