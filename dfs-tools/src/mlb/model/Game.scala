package mlb.model

import java.util.Date
import utils.DateTimeUtils._
import utils.FloatUtils._
import utils.Logger._
import CustomTypes._
import GameNumbers._
import WindDirections._

case class Game(
    date: Date,
    visitingTeam: Team,
    homeTeam: Team,
    gameNumber: GameNumber, // e.g. 1 or 2 if part of a double-header
    homePlateUmpireID: String,
    winningPitcher: Player,
    losingPitcher: Player,
    savePitcher: Option[Player],
    visitingTeamPlayerStats: List[PlayerGameStats],
    homeTeamPlayerStats: List[PlayerGameStats],
    visitingTeamRuns: Int,
    homeTeamRuns: Int) {

  visitingTeamPlayerStats.foreach { p => p.game = Some(this) }
  homeTeamPlayerStats.foreach { p => p.game = Some(this) }

  lazy val allPlayerStats: List[PlayerGameStats] = visitingTeamPlayerStats ++ homeTeamPlayerStats

  lazy val starterStats: List[PlayerGameStats] = visitingTeamPlayerStats.filter(_.isStarter) ++ homeTeamPlayerStats.filter(_.isStarter)

  val alias: String = s"$visitingTeam @ $homeTeam (${date.print()}${if (gameNumber != SINGLE_GAME) s" game $gameNumber" else ""})"

  lazy val visitingTeamStartingHitters: List[Player] = visitingTeamPlayerStats.filter(_.isInstanceOf[HitterGameStats]).map(_.player)
  lazy val homeTeamStartingHitters: List[Player] = homeTeamPlayerStats.filter(_.isInstanceOf[HitterGameStats]).map(_.player)

  lazy val visitingTeamStartingPitcher: Player = visitingTeamPlayerStats.find(p => p.isStarter && p.isInstanceOf[PitcherGameStats]).get.player
  lazy val homeTeamStartingPitcher: Player = homeTeamPlayerStats.find(p => p.isStarter && p.isInstanceOf[PitcherGameStats]).get.player

  def statsFor(player: Player): Option[PlayerGameStats] = allPlayerStats.find(_.player == player)

  def involvesTeam(team: Team): Boolean = visitingTeam == team || homeTeam == team

  def isHomeGameFor(team: Team): Boolean = homeTeam == team

  def isFromCurrentSeason: Boolean = !(date.before(mlb.Configs.MlbDotCom.seasonStartDate))

  override def toString: String = alias +
    s"\n  $visitingTeam stats:\n\t" + visitingTeamPlayerStats.sortBy(p => p.battingPosition + "" + (20 - p.hittingStats.atBats)).map(_.printStats).mkString("\n\t") +
    s"\n  $homeTeam stats:\n\t" + homeTeamPlayerStats.sortBy(p => p.battingPosition + "" + (20 - p.hittingStats.atBats)).map(_.printStats).mkString("\n\t")

  if (mlb.Configs.MlbDotCom.runSanityChecks) {
    allPlayerStats.filter(_.isInstanceOf[HitterGameStats]).find { hitter =>
      val fptsFD = hitter.fantasyPoints(FanDuelMLB)
      val fptsDK = hitter.fantasyPoints(DraftKingsMLB)
      val opposingPlayers = if (visitingTeamPlayerStats.contains(hitter)) homeTeamPlayerStats else visitingTeamPlayerStats
      val opposingPitchers = opposingPlayers.filter(_.isInstanceOf[PitcherGameStats])
      //println(s"game $alias, hitter $hitter, opposing pitchers: ${opposingPitchers}")
      (!(fptsFD ~= opposingPitchers.map(_.asInstanceOf[PitcherGameStats].hittingStatsAllowedByHitter.get(hitter.player).map(_.fantasyPoints(FanDuelMLB)).getOrElse(0f)).sum) ||
        !(fptsDK ~= opposingPitchers.map(_.asInstanceOf[PitcherGameStats].hittingStatsAllowedByHitter.get(hitter.player).map(_.fantasyPoints(DraftKingsMLB)).getOrElse(0f)).sum))
    } match {
      case Some(hitter) => log {
        val opposingPlayers = if (visitingTeamPlayerStats.contains(hitter)) homeTeamPlayerStats else visitingTeamPlayerStats
        val opposingPitchers = opposingPlayers.filter(_.isInstanceOf[PitcherGameStats])
        println(hitter.printStats)
        s"WARNING: Inconsistent stats for game $alias, hitter $hitter: \n" +
          s"fptsFD = ${hitter.fantasyPoints(FanDuelMLB)}\n" +
          s"pitcher stats allowed: ${opposingPitchers.map(pitcher => pitcher + " gave up " + pitcher.asInstanceOf[PitcherGameStats].hittingStatsAllowedByHitter.get(hitter.player).map(_.fantasyPoints(FanDuelMLB)).getOrElse("???")).mkString("\n\t")}\n" +
          s"fptsDK = ${hitter.fantasyPoints(DraftKingsMLB)}\n" +
          s"pitcher stats allowed: ${opposingPitchers.map(pitcher => pitcher + " gave up " + pitcher.asInstanceOf[PitcherGameStats].hittingStatsAllowedByHitter.get(hitter.player).map(_.fantasyPoints(DraftKingsMLB)).getOrElse("???")).mkString("\n\t")}\n"
      }
      case None => // OK
    }
  }
}

object GameNumbers {
  val SINGLE_GAME = 0
  val FIRST_OF_DOUBLEHEADER = 1
  val SECOND_OF_DOUBLEHEADER = 2
}

object WindDirections {
  val FROM_CENTER_FIELD = "fromcf"
  val FROM_LEFT_FIELD = "fromlf"
  val FROM_RIGHT_FIELD = "fromrf"
  val LEFT_TO_RIGHT = "ltor"
  val RIGHT_TO_LEFT = "rtol"
  val TO_CENTER_FIELD = "tocf"
  val TO_LEFT_FIELD = "tolf"
  val TO_RIGHT_FIELD = "torf"
  val UNKNOWN = "unknown"
}