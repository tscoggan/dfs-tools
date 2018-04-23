package mlb.model

import CustomTypes._
import mlb._
import mlb.model._
import mlb.mlbdotcom.Player_MLB

/**
 * Static data about a player
 */

case class Player(
    id: PlayerID, // Retrosheet player ID
    name: String,
    bats: Handedness,
    throws: Handedness,
    team: Team,
    position: Position,
    opponent: Option[Team] = None,
    visitingOrHomeTeam: Option[VisitingOrHomeTeam] = None, // for the upcoming game
    mlbdotcom: Option[Player_MLB] = None,
    fanduel: Option[PlayerSiteInfo] = None,
    draftkings: Option[PlayerSiteInfo] = None,
    retrosheet: Option[PlayerSiteInfo] = None) {

  lazy val battingPosition: Option[Int] = fanduel.flatMap(_.battingPosition).orElse(rg.StartingLineups.battingPositionOf(this))

  // for a given DFS game slate:
  def opposingPitcher: Player = Players.startingPitchers.find(_.team == opponent.get).get
  def opposingHitters: List[Player] = Players.startingHittersByTeam(opponent.get)

  def isStarting: Boolean = fanduel.flatMap(_.starter).getOrElse(rg.StartingLineups.isStarting(this))

  override def toString: String = s"$name ($position, $team)"
  
  def toStringTeamOnly: String = s"$name ($team)"
  
  def toString_FD: String = s"$name (${fanduel.map(_.position).getOrElse("???")}, $team)"
  def toString_DK: String = s"$name (${draftkings.map(_.position).getOrElse("???")}, $team)"
}

case class PlayerSiteInfo(name: String, team: Team, position: Position, salary: Int, starter: Option[Boolean], battingPosition: Option[Int])