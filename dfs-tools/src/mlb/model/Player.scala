package mlb.model

import CustomTypes._
import mlb._
import mlb.model._

/**
 * Static data about a player
 */

case class Player(
    id: PlayerID,
    name: String,
    bats: Handedness,
    throws: Handedness,
    team: Team,
    position: Position,
    opponent: Option[Team] = None,
    visitingOrHomeTeam: Option[VisitingOrHomeTeam] = None, // for the upcoming game
    fanduel: Option[PlayerSiteInfo] = None,
    draftkings: Option[PlayerSiteInfo] = None,
    retrosheet: Option[PlayerSiteInfo] = None) {

  lazy val battingPosition: Option[Int] = fanduel.flatMap(_.battingPosition).orElse(rg.StartingLineups.battingPositionOf(this))

  // for a given DFS game slate:
  def opposingPitcher(allStartingPitchers: List[Player]): Player = allStartingPitchers.find(_.team == opponent.get).get
  def opposingHitters(allHitters: List[Player], startersOnly: Boolean = false): List[Player] = startersOnly match {
    case true  => allHitters.filter(p => p.team == opponent.get && p.fanduel.flatMap(_.starter).getOrElse(false))
    case false => allHitters.filter(_.team == opponent.get)
  }

  def isStarting: Boolean = fanduel.flatMap(_.starter).getOrElse(rg.StartingLineups.isStarting(this))

  override def toString: String = s"$name ($position, $team)"
}

case class PlayerSiteInfo(name: String, team: Team, position: Position, salary: Int, starter: Option[Boolean], battingPosition: Option[Int])