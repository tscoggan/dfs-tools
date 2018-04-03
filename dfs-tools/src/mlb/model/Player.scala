package mlb.model

import CustomTypes._

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
    fanduel: Option[PlayerSiteInfo] = None,
    draftkings: Option[PlayerSiteInfo] = None) {
  
  // for a given DFS game slate:
  def opposingPitcher(allStartingPitchers: List[Player]): Player = allStartingPitchers.find(_.team == opponent.get).get
  def opposingHitters(allHitters: List[Player]): List[Player] = allHitters.filter(_.team == opponent.get)

  override def toString: String = s"$name ($position, $team)"
}

case class PlayerSiteInfo(name: String, team: Team, position: Position, salary: Int, starter: Option[Boolean], battingPosition: Option[Int])