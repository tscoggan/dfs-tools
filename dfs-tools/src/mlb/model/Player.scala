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
    fanduel: Option[PlayerSiteInfo] = None,
    draftkings: Option[PlayerSiteInfo] = None) {

  override def toString: String = s"$name ($position, $team)"
}

case class PlayerSiteInfo(name: String, team: String, position: Position, salary: Int)