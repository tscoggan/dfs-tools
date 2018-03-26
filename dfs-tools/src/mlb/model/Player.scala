package mlb.model

import CustomTypes._

/**
 * Static data about a player
 */

case class Player (
  id: PlayerID,
  name: String,
  bats: Handedness,
  throws: Handedness,
  teamID: TeamID,
  position: Position) {
  
  override def toString: String = s"$name ($position)"//, $teamID)"
}