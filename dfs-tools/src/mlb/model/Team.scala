package mlb.model

import CustomTypes._

/**
 * Static data about a team
 */

case class Team(
    id: TeamID,
    league: String,
    city: String,
    name: String,
    fanduelID: String,
    draftkingsID: String) {

  override def toString: String = draftkingsID
}