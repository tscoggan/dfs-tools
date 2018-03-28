package mlb.retrosheet

import mlb.model._
import mlb.model.CustomTypes._
import mlb.Teams

/**
 * Parsed from .ROS files
 */

case class PlayerData(
  playerID: PlayerID,
  lastName: String,
  firstName: String,
  bats: Handedness,
  throws: Handedness,
  teamID: TeamID,
  position: Position)

object PlayerData {

  import scala.io.Source

  def parseFrom(rosterFileName: String): List[Player] = Source.fromFile(rosterFileName).getLines.toList
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      case line =>
        val values = line.split(",").map(_.trim)

        if (values.length != 7) throw new Exception(s"Invalid roster file format: $rosterFileName")

        Player(values(0), values(2) + " " + values(1), values(3), values(4), Teams.get(values(5)), values(6))
    }

}