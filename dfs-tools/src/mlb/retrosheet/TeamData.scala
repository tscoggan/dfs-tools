package mlb.retrosheet

import mlb.model._
import mlb.model.CustomTypes._

/**
 * Parsed from TEAMYYYY files
 */

case class TeamData(
  teamID: TeamID,
  league: String,
  city: String,
  name: String)

object TeamData {

  import scala.io.Source

  def parseFrom(teamsFileName: String): List[Team] = Source.fromFile(teamsFileName).getLines.toList
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      case line =>
        val values = line.split(",").map(_.trim)

        if (values.length != 4) throw new Exception(s"Invalid teams file format: $teamsFileName")

        Team(values(0), values(1), values(2), values(3))
    }

}