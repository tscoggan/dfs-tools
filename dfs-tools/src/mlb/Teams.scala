package mlb

import retrosheet._
import model._
import model.CustomTypes._
import utils.StringUtils._
import scala.io.Source

object Teams {

  val teamIDMappings: Map[TeamID, (String, String)] = Source.fromFile(Configs.teamMappingsFile).getLines.toList.tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>
        val Array(retrosheetID, fanduelID, draftkingsID) = nextLine.splitCSV()
        (retrosheetID -> (fanduelID, draftkingsID))
    }.toMap

  val allTeams: List[Team] = TeamData.parseFrom(Configs.Retrosheet.teamsFileName).sortBy(_.id)

  val teamsByID: Map[TeamID, Team] = allTeams.map { t => (t.id, t) }.toMap

  def get(teamID: String): Team = teamsByID.get(teamID).orElse(allTeams.find { t => t.fanduelID == teamID || t.draftkingsID == teamID }) match {
    case Some(team) => team
    case None       => throw new Exception("Couldn't find team with ID " + teamID)
  }

}