package mlb

import retrosheet._
import model._
import model.CustomTypes._

object Teams {
  
  val allTeams: List[Team] = TeamData.parseFrom(Configs.Retrosheet.teamsFileName)

  val teamsByID: Map[TeamID, Team] = allTeams.map { t => (t.id, t) }.toMap
  
  def get(teamID: String): Team = teamsByID.get(teamID).get // throws exception if teamID is invalid
}