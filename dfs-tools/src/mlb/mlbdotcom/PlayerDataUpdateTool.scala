package mlb.mlbdotcom

import mlb.model._
import mlb.model.CustomTypes._
import utils.StringUtils._
import utils.FileUtils._
import utils.DateTimeUtils._
import utils.Logger._
import java.util.Date
import org.json4s._
import org.json4s.jackson.JsonMethods._

/**
 * App for downloading the latest data for all known players from MLB.com and updating the /data/mlb/mlb_dot_com/players.txt
 * file.  This should be run ad hoc whenever players have been traded to new teams, etc.  
 */

object PlayerDataUpdateTool extends App {

  implicit val formats = DefaultFormats

  case class PlayerData(player_id: MLBPlayerID, name_use: String, name_last: String, primary_position_txt: String, team_abbrev: String, bats: String, throws: String) {

    def toPlayerMLB: Player_MLB = Player_MLB(
      player_id,
      name_last,
      name_use,
      bats,
      throws,
      mlb.Teams.get(team_abbrev),
      primary_position_txt)

  }

  case class PlayerID(player_id: MLBPlayerID)

  def getLatestPlayerData(id: MLBPlayerID): Player_MLB = {
    val url = "http://lookup-service-prod.mlb.com/json/named.player_info.bam?sport_code='mlb'&player_id=" + id
    val json = parse(scala.io.Source.fromURL(url)("ISO-8859-1").mkString) \ "player_info" \ "queryResults"
    (json \ "totalSize").extract[String].toInt match {
      case 1   => (json \ "row").extract[PlayerData].toPlayerMLB
      case num => throw new Exception(s"Found $num records for player ID $id")
    }
  }

  private val allActivePlayerIDs: List[PlayerID] = {
    log("Retrieving ID's for all active players from MLB.com...")

    val url = "http://lookup-service-prod.mlb.com/json/named.search_player_all.bam?sport_code='mlb'&active_sw='Y'&name_part='%25'"

    val json = parse(scala.io.Source.fromURL(url)("ISO-8859-1").mkString) \ "search_player_all" \ "queryResults"

    (json \ "totalSize").extract[String].toInt match {
      case 0   => throw new Exception(s"Empty result set")
      case num => log(s"Query returned $num players")
    }

    (json \ "row").extract[List[PlayerID]]
  }

  private val distinctPlayerIDs = (Player_MLB.allPlayers.map(_.id) ++ allActivePlayerIDs.map(_.player_id)).distinct

  log(s"Found ${distinctPlayerIDs.length} distinct player IDs")

  private val updatedPlayers: List[Player_MLB] = {
    log("Retrieving latest player data from MLB.com for existing players...")
    distinctPlayerIDs.map { id => getLatestPlayerData(id) }
  }

  Player_MLB.savePlayersToFile(updatedPlayers.sortBy(_.id), true)
  
  log("DONE")

}