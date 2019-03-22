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
 * file.  This should be run ad hoc whenever players have been traded to new teams, etc.  It will download all active players.
 */

object PlayerDataUpdateTool extends App {

  implicit val formats = DefaultFormats

  case class PlayerData(player_id: String, name_use: String, name_last: String, position: String, team_abbrev: String, bats: String, throws: String) {

    def toPlayerMLB: Player_MLB = Player_MLB(
      player_id,
      name_last,
      name_use,
      bats,
      throws,
      mlb.Teams.get(team_abbrev),
      position)

  }

  private val updatedPlayers: List[Player_MLB] = {
    log("Retrieving latest player data from MLB.com...")

    val url = "http://lookup-service-prod.mlb.com/json/named.search_player_all.bam?sport_code='mlb'&active_sw='Y'&name_part='%25'"

    val json = parse(scala.io.Source.fromURL(url)("ISO-8859-1").mkString) \ "search_player_all" \ "queryResults"

    (json \ "totalSize").extract[String].toInt match {
      case 0   => throw new Exception(s"Empty result set")
      case num => log(s"Query returned $num players")
    }

    (json \ "row").extract[List[PlayerData]].map(_.toPlayerMLB)
  }

  log("Updating player data file...")
  Player_MLB.savePlayersToFile(updatedPlayers.sortBy(_.id), true)
  log("DONE")

}