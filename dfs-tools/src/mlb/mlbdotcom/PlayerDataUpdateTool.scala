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
 * file.  This should be run ad hoc whenever players have been traded to new teams, etc.  It will NOT download new players
 * not yet in the file.
 */

object PlayerDataUpdateTool extends App {

  implicit val formats = DefaultFormats

  case class PlayerData(player_id: String, name_use: String, name_last: String, primary_position_txt: String, team_abbrev: String, bats: String, throws: String) {

    def toPlayerMLB: Player_MLB = Player_MLB(
      player_id,
      name_last,
      name_use,
      bats,
      throws,
      mlb.Teams.get(team_abbrev),
      primary_position_txt)

  }

  private var unchanged = 0
  private var changed = 0

  private val updatedPlayers: List[Player_MLB] = {
    log("Retrieving latest player data from MLB.com...")
    Player_MLB.allPlayers.map { player =>
      val newData: Player_MLB = {
        val url = "http://lookup-service-prod.mlb.com/json/named.player_info.bam?sport_code='mlb'&player_id=" + player.id
        //log(s"Getting player data for $player from URL: $url")
        val json = parse(scala.io.Source.fromURL(url)("ISO-8859-1").mkString) \ "player_info" \ "queryResults"
        (json \ "totalSize").extract[String].toInt match {
          case 1   => (json \ "row").extract[PlayerData].toPlayerMLB
          case num => throw new Exception(s"Found $num records for player $player")
        }
      }
      if (player == newData) {
        unchanged += 1
        player
      } else {
        log(s"${player.toStringVerbose} has new data:\n\t${newData.toStringVerbose}")
        changed += 1
        newData
      }
    }
  }

  log(s"\nDONE --- $unchanged unchanged players, $changed changed players")

  if (changed > 0) {
    log("Updating player data file...")
    Player_MLB.savePlayersToFile(updatedPlayers.sortBy(_.id), true)
    log("DONE")
  }

}