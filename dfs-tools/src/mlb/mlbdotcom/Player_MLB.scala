package mlb.mlbdotcom

import mlb.model._
import mlb.model.CustomTypes._
import utils.StringUtils._
import utils.FileUtils._
import utils.DateTimeUtils._
import utils.Logger._
import mlb._
import java.util.Date

/**
 * Parsed from MLB.com XML
 */
case class Player_MLB(
    id: String,
    lastName: String,
    firstName: String,
    bats: Handedness,
    throws: Handedness,
    team: Team,
    position: Position,
    retrosheetID: Option[PlayerID] = None) {

  val name: String = s"$firstName $lastName"

  val player: Option[Player] = retrosheetID match {
    case Some(rsID) =>
      Players.retrosheetPlayers.find(_.id == rsID).orElse {
        Players.newPlayers.find(_.id == rsID)
      }
    case None =>
      Players.playerMappings.find(_.mlbPlayerID == id) match {
        case Some(mapping) =>
          Players.retrosheetPlayers.find(_.id == mapping.retrosheetID).orElse {
            Players.newPlayers.find(_.id == mapping.retrosheetID)
          }
        case None =>
          Players.retrosheetPlayers.find(p => p.name.toUpperCase == name.toUpperCase).orElse { // && p.team == team).orElse {
            Players.newPlayers.find(p => p.name.toUpperCase == name.toUpperCase && p.team == team)
          }
      }
  }

  def toCSV: String = List(id, lastName, firstName, bats, throws, team, position, player.map(_.id).getOrElse("???")).mkString("|")

  override def toString: String = s"MLB[$name ($position, $team)]"

  def toStringVerbose: String = s"Player_MLB[id=$id, name=$name, team=$team, position=$position, bats=$bats, throws=$throws, " +
    s"retrosheetID=${retrosheetID.getOrElse("???")}, player=${player.map(_.toString()).getOrElse("???")}]"
}

object Player_MLB {

  import scala.xml._

  def parseFrom(url: String): Player_MLB = {
    val xml = XML.load(url)
    val id = (xml \ "@id").text
    val lastName = (xml \ "@last_name").text
    val firstName = (xml \ "@first_name").text
    val bats = (xml \ "@bats").text
    val throws = (xml \ "@throws").text
    val teamID = (xml \ "@team").text.toUpperCase
    val position = (xml \ "@pos").text
    val playerType = (xml \ "@type").text

    //log(s"teamID: $teamID, id: $id, firstName: $firstName, lastName: $lastName, position: $position, bats: $bats, throws: $throws")

    Player_MLB(id, lastName, firstName, bats, throws, Teams.get(teamID), position)
  }

  private def parseFromCSV(csv: String): Player_MLB = {
    val Array(id, lastName, firstName, bats, throws, team, position, retrosheetID) = csv.split("\\|")
    val rsID = retrosheetID.trim match {
      case "???" | "" => None
      case validID    => Some(validID)
    }
    Player_MLB(id, lastName, firstName, bats, throws, Teams.get(team), position, rsID)
  }

  private val playersFileName = s"${Configs.MlbDotCom.dataFileDir}/players.txt"
  private val loadedThroughfileName = s"${Configs.MlbDotCom.dataFileDir}/players_loaded_through.txt"

  def saveToFile(players: List[Player_MLB], loadedThrough: Date, overwrite: Boolean = false): Unit = {
    writeLinesToFile(players.map(_.toCSV), playersFileName, overwrite)
    writeToFile(loadedThrough.print("yyyy-MM-dd"), loadedThroughfileName, true)
    log(s"Saved ${players.length} MLB.com players to file ${if (overwrite) "--- overwrote existing file" else ""}")
  }

  def playersLoadedThrough: Option[Date] = fileExists(loadedThroughfileName) match {
    case false => None
    case true  => scala.io.Source.fromFile(loadedThroughfileName).getLines.toList.filter(_.trim.nonEmpty).headOption.map(_.toDate("yyyy-MM-dd"))
  }

  def loadFromFile: List[Player_MLB] = fileExists(playersFileName) match {
    case false => Nil
    case true =>
      val players = scala.io.Source.fromFile(playersFileName).getLines.toList.filter(_.trim.nonEmpty).map(parseFromCSV(_))
      log(s"Loaded ${players.length} MLB.com players from file")
      players
  }

}