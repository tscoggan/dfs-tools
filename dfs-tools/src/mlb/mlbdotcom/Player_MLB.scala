package mlb.mlbdotcom

import mlb.model._
import mlb.model.CustomTypes._
import utils.StringUtils._
import utils.FileUtils._
import utils.DateTimeUtils._
import utils.Logger._
import mlb._
import java.util.Date
import scala.xml._

/**
 * Parsed from MLB.com XML
 */
case class Player_MLB(
    id: MLBPlayerID,
    lastName: String,
    firstName: String,
    bats: Handedness,
    throws: Handedness,
    team: Team,
    position: Position) {

  val name: String = s"$firstName $lastName"
  
  val alternateName = name.substringBefore(" Jr.").replaceAll("\\.", "").trim

  def toCSV: String = List(id, lastName, firstName, bats, throws, team, position).mkString("|")

  override def toString: String = s"MLB[$name ($position, $team)]"

  def toStringVerbose: String = s"Player_MLB[id=$id, name=$name, team=$team, position=$position, bats=$bats, throws=$throws]"
}

object Player_MLB {

  private val playersFileName = s"${Configs.MlbDotCom.dataFileDir}/players.txt"
  private val loadedThroughfileName = s"${Configs.MlbDotCom.dataFileDir}/players_loaded_through.txt"

  private val baseURL = Configs.MlbDotCom.baseURL

  val allPlayers: List[Player_MLB] = {
    val existingPlayers = loadPlayersFromFile
    val datesToLoad = {
      playersLoadedThrough match {
        case Some(lastLoadDate) => getDatesBetween(lastLoadDate.nextDay, yesterday)
        case None               => getDatesBetween(oneYearAgo, Configs.MlbDotCom.lastSeasonEndDate) ++ getDatesBetween(Configs.MlbDotCom.seasonStartDate, yesterday)
      }
    }.sorted
    log(s"Loading MLB.com players for games on ${datesToLoad.map(_.print("yyyy-MM-dd")).mkString(", ")}")
    val playerURLs = datesToLoad.flatMap(Game_MLB.getGameURLs(_)).flatMap(getPlayerURLs(_))
      .groupBy(_.substringAfterLast("/")).toList.sortBy(_._1).map {
        case (playerID, urls) =>
          urls.sortBy(_.substringAfter("gid_").take(10).toDate("yyyy_MM_dd")).last // load player from most recent URL to ensure current team
      }
    log(s"Found ${playerURLs.length} distinct player URL's")
    val newPlayers = playerURLs.filter(url => !existingPlayers.exists(_.id == url.substringAfterLast("/").substringBefore(".")))
      .flatMap(loadPlayerFromURL(_)).distinct
    log(s"...found ${newPlayers.length} new players and ${existingPlayers.length} existing players")
    if (newPlayers.nonEmpty) savePlayersToFile(newPlayers, false) // save new players to file
    writeToFile(yesterday.print("yyyy-MM-dd"), loadedThroughfileName, true)
    (existingPlayers ++ newPlayers).distinct
  }.sortBy(_.id)

  def getPlayerURLs(gameURL: String): List[String] = try {
    println("Getting player URL's for game " + gameURL)
    val batters = scala.io.Source.fromURL(gameURL + "batters/").mkString
    val pitchers = scala.io.Source.fromURL(gameURL + "pitchers/").mkString
    batters.substringsBetween("a href=\"", "\">").filter(_.endsWith(".xml")).map(file => gameURL + "batters/" + file) ++
      pitchers.substringsBetween("a href=\"", "\">").filter(_.endsWith(".xml")).map(file => gameURL + "pitchers/" + file)
  } catch {
    case e: java.io.FileNotFoundException => Nil
  }

  def loadPlayerFromURL(url: String): Option[Player_MLB] = try {
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

    Some(Player_MLB(id, lastName, firstName, bats, throws, Teams.get(teamID), position))
  } catch {
    case e: Exception =>
      log(s"Error loading player from $url: ${e.getMessage}")
      None
  }

  private def loadPlayerFromCSV(csv: String): Player_MLB = {
    val Array(id, lastName, firstName, bats, throws, team, position) = csv.split("\\|")
    Player_MLB(id, lastName, firstName, bats, throws, Teams.get(team), position)
  }

  def savePlayersToFile(players: List[Player_MLB], overwrite: Boolean = false): Unit = {
    writeLinesToFile(players.map(_.toCSV), playersFileName, overwrite)
    log(s"Saved ${players.length} MLB.com players to file ${if (overwrite) "--- overwrote existing file" else ""}")
  }

  def playersLoadedThrough: Option[Date] = fileExists(loadedThroughfileName) match {
    case false => None
    case true  => scala.io.Source.fromFile(loadedThroughfileName).getLines.toList.filter(_.trim.nonEmpty).headOption.map(_.toDate("yyyy-MM-dd"))
  }

  def loadPlayersFromFile: List[Player_MLB] = fileExists(playersFileName) match {
    case false => Nil
    case true =>
      val players = scala.io.Source.fromFile(playersFileName).getLines.toList.filter(_.trim.nonEmpty).map(loadPlayerFromCSV(_))
      log(s"Loaded ${players.length} MLB.com players from file")
      players
  }

}