package mlb.mlbdotcom

import mlb.model._
import mlb.model.CustomTypes._
import utils.StringUtils._
import utils.FileUtils._
import utils.DateTimeUtils._
import utils.Logger._
import mlb._
import java.util.Date
import scala.util.{ Try, Success, Failure }
import scala.xml._

object Game_MLB {

  private val baseURL = Configs.MlbDotCom.baseURL

  private val gamesRootDir = s"${Configs.MlbDotCom.dataFileDir}/games"
  private val loadedThroughfileName = s"${Configs.MlbDotCom.dataFileDir}/games_loaded_through.txt"

  lazy val thisSeasonGames: List[Game] = loadGamesForDateRange(Configs.MlbDotCom.seasonStartDate, yesterday)

  lazy val past1YearGames: List[Game] = {
    log(s"Loading MLB games from ${oneYearAgo.print()} to ${yesterday.print()}")
    loadGamesForDateRange(oneYearAgo, Configs.MlbDotCom.lastSeasonEndDate) ++ loadGamesForDateRange(Configs.MlbDotCom.seasonStartDate, yesterday)
  }

  lazy val gamesSinceStartOfLastSeason: List[Game] = {
    log(s"Loading MLB games from ${Configs.MlbDotCom.lastSeasonStartDate} to ${yesterday.print()}")
    loadGamesForDateRange(Configs.MlbDotCom.lastSeasonStartDate, Configs.MlbDotCom.lastSeasonEndDate) ++ // last season
      loadGamesForDateRange(Configs.MlbDotCom.seasonStartDate, yesterday) // current season
  }

  def loadGamesForDateRange(from: Date, to: Date): List[Game] = getDatesBetween(from, to) flatMap { date =>
    val dayDir = s"${gamesRootDir}/year_${date.print("yyyy")}/month_${date.print("MM")}/day_${date.print("dd")}"
    if (fileExists(dayDir)) {
      val gameDirs = getSubdirectories(dayDir)
      log(s"Found ${gameDirs.length} games for ${date.print()}")
      if (Configs.MlbDotCom.runSanityChecks || date.trimTime == yesterday.trimTime || date.trimTime == today.trimTime) {
        if (getGameURLs(date).length != gameDirs.length) throw new Exception(s"Should have found ${getGameURLs(date).length} games for ${date.print()}")
        else log("...correct # of games")
      }
      gameDirs.flatMap { dir => loadGameFromFile(dir.toString) }
    } else {
      log(s"Downloading ${date.print()} games from MLB.com website")
      getGameURLs(date).flatMap { url => loadGameFromURL(url) }
    }
  }

  def getGameURLs(date: Date): List[String] = {
    val dateStr = getDateFormat("yyyy-MM-dd").format(date)
    val Array(year, month, day) = dateStr.split("-")
    val games = XML.load(baseURL + s"/components/game/mlb/year_$year/month_$month/day_$day/miniscoreboard.xml") \\ "game"
    games.flatMap(_.attribute("game_data_directory")).toList.flatten.map(g => baseURL + g.text + "/")
      .filterNot(_.contains("aasmlb")) // ignore all-star games
  }

  def loadGameFromURL(url: String): Option[Game] = {
    val eventsFileName = s"${Configs.MlbDotCom.dataFileDir}/games/${url.substringAfter("game/mlb/")}game_events.xml"
    val boxScoreFileName = s"${Configs.MlbDotCom.dataFileDir}/games/${url.substringAfter("game/mlb/")}boxscore.xml"
    val lineScoreFileName = s"${Configs.MlbDotCom.dataFileDir}/games/${url.substringAfter("game/mlb/")}linescore.xml"

    try {

      val lineScoreXML = fileExists(lineScoreFileName) match {
        case false =>
          val xml = XML.load(url + "linescore.xml")
          writeToFile(xml.toString, lineScoreFileName, true)
          log(s"Downloaded game line score: $lineScoreFileName")
          xml
        case true => XML.loadFile(lineScoreFileName)
      }

      (lineScoreXML \ "@status").text match {
        case "Postponed" | "Suspended" | "In Progress" | "Cancelled" => None
        case "Final" | "Completed Early" | "Completed Early: Rain" | "Game Over" => {
          val eventsXML = fileExists(eventsFileName) match {
            case false =>
              val xml = XML.load(url + "game_events.xml")
              writeToFile(xml.toString, eventsFileName, true)
              log(s"Downloaded game events: $eventsFileName")
              xml
            case true => XML.loadFile(eventsFileName)
          }
          val boxScoreXML = fileExists(boxScoreFileName) match {
            case false =>
              val xml = XML.load(url + "boxscore.xml")
              writeToFile(xml.toString, boxScoreFileName, true)
              log(s"Downloaded game box score: $boxScoreFileName")
              xml
            case true => XML.loadFile(boxScoreFileName)
          }

          Some((new MLBGameParser(eventsXML, boxScoreXML, lineScoreXML)).toGame)
        }
        case other => throw new Exception(s"Unknown game status in $lineScoreFileName")
      }

    } catch {
      case e: java.io.FileNotFoundException =>
        log("WARNING: Game files not found for " + url)
        None
    }
  }

  def loadGameFromFile(gameDirectory: String): Option[Game] = {
    val eventsFileName = s"${gameDirectory.trimSuffix("/")}/game_events.xml"
    val boxScoreFileName = s"${gameDirectory.trimSuffix("/")}/boxscore.xml"
    val lineScoreFileName = s"${gameDirectory.trimSuffix("/")}/linescore.xml"

    try {

      val lineScoreXML = XML.loadFile(lineScoreFileName)

      (lineScoreXML \ "@status").text match {
        case "Postponed" | "Suspended" | "Cancelled" => None
        case "In Progress"             => throw new Exception(s"Tried to load 'In Progress' game from file --- need to re-load from URL: " + gameDirectory)
        case "Final" | "Completed Early" | "Completed Early: Rain" => {
          val eventsXML = XML.loadFile(eventsFileName)
          val boxScoreXML = XML.loadFile(boxScoreFileName)
          Some((new MLBGameParser(eventsXML, boxScoreXML, lineScoreXML)).toGame)
        }
        case other => throw new Exception(s"Unknown game status in $lineScoreFileName")
      }

    } catch {
      case e: java.io.FileNotFoundException =>
        log("WARNING: Game files not found for " + gameDirectory)
        None
    }
  }

}