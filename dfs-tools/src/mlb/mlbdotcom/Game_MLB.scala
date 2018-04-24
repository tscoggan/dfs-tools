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

/**
 * Parsed from MLB.com XML
 */
case class Game_MLB() {

}

object Game_MLB {

  private val baseURL = Configs.MlbDotCom.baseURL

  private val gamesRootDir = s"${Configs.MlbDotCom.dataFileDir}/games"
  private val loadedThroughfileName = s"${Configs.MlbDotCom.dataFileDir}/games_loaded_through.txt"

  val allGames: List[Game] = loadGamesForDateRange(Configs.MlbDotCom.seasonStartDate, yesterday)

  def loadGamesForDateRange(from: Date, to: Date): List[Game] = getDatesBetween(from, to) flatMap { date =>
    val dayDir = s"${gamesRootDir}/year_${date.print("yyyy")}/month_${date.print("MM")}/day_${date.print("dd")}"
    if (fileExists(dayDir)) {
      val gameDirs = getSubdirectories(dayDir)
      log(s"Found ${gameDirs.length} games for ${date.print()}")
      gameDirs.flatMap { dir =>
        loadGameFromFile(dir.toString) match {
          case Success(game) => List(game)
          case _             => Nil
        }
      }
    } else {
      log(s"Downloading ${date.print()} games from MLB.com website")
      getGameURLs(date).flatMap { url =>
        loadGameFromURL(url) match {
          case Success(game) => List(game)
          case _             => Nil
        }
      }
    }
  }

  def getGameURLs(date: Date): List[String] = {
    val dateStr = getDateFormat("yyyy-MM-dd").format(date)
    val Array(year, month, day) = dateStr.split("-")
    val games = XML.load(baseURL + s"/components/game/mlb/year_$year/month_$month/day_$day/miniscoreboard.xml") \\ "game"
    games.flatMap(_.attribute("game_data_directory")).toList.flatten.map(g => baseURL + g.text + "/")
  }

  def loadGameFromURL(url: String): Try[Game] = Try {
    val eventsFileName = s"${Configs.MlbDotCom.dataFileDir}/games/${url.substringAfter("game/mlb/")}game_events.xml"
    val rawBoxScoreFileName = s"${Configs.MlbDotCom.dataFileDir}/games/${url.substringAfter("game/mlb/")}rawboxscore.xml"

    val eventsXML = fileExists(eventsFileName) match {
      case false =>
        val xml = XML.load(url + "game_events.xml")
        writeToFile(xml.toString, eventsFileName, true)
        log(s"Downloaded game events: $eventsFileName")
        xml
      case true => XML.loadFile(eventsFileName)
    }
    val rawBoxScoreXML = fileExists(rawBoxScoreFileName) match {
      case false =>
        val xml = XML.load(url + "rawboxscore.xml")
        writeToFile(xml.toString, rawBoxScoreFileName, true)
        log(s"Downloaded game box score: $rawBoxScoreFileName")
        xml
      case true => XML.loadFile(rawBoxScoreFileName)
    }

    (new MLBGameParser(eventsXML, rawBoxScoreXML)).toGame
  }

  def loadGameFromFile(gameDirectory: String): Try[Game] = Try {
    val eventsFileName = s"${gameDirectory.trimSuffix("/")}/game_events.xml"
    val rawBoxScoreFileName = s"${gameDirectory.trimSuffix("/")}/rawboxscore.xml"

    val eventsXML = XML.loadFile(eventsFileName)
    val rawBoxScoreXML = XML.loadFile(rawBoxScoreFileName)

    (new MLBGameParser(eventsXML, rawBoxScoreXML)).toGame
  }

}