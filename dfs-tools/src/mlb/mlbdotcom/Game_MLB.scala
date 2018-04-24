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

  def getGameURLs(date: Date): List[String] = {
    val dateStr = getDateFormat("yyyy-MM-dd").format(date)
    val Array(year, month, day) = dateStr.split("-")
    val games = XML.load(baseURL + s"/components/game/mlb/year_$year/month_$month/day_$day/miniscoreboard.xml") \\ "game"
    games.flatMap(_.attribute("game_data_directory")).toList.flatten.map(g => baseURL + g.text + "/")
  }

  def parseFrom(url: String): Try[Game_MLB] = Try {
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
    
    Game_MLB()
  }

  private val gamesRootDir = s"${Configs.MlbDotCom.dataFileDir}/games"
  private val loadedThroughfileName = s"${Configs.MlbDotCom.dataFileDir}/games_loaded_through.txt"

  def gamesLoadedThrough: Option[Date] = fileExists(loadedThroughfileName) match {
    case false => None
    case true  => scala.io.Source.fromFile(loadedThroughfileName).getLines.toList.filter(_.trim.nonEmpty).headOption.map(_.toDate("yyyy-MM-dd"))
  }

  //  def loadFromFile: List[Game_MLB] = fileExists(playersFileName) match {
  //    case false => Nil
  //    case true =>
  //      val players = scala.io.Source.fromFile(playersFileName).getLines.toList.filter(_.trim.nonEmpty).map(parseFromCSV(_))
  //      log(s"Loaded ${players.length} MLB.com players from file")
  //      players
  //  }

}