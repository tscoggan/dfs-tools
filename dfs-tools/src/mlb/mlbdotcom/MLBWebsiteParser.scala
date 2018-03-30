package mlb.mlbdotcom

import scala.xml._
import java.util.Date
import utils.StringUtils._
import utils.DateTimeUtils._
import mlb._

/**
 * Retrieves data from the MLB.com website (gd2.mlb.com)
 */
object MLBWebsiteParser {

  type GameURL = String

  private val baseURL = Configs.MlbDotCom.baseURL

  def getGameURLs(date: Date): List[GameURL] = {
    val dateStr = getDateFormat("yyyy-MM-dd").format(date)
    val Array(year, month, day) = dateStr.split("-")
    val games = XML.load(baseURL + s"/components/game/mlb/year_$year/month_$month/day_$day/miniscoreboard.xml") \\ "game"
    games.flatMap(_.attribute("game_data_directory")).toList.flatten.map(g => baseURL + g.text + "/")
  }

  def getPlayerURLs(game: GameURL): List[String] = {
    val batters = scala.io.Source.fromURL(game + "batters/").mkString
    val pitchers = scala.io.Source.fromURL(game + "pitchers/").mkString
    batters.substringsBetween("a href=\"", "\">").filter(_.endsWith(".xml")).map(file => game + "batters/" + file) ++
      pitchers.substringsBetween("a href=\"", "\">").filter(_.endsWith(".xml")).map(file => game + "pitchers/" + file)
  }

}