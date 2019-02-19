package mlb

import mlb._
import mlb.model._
import mlb.model.CustomTypes._
import mlbdotcom._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._

object Season2018Stats {
  
  val allStarGameDate = "2018-07-17".toDate("yyyy-MM-dd")

  val games = Game_MLB.loadGamesForDateRange("2018-03-29".toDate("yyyy-MM-dd"), "2018-10-01".toDate("yyyy-MM-dd"))

  val season = Season("2018", games)

  val season1stHalf = Season("2018 H1", games.filter(_.date.before(allStarGameDate)))

  val season2ndHalf = Season("2018 H2", games.filter(_.date.after(allStarGameDate)))

  log(s"Finished loading ${season.label} season: ${games.length} games --- ${season2ndHalf.games.length} games after All-Star break")
  
  val stats = HistoricalStats(season)

}