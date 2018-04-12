package mlb

import mlb._
import mlb.model._
import mlb.model.CustomTypes._
import mlb.retrosheet._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.MathUtils._
import utils.StringUtils._

object Season2017Stats {
  
  val allStarGameDate = "2017-07-11".toDate("yyyy-MM-dd")

  val games = FileUtils.getListOfFiles(Configs.Retrosheet.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

  val season = Season("2017", games)

  val season1stHalf = Season("2017 H1", games.filter(_.date.before(allStarGameDate)))

  val season2ndHalf = Season("2017 H2", games.filter(_.date.after(allStarGameDate)))

  log(s"Finished loading ${season.year} season: ${games.length} games --- ${season2ndHalf.games.length} games after All-Star break")
  
  val stats = HistoricalStats(season)

}