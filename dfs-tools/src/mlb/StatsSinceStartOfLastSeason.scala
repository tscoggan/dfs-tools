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
import utils.DateTimeUtils._

object StatsSinceStartOfLastSeason {

  val games = Game_MLB.gamesSinceStartOfLastSeason

  val season = Season("Last Season-to-Current", games)

  log(s"Finished loading ${season.label} games: ${games.length} games " +
      s"--- ${games.filter(_.date.after(Configs.MlbDotCom.seasonStartDate.previousDay)).length} games from this season")
  
  val stats = HistoricalStats(season)

}