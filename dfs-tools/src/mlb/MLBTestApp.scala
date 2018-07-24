package mlb

//import retrosheet._
import mlbdotcom._
import model._
import utils._
import utils.StringUtils._
import utils.DateTimeUtils._
import utils.DoubleUtils._
import scala.util.{ Try, Success, Failure }

object MLBTestApp extends App {

  //    val games = Game_MLB.loadGamesForDateRange(yesterday, yesterday)
  //  println(s"Found ${games.length} games")
  //  games.foreach { g => println(g + "\n\n") }

  import mlb.StatsSinceStartOfLastSeason.stats._
  mlb.StatsSinceStartOfLastSeason.stats.logSummary

  val values = season.allPitchers.filter(_.gamesStartedAsPitcher.length >= 10).flatMap { pitcher =>
    val gamesStarted = pitcher.gamesStartedAsPitcher.length
    val numberOfPA = pitcher.pitchingPA_asStarter
    val fptsPerPA_FD = pitcher.pitcherFptsPerPA_asStarter(FanDuelMLB)
    val fptsPerPA_DK = pitcher.pitcherFptsPerPA_asStarter(DraftKingsMLB)
    val fptsAllowedPerPA_FD = pitcher.pitcherFptsAllowedPerPA_asStarter(FanDuelMLB)
    val fptsAllowedPerPA_DK = pitcher.pitcherFptsAllowedPerPA_asStarter(DraftKingsMLB)
    List(s"${pitcher.player.toStringTeamOnly},$gamesStarted,$numberOfPA,${fptsPerPA_FD.rounded(2)},${fptsPerPA_DK.rounded(2)},${fptsAllowedPerPA_FD.rounded(2)},${fptsAllowedPerPA_DK.rounded(2)}")
  }

  FileUtils.writeLinesToFile("Pitcher,Games Started,# Plate Appearances, Pitcher FPTS/PA (FD), Pitcher FPTS/PA (DK), Hitter FPTS/PA Allowed (FD), Hitter FPTS/PA Allowed (DK)" :: values,
    "C:/Users/Tom/Desktop/temp.csv",
    true)

}