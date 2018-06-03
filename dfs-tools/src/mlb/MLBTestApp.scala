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

  import mlb.StatsPast1Year.stats._
  mlb.StatsPast1Year.stats.logSummary

  val values = season.games.flatMap{ game =>
    val visitingTeamPAs = game.visitingTeamPlayerStats.map(_.hittingStats.atBats).sum
    val homeTeamPAs = game.homeTeamPlayerStats.map(_.hittingStats.atBats).sum
    List(s"${game.visitingTeamRuns},$visitingTeamPAs,${(visitingTeamPAs.toDouble / 9.0).rounded(1)},Away",
        s"${game.homeTeamRuns},$homeTeamPAs,${(homeTeamPAs.toDouble / 9.0).rounded(1)},Home")
  }

  FileUtils.writeLinesToFile("Team Runs,Total PAs, Times Through Batting Order,Home/Away" :: values,
    "C:/Users/Tom/Desktop/temp.csv",
    true)

}