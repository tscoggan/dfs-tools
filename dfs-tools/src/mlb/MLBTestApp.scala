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

  val hitter = Players.get("456715")
  println("hitter: "+hitter)
  
  val stats = season.hitterFptsPerAB_vs_PitcherType(mlb.model.Left, hitter, FanDuelMLB)
  stats.foreach { s => println("PA: "+s.atBats+", FPTS/PA: "+s.fptsPerAB) }

}