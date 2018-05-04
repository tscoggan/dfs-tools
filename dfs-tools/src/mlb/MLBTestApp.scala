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

  import mlb.Past1YearStats.stats._
  mlb.Past1YearStats.stats.logSummary

  println("#### SAME GAME ####")
  
  val avgFPTS_pitchersSameGame = utils.MathUtils.mean {
    season.games.map { game => List(game.statsFor(game.visitingTeamStartingPitcher).get, game.statsFor(game.homeTeamStartingPitcher).get) }
      .map {
        case pitchers => 
          pitchers.map(_.asInstanceOf[PitcherGameStats].pitchingStats.fantasyPoints(DraftKingsMLB)).sum
      }
  }

  println("#### DIFF GAME ####")
  
  val avgFPTS_pitchersDiffGames = utils.MathUtils.mean {
    season.games.groupBy(_.date).flatMap {
      case (date, games) =>
        val pitchers = games.flatMap { game => List(game.statsFor(game.visitingTeamStartingPitcher).get, game.statsFor(game.homeTeamStartingPitcher).get) }
        val pairs = pitchers.combinations(2).filter { _.map(_.game.get.alias).distinct.length > 1 } // must be in different games
        pairs.map {
          case pitchers => 
            pitchers.map(_.asInstanceOf[PitcherGameStats].pitchingStats.fantasyPoints(DraftKingsMLB)).sum
        }
    }
  }

  println(s"Same game: ${avgFPTS_pitchersSameGame.rounded(2)} FPTS per pair of pitchers")
  println(s"Different games: ${avgFPTS_pitchersDiffGames.rounded(2)} FPTS per pair of pitchers")

}