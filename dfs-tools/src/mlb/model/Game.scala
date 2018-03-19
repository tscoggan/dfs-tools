package mlb.model

import java.util.Date
import utils.DateTimeUtils._
import CustomTypes._
import GameNumbers._
import WindDirections._

case class Game(
    date: Date,
    visitingTeam: Team,
    homeTeam: Team,
    gameNumber: GameNumber,
    dayGame: Boolean,
    usedDesignatedHitter: Boolean,
    homePlateUmpireID: String,
    temperature: Option[Int],
    windDirection: WindDirection,
    windSpeed: Option[Int],
    precipitation: String,
    winningPitcher: Player,
    losingPitcher: Player,
    savePitcher: Option[Player],
    visitingTeamPlayerStats: List[PlayerGameStats],
    homeTeamPlayerStats: List[PlayerGameStats]) {

  override def toString: String = s"$visitingTeam @ $homeTeam (${date.print()}${if (gameNumber != SINGLE_GAME) s" game $gameNumber" else ""})" +
    s"\n  $visitingTeam stats:\n\t" + visitingTeamPlayerStats.sortBy(p => p.battingPosition + "" + (20 - p.atBats)).map(_.printStats).mkString("\n\t") +
    s"\n  $homeTeam stats:\n\t" + homeTeamPlayerStats.sortBy(p => p.battingPosition + "" + (20 - p.atBats)).map(_.printStats).mkString("\n\t")
}

object GameNumbers {
  val SINGLE_GAME = 0
  val FIRST_OF_DOUBLEHEADER = 1
  val SECOND_OF_DOUBLEHEADER = 2
}

object WindDirections {
  val FROM_CENTER_FIELD = "fromcf"
  val FROM_LEFT_FIELD = "fromlf"
  val FROM_RIGHT_FIELD = "fromrf"
  val LEFT_TO_RIGHT = "ltor"
  val RIGHT_TO_LEFT = "rtol"
  val TO_CENTER_FIELD = "tocf"
  val TO_LEFT_FIELD = "tolf"
  val TO_RIGHT_FIELD = "torf"
  val UNKNOWN = "unknown"
}