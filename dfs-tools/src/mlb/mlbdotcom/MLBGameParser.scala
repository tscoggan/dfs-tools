package mlb.mlbdotcom

import scala.xml._
import mlb.model._
import mlb.model.CustomTypes._
import mlb._
import utils.Logger._
import utils.StringUtils._
import utils.DateTimeUtils._
import java.util.Date
import scala.collection.mutable

class MLBGameParser(eventsXML: Elem, rawBoxScoreXML: Elem, lineScoreXML: Elem) {

  implicit def playerIDToPlayer(playerID: String): Player = Players.mlbDotComPlayersByID(playerID).player match {
    case Some(player) => player
    case None => throw new Exception("Couldn't find player with MLB.com ID "+playerID)
  }

  //var bases: Bases = new Bases(this)

  var date: Date = (rawBoxScoreXML \ "@date").text.toDate("MMM dd, yyyy")

  var gameNumber: GameNumber = (rawBoxScoreXML \ "@game_id").text.substringAfterLast("-").toInt

  var homePlateUmpireID: String =
    (rawBoxScoreXML \ "umpires" \ "umpire").find(_.attribute("position").head.text == "HP").map(_.attribute("id").head.text).get

  var winningPitcher: Player =
    (lineScoreXML \ "winning_pitcher").find(_.attribute("id").head.text.trim.nonEmpty).map(_.attribute("id").head.text).get

  var losingPitcher: Player =
    (lineScoreXML \ "losing_pitcher").find(_.attribute("id").head.text.trim.nonEmpty).map(_.attribute("id").head.text).get

  var savePitcher: Option[Player] =
    (lineScoreXML \ "save_pitcher").find(_.attribute("id").head.text.trim.nonEmpty).map(_.attribute("id").head.text)

  var visitingTeam: Team =
    Teams.get { (rawBoxScoreXML \ "team").find(_.attribute("team_flag").head.text == "away").map(_.attribute("team_code").head.text).get }

  var homeTeam: Team =
    Teams.get { (rawBoxScoreXML \ "team").find(_.attribute("team_flag").head.text == "home").map(_.attribute("team_code").head.text).get }

  // players currently in the game
  val visitingTeamActivePlayers: mutable.Map[PlayerID, PlayerGameStats] = mutable.Map.empty
  val homeTeamActivePlayers: mutable.Map[PlayerID, PlayerGameStats] = mutable.Map.empty

  // current pitcher
  var visitingTeamPitcher: Option[PitcherGameStats] = None
  var homeTeamPitcher: Option[PitcherGameStats] = None

  // all players who played in this game
  val visitingTeamPlayers: mutable.ListBuffer[PlayerGameStats] = mutable.ListBuffer.empty
  val homeTeamPlayers: mutable.ListBuffer[PlayerGameStats] = mutable.ListBuffer.empty

  var inning: Int = -1
  var outsThisInning = 0
  var visitingOrHome: Int = -1

  def toGame: Game = Game(date, visitingTeam, homeTeam, gameNumber, homePlateUmpireID, winningPitcher, losingPitcher,
    savePitcher, visitingTeamPlayers.toList, homeTeamPlayers.toList)

}