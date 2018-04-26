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

  implicit def playerIDToPlayer(playerID: String): Player = Players.mlbDotComPlayersByID.get(playerID).flatMap(_.player) match {
    case Some(player) => player
    case None         => throw new Exception("Couldn't find player with MLB.com ID " + playerID)
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

  // all players who played in this game --- key = display name
  val visitingTeamPlayers: Map[String, PlayerGameStats] = {
    val teamXML = (rawBoxScoreXML \ "team").find(_.attribute("team_flag").head.text == "away").get
    val hittersXML = teamXML \ "batting" \ "batter"
    val pitchersXML = teamXML \ "pitching" \ "pitcher"

    val hitters = hittersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val batOrder = (p \ "@bat_order").text
      if (batOrder.length == 3) { // only include players who played
        Some((displayName, HitterGameStats(date, player.id, batOrder.tail == "00", batOrder.head.asDigit)))
      } else None
    }.toList

    val pitchers = pitchersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val pitchOrder = (p \ "@pitch_order").text
      val battingPosition = hitters.find { case (name, p) => p.playerID == player.id }.map { case (name, p) => p.battingPosition }.getOrElse(0)
      if (pitchOrder.length == 3) { // only include players who played
        Some((displayName, PitcherGameStats(date, player.id, pitchOrder == "100", battingPosition)))
      } else None
    }.toList

    (hitters ++ pitchers).toMap
  }
  val homeTeamPlayers: Map[String, PlayerGameStats] = {
    val teamXML = (rawBoxScoreXML \ "team").find(_.attribute("team_flag").head.text == "home").get
    val hittersXML = teamXML \ "batting" \ "batter"
    val pitchersXML = teamXML \ "pitching" \ "pitcher"

    val hitters = hittersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val batOrder = (p \ "@bat_order").text
      if (batOrder.length == 3) { // only include players who played
        Some((displayName, HitterGameStats(date, player.id, batOrder.tail == "00", batOrder.head.asDigit)))
      } else None
    }.toList

    val pitchers = pitchersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val pitchOrder = (p \ "@pitch_order").text
      val battingPosition = hitters.find { case (name, p) => p.playerID == player.id }.map { case (name, p) => p.battingPosition }.getOrElse(0)
      if (pitchOrder.length == 3) { // only include players who played
        Some((displayName, PitcherGameStats(date, player.id, pitchOrder == "100", battingPosition)))
      } else None
    }.toList

    (hitters ++ pitchers).toMap
  }

  def toGame: Game = Game(date, visitingTeam, homeTeam, gameNumber, homePlateUmpireID, winningPitcher, losingPitcher,
    savePitcher, visitingTeamPlayers.values.toList, homeTeamPlayers.values.toList)

}