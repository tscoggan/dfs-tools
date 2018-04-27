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
import scala.annotation.tailrec

class MLBGameParser(eventsXML: Elem, rawBoxScoreXML: Elem, lineScoreXML: Elem) {

  implicit def playerIDToPlayer(playerID: MLBPlayerID): Player = Players.mlbDotComPlayersByID.get(playerID).flatMap(_.player) match {
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

  // all players who played in this game
  val visitingTeamPlayers: Map[MLBPlayerID, PlayerGameStats] = {
    val teamXML = (rawBoxScoreXML \ "team").find(_.attribute("team_flag").head.text == "away").get
    val hittersXML = teamXML \ "batting" \ "batter"
    val pitchersXML = teamXML \ "pitching" \ "pitcher"

    val hitters = hittersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val batOrder = (p \ "@bat_order").text
      if (batOrder.length == 3) { // only include players who played
        Some((mlbPlayerID, HitterGameStats(date, player.id, batOrder.tail == "00", batOrder.head.asDigit)))
      } else None
    }.toList

    val pitchers = pitchersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val pitchOrder = (p \ "@pitch_order").text
      val battingPosition = hitters.find { case (name, p) => p.playerID == player.id }.map { case (name, p) => p.battingPosition }.getOrElse(0)
      if (pitchOrder.length == 3) { // only include players who played
        Some((mlbPlayerID, PitcherGameStats(date, player.id, pitchOrder == "100", battingPosition)))
      } else None
    }.toList

    (hitters ++ pitchers).toMap
  }
  val homeTeamPlayers: Map[MLBPlayerID, PlayerGameStats] = {
    val teamXML = (rawBoxScoreXML \ "team").find(_.attribute("team_flag").head.text == "home").get
    val hittersXML = teamXML \ "batting" \ "batter"
    val pitchersXML = teamXML \ "pitching" \ "pitcher"

    val hitters = hittersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val batOrder = (p \ "@bat_order").text
      if (batOrder.length == 3) { // only include players who played
        Some((mlbPlayerID, HitterGameStats(date, player.id, batOrder.tail == "00", batOrder.head.asDigit)))
      } else None
    }.toList

    val pitchers = pitchersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val pitchOrder = (p \ "@pitch_order").text
      val battingPosition = hitters.find { case (name, p) => p.playerID == player.id }.map { case (name, p) => p.battingPosition }.getOrElse(0)
      if (pitchOrder.length == 3) { // only include players who played
        Some((mlbPlayerID, PitcherGameStats(date, player.id, pitchOrder == "100", battingPosition)))
      } else None
    }.toList

    (hitters ++ pitchers).toMap
  }

  val visitingTeamEventsXML = (eventsXML \ "inning" \ "top" \ "_")
  val homeTeamEventsXML = (eventsXML \ "inning" \ "bottom" \ "_")

  MLBGameParser.parseEvents(visitingTeamEventsXML, visitingTeamPlayers ++ homeTeamPlayers)
  MLBGameParser.parseEvents(homeTeamEventsXML, visitingTeamPlayers ++ homeTeamPlayers)

  def toGame: Game = Game(date, visitingTeam, homeTeam, gameNumber, homePlateUmpireID, winningPitcher, losingPitcher,
    savePitcher, visitingTeamPlayers.values.toList, homeTeamPlayers.values.toList)

}

object MLBGameParser {

  @tailrec
  def parseEvents(remainingEventsXML: NodeSeq, players: Map[MLBPlayerID, PlayerGameStats], previousOuts: Int = 0): Unit = remainingEventsXML.headOption match {
    case Some(eventXML) =>
      val event = (eventXML \ "@event").text
      val outs = (eventXML \ "@o").text.toInt
      val description = (eventXML \ "@des").text

      eventXML.toString.substringBefore(" ").tail match {
        case "atbat" =>
          val hitter = players((eventXML \ "@batter").text)
          val pitcher = players((eventXML \ "@pitcher").text)
          
          //println(s"hitter: $hitter, pitcher: $pitcher")
          
          event match {
            case "Batter Interference"  => //???
            case "Bunt Groundout"       => //???
            case "Bunt Lineout"         => //???
            case "Bunt Pop Out"         => //???
            case "Catcher Interference" => //???
            case "Double"               => //???
            case "Double Play"          => //???
            case "Fan interference"     => //???
            case "Field Error"          => //???
            case "Fielders Choice"      => //???
            case "Fielders Choice Out"  => //???
            case "Flyout"               => //???
            case "Forceout"             => //???
            case "Grounded Into DP"     => //???
            case "Groundout"            => //???
            case "Hit By Pitch"         => //???
            case "Home Run"             => //???
            case "Intent Walk"          => //???
            case "Lineout"              => //???
            case "Pop Out"              => //???
            case "Runner Out"           => //???
            case "Sac Bunt"             => //???
            case "Sac Fly"              => //???
            case "Sac Fly DP"           => //???
            case "Single"               => //???
            case "Strikeout"            => //???
            case "Strikeout - DP"       => //???
            case "Triple"               => //???
            case "Triple Play"          => //???
            case "Walk"                 => //???
          }
        case "action" =>
          event match {
            case "Balk"                     => //???
            case "Caught Stealing 2B"       => //???
            case "Caught Stealing 3B"       => //???
            case "Caught Stealing Home"     => //???
            case "Defensive Indiff"         => //???
            case "Defensive Sub"            => //???
            case "Defensive Switch"         => //???
            case "Ejection"                 => //???
            case "Error"                    => //???
            case "Game Advisory"            => //???
            case "Manager Review"           => //???
            case "Offensive Sub"            => //???
            case "Passed Ball"              => //???
            case "Picked off stealing 2B"   => //???
            case "Picked off stealing 3B"   => //???
            case "Picked off stealing home" => //???
            case "Pickoff 1B"               => //???
            case "Pickoff 2B"               => //???
            case "Pickoff 3B"               => //???
            case "Pickoff Error 1B"         => //???
            case "Pickoff Error 2B"         => //???
            case "Pitching Substitution"    => //???
            case "Player Injured"           => //???
            case "Runner Advance"           => //???
            case "Runner Out"               => //???
            case "Stolen Base 2B"           => //???
            case "Stolen Base 3B"           => //???
            case "Stolen Base Home"         => //???
            case "Umpire Review"            => //???
            case "Umpire Substitution"      => //???
            case "Wild Pitch"               => //???
          }
      }

      //println(s"\nevent: $event, outs: $outs, description: $description")

      parseEvents(remainingEventsXML.tail, players, outs)
    case None => // done
  }

}