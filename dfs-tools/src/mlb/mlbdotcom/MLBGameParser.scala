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

  type HomeOrAway = Int
  private val VISITING_TEAM: HomeOrAway = 0
  private val HOME_TEAM: HomeOrAway = 1

  implicit def playerIDToPlayer(playerID: MLBPlayerID): Player = Players.mlbDotComPlayersByID.get(playerID).flatMap(_.player) match {
    case Some(player) => player
    case None         => throw new Exception("Couldn't find player with MLB.com ID " + playerID)
  }

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

  logDebug(s"### ${date.print()} $visitingTeam @ $homeTeam ###")

  val playerDisplayNames: mutable.Map[PlayerID, String] = mutable.Map.empty
  val mlbPlayerIdByDisplayName: mutable.Map[(String, HomeOrAway), MLBPlayerID] = mutable.Map.empty

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
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((displayName, VISITING_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((displayName.substringAfterLast(" "), VISITING_TEAM) -> mlbPlayerID)
        if (displayName.endsWith(" Jr.")) mlbPlayerIdByDisplayName += ((displayName.trimSuffix(" Jr."), VISITING_TEAM) -> mlbPlayerID)

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
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((displayName, VISITING_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((displayName.substringAfterLast(" "), VISITING_TEAM) -> mlbPlayerID)
        if (displayName.endsWith(" Jr.")) mlbPlayerIdByDisplayName += ((displayName.trimSuffix(" Jr."), VISITING_TEAM) -> mlbPlayerID)

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
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((displayName, HOME_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((displayName.substringAfterLast(" "), HOME_TEAM) -> mlbPlayerID)
        if (displayName.endsWith(" Jr.")) mlbPlayerIdByDisplayName += ((displayName.trimSuffix(" Jr."), HOME_TEAM) -> mlbPlayerID)

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
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((displayName, HOME_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((displayName.substringAfterLast(" "), HOME_TEAM) -> mlbPlayerID)
        if (displayName.endsWith(" Jr.")) mlbPlayerIdByDisplayName += ((displayName.trimSuffix(" Jr."), HOME_TEAM) -> mlbPlayerID)

        Some((mlbPlayerID, PitcherGameStats(date, player.id, pitchOrder == "100", battingPosition)))
      } else None
    }.toList

    (hitters ++ pitchers).toMap
  }

  var visitingTeamPitcher: PitcherGameStats = visitingTeamPlayers.find {
    case (id, p) => p.isInstanceOf[PitcherGameStats] && p.isStarter
  }.map(_._2.asInstanceOf[PitcherGameStats]).get

  var homeTeamPitcher: PitcherGameStats = homeTeamPlayers.find {
    case (id, p) => p.isInstanceOf[PitcherGameStats] && p.isStarter
  }.map(_._2.asInstanceOf[PitcherGameStats]).get

  val visitingTeamInningsXML = (eventsXML \ "inning" \ "top")
  val homeTeamInningsXML = (eventsXML \ "inning" \ "bottom")

  @tailrec
  private def parseInnings(remainingInningsXML: NodeSeq, players: Map[MLBPlayerID, PlayerGameStats], battingTeam: HomeOrAway): Unit = {
    remainingInningsXML.headOption match {
      case Some(inningXML) =>
        //logDebug("\n\nINNING: " + inningXML.toString)
        parseEvents(inningXML \ "_", players, 0, battingTeam)
        parseInnings(remainingInningsXML.tail, players, battingTeam)
      case None => // done
    }
  }

  @tailrec
  private def parseEvents(remainingEventsXML: NodeSeq, players: Map[MLBPlayerID, PlayerGameStats], previousOuts: Int, battingTeam: HomeOrAway): Unit = {
    remainingEventsXML.headOption match {

      case Some(eventXML) =>
        val event = (eventXML \ "@event").text
        val outs = (eventXML \ "@o").text.toInt
        val description = (eventXML \ "@des").text

        logDebug(s"\nEVENT: $event --- ${eventXML.toString}")

        val pitcher = (eventXML \ "@pitcher").headOption.map(id => players(id.text).asInstanceOf[PitcherGameStats]) match {
          case Some(p) =>
            battingTeam match {
              case VISITING_TEAM =>
                homeTeamPitcher = p
                homeTeamPitcher
              case HOME_TEAM =>
                visitingTeamPitcher = p
                visitingTeamPitcher
            }
          case None =>
            battingTeam match {
              case VISITING_TEAM => homeTeamPitcher
              case HOME_TEAM     => visitingTeamPitcher
            }
        }

        eventXML.toString.substringBefore(" ").tail match {
          case "atbat" => {
            val hitter = players((eventXML \ "@batter").text)

            logDebug(s"hitter: $hitter, pitcher: $pitcher")

            if (outs > previousOuts) {
              hitter.addOutsAgainst(outs - previousOuts, pitcher)
              logDebug(s"${outs - previousOuts} outs")
            }

            if (event != "Runner Out") hitter.addAtBatAgainst(pitcher)

            val runnersWhoScored: List[PlayerGameStats] = if (description.contains("scores")) {
              namesOfPlayersWhoScored(description).map {
                case playerDisplayName =>
                  val mlbPlayerID = mlbPlayerIdByDisplayName((playerDisplayName, battingTeam))
                  val player = battingTeam match {
                    case VISITING_TEAM => visitingTeamPlayers(mlbPlayerID)
                    case HOME_TEAM     => homeTeamPlayers(mlbPlayerID)
                  }
                  //logDebug(s"Runner scored --- name: $playerDisplayName, mlbPlayerID: $mlbPlayerID, player: ${player.player}")
                  player.addRunAgainst(pitcher)
                  player
              }
            } else Nil

            event match {
              case "Batter Interference" =>
                if (description.contains("strikes out") || description.contains("called out on strikes")) hitter.addStrikeoutAgainst(pitcher)
              case "Bunt Groundout" | "Bunt Lineout" | "Bunt Pop Out" | "Fielders Choice" | "Fielders Choice Out"
                | "Flyout" | "Forceout" | "Groundout" | "Lineout" | "Pop Out" | "Sac Bunt" | "Sac Fly" => // out already recorded --- do nothing
              case "Catcher Interference" => //???
              case "Double" => hitter.addDoubleAgainst(pitcher)
              case "Double Play" | "Grounded Into DP" | "Sac Fly DP" | "Triple Play" => // outs already recorded --- do nothing
              case "Fan interference" =>
                if (description.contains(s"${playerDisplayNames(hitter.playerID)} singles")) hitter.addSingleAgainst(pitcher)
                else if (description.contains(s"${playerDisplayNames(hitter.playerID)} doubles")) hitter.addDoubleAgainst(pitcher)
                else if (description.contains(s"${playerDisplayNames(hitter.playerID)} triples")) hitter.addTripleAgainst(pitcher)
                else if (description.contains(s"${playerDisplayNames(hitter.playerID)} homers")) hitter.addHomeRunAgainst(pitcher)
              case "Field Error" => // do nothing
              case "Home Run" =>
                hitter.addHomeRunAgainst(pitcher)
                hitter.addRBIAgainst(pitcher, 1 + runnersWhoScored.length)
                hitter.addRunAgainst(pitcher)
              case "Intent Walk" | "Walk" | "Hit By Pitch" => hitter.addWalkAgainst(pitcher)
              case "Runner Out"                            => // out already recorded --- do nothing
              case "Single"                                => hitter.addSingleAgainst(pitcher)
              case "Strikeout" | "Strikeout - DP"          => hitter.addStrikeoutAgainst(pitcher)
              case "Triple"                                => hitter.addTripleAgainst(pitcher)
              case _                                       => throw new Exception("Unknown event: " + event)
            }
          }

          case "action" => {
            val player = (eventXML \ "@player").headOption.flatMap(id => players.get(id.text))

            logDebug(s"player: $player, pitcher: $pitcher")

            if (outs > previousOuts) {
              player match {
                case Some(p) => p.addOutsAgainst(outs - previousOuts, pitcher)
                case None    => pitcher.pitchingStats.addOuts(outs - previousOuts)
              }
              logDebug(s"${outs - previousOuts} outs")
            }

            event match {
              case "Balk"                              => //???
              case "Caught Stealing 2B"                => //???
              case "Caught Stealing 3B"                => //???
              case "Caught Stealing Home"              => //???
              case "Defensive Indiff"                  => //???
              case "Defensive Sub"                     => //???
              case "Defensive Switch"                  => //???
              case "Ejection"                          => //???
              case "Error"                             => //???
              case "Game Advisory"                     => //???
              case "Manager Review"                    => //???
              case "Offensive Sub"                     => //???
              case "Passed Ball"                       => //???
              case "Picked off stealing 2B"            => //???
              case "Picked off stealing 3B"            => //???
              case "Picked off stealing home"          => //???
              case "Pickoff 1B"                        => //???
              case "Pickoff 2B"                        => //???
              case "Pickoff 3B"                        => //???
              case "Pickoff Error 1B"                  => //???
              case "Pickoff Error 2B"                  => //???
              case "Pitching Substitution"             => //???
              case "Player Injured"                    => //???
              case "Runner Advance"                    => //???
              case "Runner Out"                        => //???
              case "Stolen Base 2B" | "Stolen Base 3B" => //???
              case "Stolen Base Home"                  => //???
              case "Umpire Review"                     => //???
              case "Umpire Substitution"               => //???
              case "Wild Pitch"                        => //???
              case "Batter Turn"                       => //???
              case _                                   => throw new Exception("Unknown event: " + event)
            }
          }
        }

        parseEvents(remainingEventsXML.tail, players, outs, battingTeam)
      case None => // done
    }
  }

  parseInnings(visitingTeamInningsXML, visitingTeamPlayers ++ homeTeamPlayers, VISITING_TEAM)
  parseInnings(homeTeamInningsXML, visitingTeamPlayers ++ homeTeamPlayers, HOME_TEAM)

  def toGame: Game = Game(date, visitingTeam, homeTeam, gameNumber, homePlateUmpireID, winningPitcher, losingPitcher,
    savePitcher, visitingTeamPlayers.values.toList, homeTeamPlayers.values.toList)

  private def namesOfPlayersWhoScored(play: String): List[String] = {
    //logDebug("### namesOfPlayersWhoScored --- \n\tplay: "+play)
    def next(play: String, playerNames: List[String]): List[String] = play.contains("scores") match {
      case true =>
        val scoringPlayerName = play.substringBefore("scores").trim.substringAfterLast("  ")
        //logDebug("\tscoring player: "+scoringPlayerName)
        val remainingPlay = play.substringAfter("scores")
        next(remainingPlay, scoringPlayerName :: playerNames)
      case false => playerNames
    }
    next(play, Nil)
  }

}