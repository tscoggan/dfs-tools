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

class MLBGameParser(eventsXML: Elem, boxScoreXML: Elem, lineScoreXML: Elem) {

  import MLBGameParser._

  type HomeOrAway = Int
  private val VISITING_TEAM: HomeOrAway = 0
  private val HOME_TEAM: HomeOrAway = 1

  implicit def playerIDToPlayer(playerID: MLBPlayerID): Player = Players.playersByID.get(playerID) match {
    case Some(player) => player
    case None         => throw new Exception("Couldn't find player with MLB.com ID " + playerID)
  }

  var date: Date = (boxScoreXML \ "@date").text.toDate("MMM dd, yyyy")

  var gameNumber: GameNumber = (boxScoreXML \ "@game_id").text.substringAfterLast("-").toInt

  var winningPitcher: Player =
    (lineScoreXML \ "winning_pitcher").find(_.attribute("id").head.text.trim.nonEmpty).map(_.attribute("id").head.text).get

  var losingPitcher: Player =
    (lineScoreXML \ "losing_pitcher").find(_.attribute("id").head.text.trim.nonEmpty).map(_.attribute("id").head.text).get

  var savePitcher: Option[Player] =
    (lineScoreXML \ "save_pitcher").find(_.attribute("id").head.text.trim.nonEmpty).map(_.attribute("id").head.text)

  var visitingTeam: Team = Teams.get { (boxScoreXML \ "@away_team_code").text }

  var homeTeam: Team = Teams.get { (boxScoreXML \ "@home_team_code").text }

  val visitingTeamRuns: Int = (lineScoreXML \ "@away_team_runs").text.toInt
  val homeTeamRuns: Int = (lineScoreXML \ "@home_team_runs").text.toInt

  logDebug(s"### ${date.print()} $visitingTeam $visitingTeamRuns @ $homeTeam $homeTeamRuns ###")

  val playerDisplayNames: mutable.Map[MLBPlayerID, String] = mutable.Map.empty
  val mlbPlayerIdByDisplayName: mutable.Map[(String, HomeOrAway), MLBPlayerID] = mutable.Map.empty

  // all players who played in this game
  val visitingTeamPlayers: Map[MLBPlayerID, PlayerGameStats] = {
    val hittersXML = (boxScoreXML \ "batting").find(_.attribute("team_flag").head.text == "away").get \ "batter"
    val pitchersXML = (boxScoreXML \ "pitching").find(_.attribute("team_flag").head.text == "away").get \ "pitcher"

    val hitters = hittersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val batOrder = (p \ "@bo").text
      if (batOrder.length == 3) { // only include players who played
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((cleanName(displayName), VISITING_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((cleanName(displayName).substringAfterLast(" "), VISITING_TEAM) -> mlbPlayerID) // last name
        mlbPlayerIdByDisplayName += ((cleanName(displayName).head + " " + cleanName(displayName).substringAfterLast(" "), VISITING_TEAM) -> mlbPlayerID) // first letter of first name + last name

        Some((mlbPlayerID, HitterGameStats(date, player.id, batOrder.tail == "00", batOrder.head.asDigit)))
      } else None
    }.toList

    val pitchers = pitchersXML.zipWithIndex.map {
      case (p, pitchOrder) =>
        val mlbPlayerID = (p \ "@id").text
        val player = playerIDToPlayer(mlbPlayerID)
        val displayName = (p \ "@name_display_first_last").text
        val battingPosition = hitters.find { case (name, p) => p.playerID == player.id }.map { case (name, p) => p.battingPosition }.getOrElse(0)
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((cleanName(displayName), VISITING_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((cleanName(displayName).substringAfterLast(" "), VISITING_TEAM) -> mlbPlayerID) // last name
        mlbPlayerIdByDisplayName += ((cleanName(displayName).head + " " + cleanName(displayName).substringAfterLast(" "), VISITING_TEAM) -> mlbPlayerID) // first letter of first name + last name

        val pitcher = PitcherGameStats(date, player.id, pitchOrder == 0, battingPosition)
        if (pitcher.playerID == winningPitcher.id) pitcher.pitchingStats.win = 1
        if (pitcher.playerID == losingPitcher.id) pitcher.pitchingStats.loss = 1
        if (pitcher.playerID == savePitcher.map(_.id).getOrElse("")) pitcher.pitchingStats.save = 1
        pitcher.addEarnedRuns((p \ "@er").text.toInt)
        (mlbPlayerID, pitcher)
    }.toList

    if (pitchers.length == 1) pitchers.head._2.pitchingStats.completeGame = 1

    (hitters ++ pitchers).toMap
  }
  val homeTeamPlayers: Map[MLBPlayerID, PlayerGameStats] = {
    val hittersXML = (boxScoreXML \ "batting").find(_.attribute("team_flag").head.text == "home").get \ "batter"
    val pitchersXML = (boxScoreXML \ "pitching").find(_.attribute("team_flag").head.text == "home").get \ "pitcher"

    val hitters = hittersXML.flatMap { p =>
      val mlbPlayerID = (p \ "@id").text
      val player = playerIDToPlayer(mlbPlayerID)
      val displayName = (p \ "@name_display_first_last").text
      val batOrder = (p \ "@bo").text
      if (batOrder.length == 3) { // only include players who played
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((cleanName(displayName), HOME_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((cleanName(displayName).substringAfterLast(" "), HOME_TEAM) -> mlbPlayerID) // last name
        mlbPlayerIdByDisplayName += ((cleanName(displayName).head + " " + cleanName(displayName).substringAfterLast(" "), HOME_TEAM) -> mlbPlayerID) // first letter of first name + last name

        Some((mlbPlayerID, HitterGameStats(date, player.id, batOrder.tail == "00", batOrder.head.asDigit)))
      } else None
    }.toList

    val pitchers = pitchersXML.zipWithIndex.map {
      case (p, pitchOrder) =>
        val mlbPlayerID = (p \ "@id").text
        val player = playerIDToPlayer(mlbPlayerID)
        val displayName = (p \ "@name_display_first_last").text
        val battingPosition = hitters.find { case (name, p) => p.playerID == player.id }.map { case (name, p) => p.battingPosition }.getOrElse(0)
        playerDisplayNames += (player.id -> displayName)
        mlbPlayerIdByDisplayName += ((cleanName(displayName), HOME_TEAM) -> mlbPlayerID)
        mlbPlayerIdByDisplayName += ((cleanName(displayName).substringAfterLast(" "), HOME_TEAM) -> mlbPlayerID) // last name
        mlbPlayerIdByDisplayName += ((cleanName(displayName).head + " " + cleanName(displayName).substringAfterLast(" "), HOME_TEAM) -> mlbPlayerID) // first letter of first name + last name

        val pitcher = PitcherGameStats(date, player.id, pitchOrder == 0, battingPosition)
        if (pitcher.playerID == winningPitcher.id) pitcher.pitchingStats.win = 1
        if (pitcher.playerID == losingPitcher.id) pitcher.pitchingStats.loss = 1
        if (pitcher.playerID == savePitcher.map(_.id).getOrElse("")) pitcher.pitchingStats.save = 1
        pitcher.addEarnedRuns((p \ "@er").text.toInt)
        (mlbPlayerID, pitcher)
    }.toList

    if (pitchers.length == 1) pitchers.head._2.pitchingStats.completeGame = 1

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
        val rbi = (eventXML \ "@rbi").headOption.map(_.text.toInt).getOrElse(0)
        lazy val homeTeamRuns = (eventXML \ "@home_team_runs").headOption.map(_.text.toInt).get
        lazy val awayTeamRuns = (eventXML \ "@away_team_runs").headOption.map(_.text.toInt).get

        lazy val pitcherTeamLead = battingTeam match {
          case VISITING_TEAM => homeTeamRuns - awayTeamRuns
          case HOME_TEAM     => awayTeamRuns - homeTeamRuns
        }

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

        val runnersWhoScored: List[PlayerGameStats] = if (description.contains("scores")) {
          namesOfPlayersWhoScored(description).map {
            case playerDisplayName =>
              val mlbPlayerID = mlbPlayerIdByDisplayName.getOrElse((cleanName(playerDisplayName), battingTeam),
                mlbPlayerIdByDisplayName((cleanName(playerDisplayName).head + " " + cleanName(playerDisplayName).substringAfterLast(" "), battingTeam)))
              val player = battingTeam match {
                case VISITING_TEAM => visitingTeamPlayers(mlbPlayerID)
                case HOME_TEAM     => homeTeamPlayers(mlbPlayerID)
              }
              //logDebug(s"Runner scored --- name: $playerDisplayName, mlbPlayerID: $mlbPlayerID, player: ${player.player}")
              player.addRunAgainst(pitcher)
              player
          }
        } else Nil

        eventXML.toString.substringBefore(" ").tail match {

          case "atbat" => {
            val hitter = players((eventXML \ "@batter").text)

            logDebug(s"hitter: $hitter, pitcher: $pitcher")

            if (outs > previousOuts) {
              hitter.addOutsAgainst(outs - previousOuts, pitcher)
              logDebug(s"${outs - previousOuts} outs")
            }

            if (event != "Runner Out") hitter.addAtBatAgainst(pitcher)

            event match {
              case "Batter Interference" =>
                if (description.contains("strikes out") || description.contains("called out on strikes")) hitter.addStrikeoutAgainst(pitcher)
              case "Bunt Groundout" | "Bunt Lineout" | "Bunt Pop Out" | "Fielders Choice" | "Fielders Choice Out"
                | "Flyout" | "Forceout" | "Groundout" | "Lineout" | "Pop Out" | "Sac Bunt" | "Sac Fly"
                | "Caught Stealing 2B" | "Caught Stealing 3B" | "Caught Stealing Home" | "Pickoff 1B"
                | "Pickoff 2B" | "Pickoff 3B" | "Pickoff Caught Stealing 2B" | "Pickoff Caught Stealing 3B" | "Pickoff Caught Stealing Home" =>
              case "Catcher Interference" =>
              case "Double" =>
                hitter.addDoubleAgainst(pitcher)
              case "Double Play" | "Grounded Into DP" | "Sac Fly DP" | "Sacrifice Bunt DP" | "Triple Play" | "Strikeout Double Play"
                | "Base Running Double Play" | "Sac Bunt Double Play" | "Sac Fly Double Play" | "Runner Double Play" => // outs already recorded --- do nothing
              case "Fan interference" | "Fan Interference" =>
                if (description.contains(s"${playerDisplayNames(hitter.playerID)} singles")) hitter.addSingleAgainst(pitcher)
                else if (description.contains(s"${playerDisplayNames(hitter.playerID)} doubles")) hitter.addDoubleAgainst(pitcher)
                else if (description.contains(s"${playerDisplayNames(hitter.playerID)} triples")) hitter.addTripleAgainst(pitcher)
                else if (description.contains(s"${playerDisplayNames(hitter.playerID)} homers")) hitter.addHomeRunAgainst(pitcher)
              case "Field Error" | "Pickoff Error 1B" | "Pickoff Error 2B" | "Pickoff Error 3B" => // do nothing
              case "Home Run" =>
                hitter.addHomeRunAgainst(pitcher)
                hitter.addRunAgainst(pitcher)
              case "Intent Walk" | "Walk" | "Hit By Pitch" =>
                hitter.addWalkAgainst(pitcher)
              case "Runner Out" => // out already recorded --- do nothing
              case "Single" =>
                hitter.addSingleAgainst(pitcher)
              case "Strikeout" | "Strikeout - DP" => hitter.addStrikeoutAgainst(pitcher)
              case "Triple" =>
                hitter.addTripleAgainst(pitcher)
              case "Wild Pitch"    => // do nothing
              case "Game Advisory" => // do nothing
              case "Passed Ball"   => // do nothing
              case "Stolen Base 2B" | "Stolen Base 3B" | "Stolen Base Home" =>
                namesOfPlayersWhoStoleBase(description).map {
                  case (playerDisplayName, stoleHome) =>
                    val mlbPlayerID = mlbPlayerIdByDisplayName.getOrElse((cleanName(playerDisplayName), battingTeam),
                      mlbPlayerIdByDisplayName((cleanName(playerDisplayName).head + " " + cleanName(playerDisplayName).substringAfterLast(" "), battingTeam)))
                    val player = battingTeam match {
                      case VISITING_TEAM => visitingTeamPlayers(mlbPlayerID)
                      case HOME_TEAM     => homeTeamPlayers(mlbPlayerID)
                    }
                    //logDebug(s"Runner scored --- name: $playerDisplayName, mlbPlayerID: $mlbPlayerID, player: ${player.player}")
                    player.addStolenBaseAgainst(pitcher)
                    if (stoleHome) player.addRunAgainst(pitcher)
                }
              case "" => // should only see this for a game that ended before completion
              case _  => throw new Exception("Unknown event: " + event)
            }

            hitter.addRBIAgainst(pitcher, rbi)
          }

          case "action" => {
            val player = (eventXML \ "@player").headOption.flatMap(id => players.get(id.text))

            logDebug(s"player: $player, pitcher: $pitcher")

            if (rbi > 0) throw new Exception(s"$rbi RBI's on 'action' event --- not recorded")

            if (outs > previousOuts) {
              player match {
                case Some(p) => p.addOutsAgainst(outs - previousOuts, pitcher)
                case None    => pitcher.pitchingStats.addOuts(outs - previousOuts)
              }
              logDebug(s"${outs - previousOuts} outs")
            }

            event match {
              case "Balk" => //???
              case "Caught Stealing 2B" => //???
              case "Caught Stealing 3B" => //???
              case "Caught Stealing Home" => //???
              case "Defensive Indiff" => //???
              case "Defensive Sub" => //???
              case "Defensive Switch" => //???
              case "Ejection" => //???
              case "Error" => //???
              case "Game Advisory" => //???
              case "Manager Review" => //???
              case "Offensive Sub" => //???
              case "Passed Ball" => //???
              case "Picked off stealing 2B" => //???
              case "Picked off stealing 3B" => //???
              case "Picked off stealing home" => //???
              case "Pickoff Caught Stealing 2B" => //???
              case "Pickoff Caught Stealing 3B" => //???
              case "Pickoff Caught Stealing home" | "Pickoff Caught Stealing Home" => //???
              case "Pickoff 1B" => //???
              case "Pickoff 2B" => //???
              case "Pickoff 3B" => //???
              case "Pickoff Attempt 1B" => //???
              case "Pickoff Attempt 2B" => //???
              case "Pickoff Attempt 3B" => //???
              case "Pickoff Error 1B" => //???
              case "Pickoff Error 2B" => //???
              case "Pickoff Error 3B" => //???
              case "Pitching Substitution" => //???
              case "Player Injured" => //???
              case "Runner Advance" => //???
              case "Other Advance" => //???
              case "Runner Out" => //???
              case "Stolen Base 2B" | "Stolen Base 3B" | "Stolen Base Home" =>
                namesOfPlayersWhoStoleBase(description).map {
                  case (playerDisplayName, stoleHome) =>
                    val mlbPlayerID = mlbPlayerIdByDisplayName.getOrElse((cleanName(playerDisplayName), battingTeam),
                      mlbPlayerIdByDisplayName((cleanName(playerDisplayName).head + " " + cleanName(playerDisplayName).substringAfterLast(" "), battingTeam)))
                    val player = battingTeam match {
                      case VISITING_TEAM => visitingTeamPlayers(mlbPlayerID)
                      case HOME_TEAM     => homeTeamPlayers(mlbPlayerID)
                    }
                    //logDebug(s"Runner scored --- name: $playerDisplayName, mlbPlayerID: $mlbPlayerID, player: ${player.player}")
                    player.addStolenBaseAgainst(pitcher)
                    if (stoleHome) player.addRunAgainst(pitcher)
                }
              case "Umpire Review"            => //???
              case "Umpire Substitution"      => //???
              case "Wild Pitch"               => //???
              case "Batter Turn"              => //???
              case "Pitch Challenge"          => //???
              case "Pitcher Switch"           => //???
              case "Base Running Double Play" => //???
              case "Offensive Substitution"   => //???
              case "Injury"                   => //???
              case _                          => throw new Exception("Unknown event: " + event)
            }
          }
        }

        parseEvents(remainingEventsXML.tail, players, outs, battingTeam)
      case None => // done
    }
  }

  parseInnings(visitingTeamInningsXML, visitingTeamPlayers ++ homeTeamPlayers, VISITING_TEAM)
  parseInnings(homeTeamInningsXML, visitingTeamPlayers ++ homeTeamPlayers, HOME_TEAM)

  def toGame: Game = Game(date, visitingTeam, homeTeam, gameNumber, winningPitcher, losingPitcher,
    savePitcher, visitingTeamPlayers.values.toList, homeTeamPlayers.values.toList, visitingTeamRuns, homeTeamRuns)

  private def namesOfPlayersWhoScored(play: String): List[String] = {
    //logDebug("### namesOfPlayersWhoScored --- \n\tplay: "+play)
    def next(play: String, playerNames: List[String]): List[String] = play.contains("scores") match {
      case true =>
        val scoringPlayerName = play.substringBefore("scores").substringAfterLast(",").trim.substringAfterLast("  ")
        //logDebug("\tscoring player: "+scoringPlayerName)
        val remainingPlay = play.substringAfter("scores")
        next(remainingPlay, scoringPlayerName :: playerNames)
      case false => playerNames
    }
    next(play, Nil)
  }

  // Boolean value returned is true if player stole home base
  private def namesOfPlayersWhoStoleBase(play: String): List[(String, Boolean)] = {
    //logDebug("### namesOfPlayersWhoStoleBase --- \n\tplay: "+play)
    def next(play: String, playerNames: List[(String, Boolean)]): List[(String, Boolean)] = play.contains("steals") match {
      case true =>
        val playerName = play.substringBefore("steals").substringAfterLast(",").substringAfterLast(":").trim.substringAfterLast("  ")
        //logDebug("\tplayer: "+playerName)
        val remainingPlay = play.substringAfter("steals")
        val stoleHome: Boolean = play.substringAfter("steals").substringBefore("  ").contains("home")
        next(remainingPlay, (playerName, stoleHome) :: playerNames)
      case false => playerNames
    }
    next(play, Nil)
  }

}

object MLBGameParser {

  def cleanName(name: String): String = {
    if (name.endsWith(" Jr.") || name.endsWith(" Jr")) name.substringBefore(" Jr")
    else name
  }

}