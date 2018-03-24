package mlb.retrosheet

import mlb.model._
import mlb.model.CustomTypes._
import mlb._
import utils.Logger._
import utils.StringUtils._
import utils.DateTimeUtils._
import java.util.Date
import scala.io.Source
import scala.collection.mutable
import mlb.retrosheet.Bases._

object EventFileConstants {
  val VISITING_TEAM = 0
  val HOME_TEAM = 1

  val PITCHER = 1
}

class EventFileParser(filename: String) {
  import EventFileConstants._

  lazy val games: List[Game] = {

    val remainingLines: Iterator[String] = Source.fromFile(filename).getLines.toIterator

    def parseNext(completedGames: List[Game], currentGame: Option[GameData]): List[Game] = {
      if (remainingLines.isEmpty) currentGame match {
        // end of file
        case Some(current) => (current.toGame :: completedGames).reverse
        case None          => completedGames.reverse
      }
      else {
        val nextLine = remainingLines.next()

        if (nextLine.startsWith("id,")) {
          val Array(_, gameID) = nextLine.splitCSV()
          logDebug(s"### Found new game ($gameID) ###")
          currentGame match {
            case Some(current) => parseNext(current.toGame :: completedGames, Some(new GameData(gameID))) // add current game and start a new one
            case None          => parseNext(completedGames, Some(new GameData(gameID))) // first game in file
          }
        } else {
          currentGame match {
            case Some(current) => parseNext(completedGames, Some(current.update(nextLine))) // update the current game
            case None          => throw new Exception("Encountered unexpected event file line: " + nextLine)
          }
        }
      }
    }

    parseNext(Nil, None)
  }

}

class GameData(id: String) {
  import EventFileConstants._

  var bases: Bases = new Bases(this)

  var date: Option[Date] = None
  var gameNumber: GameNumber = 0
  var dayGame: Option[Boolean] = None
  var usedDesignatedHitter: Option[Boolean] = None
  var homePlateUmpireID: Option[String] = None
  var temperature: Option[Int] = None
  var windDirection: String = ""
  var windSpeed: Option[Int] = None
  var precipitation: String = ""
  var winningPitcher: Option[Player] = None
  var losingPitcher: Option[Player] = None
  var savePitcher: Option[Player] = None

  var visitingTeam: Option[Team] = None
  var homeTeam: Option[Team] = None

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

  def recordOuts(numberOfOuts: Int) = {
    val pitcher = if (visitingOrHome.toInt == VISITING_TEAM) homeTeamPitcher.get else visitingTeamPitcher.get
    pitcher.addOuts(numberOfOuts)
    outsThisInning += numberOfOuts
    if (outsThisInning > 3) logDebug(s"WARNING: outsThisInning = $outsThisInning")
  }

  def update(nextLine: String): GameData = {
    nextLine.splitCSV() match {
      case Array("info", "visteam", teamID)          => visitingTeam = Some(Teams.get(teamID))
      case Array("info", "hometeam", teamID)         => homeTeam = Some(Teams.get(teamID))
      case Array("info", "date", dateStr)            => date = Some(getDateFormat("yyyy/MM/dd").parse(dateStr))
      case Array("info", "number", gameNum)          => gameNumber = gameNum.toInt
      case Array("info", "daynight", dayNight)       => dayGame = Some(dayNight.toLowerCase == "day")
      case Array("info", "usedh", usedDH)            => usedDesignatedHitter = Some(usedDH.toLowerCase == "true")
      case Array("info", "umphome", homePlateUmpire) => homePlateUmpireID = Some(homePlateUmpire)
      case Array("info", "temp", temp)               => if (temp != 0) temperature = Some(temp.toInt)
      case Array("info", "winddir", windDir)         => windDirection = windDir
      case Array("info", "windspeed", windSpd)       => if (windSpd != -1) windSpeed = Some(windSpd.toInt)
      case Array("info", "precip", precip)           => precipitation = precip
      case Array("info", "wp", wpPlayerID)           => if (wpPlayerID.trim.nonEmpty) winningPitcher = Some(Players.get(wpPlayerID))
      case Array("info", "lp", lpPlayerID)           => if (lpPlayerID.trim.nonEmpty) losingPitcher = Some(Players.get(lpPlayerID))
      case Array("info", "save", savePlayerID)       => if (savePlayerID.trim.nonEmpty) savePitcher = Some(Players.get(savePlayerID))

      case Array("start", playerID, playerName, visitingOrHome, battingPosition, fieldingPosition) =>
        logDebug(nextLine)
        val starter = fieldingPosition.toInt match {
          case PITCHER => PitcherGameStats(date.get, playerID, true, battingPosition.toInt)
          case _       => HitterGameStats(date.get, playerID, true, battingPosition.toInt)
        }
        winningPitcher.foreach { p => if (p.id == playerID) starter.asInstanceOf[PitcherGameStats].win = 1 }
        losingPitcher.foreach { p => if (p.id == playerID) starter.asInstanceOf[PitcherGameStats].loss = 1 }
        savePitcher.foreach { p => if (p.id == playerID) starter.asInstanceOf[PitcherGameStats].save = 1 }

        visitingOrHome.toInt match {
          case VISITING_TEAM =>
            visitingTeamActivePlayers(playerID) = starter
            visitingTeamPlayers += starter
            if (fieldingPosition.toInt == PITCHER) visitingTeamPitcher = Some(starter.asInstanceOf[PitcherGameStats])
          case HOME_TEAM =>
            homeTeamActivePlayers(playerID) = starter
            homeTeamPlayers += starter
            if (fieldingPosition.toInt == PITCHER) homeTeamPitcher = Some(starter.asInstanceOf[PitcherGameStats])
        }

      case Array("sub", playerID, playerName, visitingOrHome, battingPosition, fieldingPosition) =>
        logDebug(nextLine)
        val sub = (visitingTeamPlayers ++ homeTeamPlayers).find(_.playerID == playerID) // first look to see if player is already in the game (e.g. switching field positions)
          .getOrElse {
            fieldingPosition.toInt match {
              case PITCHER => PitcherGameStats(date.get, playerID, false, battingPosition.toInt)
              case _       => HitterGameStats(date.get, playerID, false, battingPosition.toInt)
            }
          }
        if (sub.battingPosition != battingPosition.toInt) {
          logDebug(s"!!! Changing $sub batting position from ${sub.battingPosition} to ${battingPosition.toInt}")
          sub.battingPosition = battingPosition.toInt
        }
        winningPitcher.foreach { p => if (p.id == playerID) sub.asInstanceOf[PitcherGameStats].win = 1 }
        losingPitcher.foreach { p => if (p.id == playerID) sub.asInstanceOf[PitcherGameStats].loss = 1 }
        savePitcher.foreach { p => if (p.id == playerID) sub.asInstanceOf[PitcherGameStats].save = 1 }

        visitingOrHome.toInt match {
          case VISITING_TEAM =>
            val subFor = visitingTeamActivePlayers.find { case (playerID, p) => p.battingPosition == battingPosition.toInt }.get._2
            if (sub.playerID != subFor.playerID) {
              visitingTeamActivePlayers -= subFor.playerID
              visitingTeamActivePlayers(playerID) = sub
              visitingTeamPlayers += sub
              if (fieldingPosition.toInt == PITCHER) visitingTeamPitcher = Some(sub.asInstanceOf[PitcherGameStats])
              logDebug(s"$sub replaced $subFor for ${visitingTeam.get} in inning $inning (batting position: $battingPosition)")
              bases.replace(subFor, sub)
            }
          case HOME_TEAM =>
            val subFor = homeTeamActivePlayers.find { case (playerID, p) => p.battingPosition == battingPosition.toInt }.get._2
            if (sub.playerID != subFor.playerID) {
              homeTeamActivePlayers -= subFor.playerID
              homeTeamActivePlayers(playerID) = sub
              homeTeamPlayers += sub
              if (fieldingPosition.toInt == PITCHER) homeTeamPitcher = Some(sub.asInstanceOf[PitcherGameStats])
              logDebug(s"$sub replaced $subFor for ${homeTeam.get} in inning $inning (batting position: $battingPosition)")
              bases.replace(subFor, sub)
            }
        }

      case Array("play", inning, visitingOrHome, batterPlayerID, pitchCount, pitches, event) =>
        logDebug(nextLine)
        if (this.inning != inning.toInt || this.visitingOrHome != visitingOrHome.toInt) {
          bases.clear
          outsThisInning = 0
        }
        this.inning = inning.toInt
        this.visitingOrHome = visitingOrHome.toInt

        if (event != "NP") {
          val hitter = if (visitingOrHome.toInt == VISITING_TEAM) visitingTeamActivePlayers(batterPlayerID) else homeTeamActivePlayers(batterPlayerID)
          val pitcher = if (visitingOrHome.toInt == VISITING_TEAM) homeTeamPitcher.get else visitingTeamPitcher.get

          val (play, modifiers, advances) = deconstruct(event)

          logDebug(s"\thitter: $hitter\n\tpitcher: $pitcher\n\tevent: $event\n\tplay: $play\n\tmodifiers: $modifiers\n\tadvances: $advances")

          if (play.head.isDigit && modifiers.exists(_.contains("DP"))) { // ### WHAT ABOUT OTHER TYPES OF DOUBLE PLAYS? EXAMPLE: K/DP ###
            logDebug("\t### Double play")
            hitter.addAtBat
            val runnerOutAdvances = play.substringsBetween("(", ")").map { base => base + "X" + base } // runners called out
            val batterAdvance = if ((runnerOutAdvances ++ advances).count(adv => adv.contains("X") && !adv.contains("E")) == 2) List("B-1") else Nil // implied that batter reaches 1st base
            val allAdvances = Bases.merge(batterAdvance, runnerOutAdvances, advances)
            recordOuts(2 - allAdvances.count(adv => adv.contains("X") && !adv.contains("E"))) // required because not all outs are explicitly specified
            bases.update(hitter, pitcher, allAdvances: _*) // no RBI's credited on double plays

          } else if (play.head.isDigit && modifiers.exists(_.contains("TP"))) {
            logDebug("\t### Triple play")
            hitter.addAtBat
            val runnerOutAdvances = play.substringsBetween("(", ")").map { base => base + "X" + base } // runners called out
            val allAdvances = Bases.merge(runnerOutAdvances, advances)
            recordOuts(3 - allAdvances.count(adv => adv.contains("X") && !adv.contains("E"))) // required because not all outs are explicitly specified
            bases.update(hitter, pitcher, allAdvances: _*) // no RBI's credited on triple plays

          } else if (play.startsWith("FC")) {
            logDebug("\t### Fielder's choice")
            hitter.addAtBat
            val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-1") else Nil // implied that batter reaches 1st base
            val allAdvances = Bases.merge(batterAdvance, advances)
            hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

          } else if (play.startsWith("FLE")) {
            logDebug("\t### Foul ball error")

          } else if (play.takeWhile(_ != '(').contains("E")) {
            logDebug("\t### Error")
            hitter.addAtBat
            val runnerOutAdvances = play.substringsBetween("(", ")").map { base => base + "X" + base } // runners called out
            val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-1") else Nil // implied that batter reaches 1st base
            val allAdvances = Bases.merge(batterAdvance, runnerOutAdvances, advances)
            hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

          } else if (play.head.isDigit) {
            if (modifiers.exists(_.startsWith("FO"))) {
              logDebug("\t### Force out")
              hitter.addAtBat
              val runnerOutAdvances = play.substringsBetween("(", ")").map { base => base + "X" + nextBase(base.head).get } // runners called out
              val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-1") else Nil // implied that batter reaches 1st base
              val allAdvances = Bases.merge(batterAdvance, runnerOutAdvances, advances)
              hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

            } else {
              logDebug("\t### Out")
              hitter.addAtBat
              recordOuts(1)
              hitter.addRBI(bases.update(hitter, pitcher, advances: _*))
            }

          } else if (play.startsWith("SB")) {
            logDebug("\t### Stolen base")
            val sbAdvances: List[Advance] = play.split(";").map(_.charAt(2)).map { stolenBase =>
              stolenBase match {
                case HOME_BASE =>
                  bases.runnerOn(3) match {
                    case Some(runner) => runner.addStolenBase
                    case None         => logDebug(s"WARNING: No runner on 3rd base...cannot credit stolen base")
                  }
                  "3-H"
                case base =>
                  bases.runnerOn(base.asDigit - 1) match {
                    case Some(runner) => runner.addStolenBase
                    case None         => logDebug(s"WARNING: No runner on base ${base.asDigit - 1}...cannot credit stolen base")
                  }
                  (base.asDigit - 1) + "-" + base
              }
            }.toList
            bases.update(hitter, pitcher, Bases.merge(sbAdvances, advances): _*)

          } else if (play.startsWith("CS")) {
            logDebug("\t### Caught stealing")
            val throwoutAdvance = if (!play.contains("E")) List(previousBase(play.charAt(2)).get + "X" + play.charAt(2)) else Nil
            val allAdvances = Bases.merge(throwoutAdvance, advances)
            bases.update(hitter, pitcher, allAdvances: _*)

          } else if (play.startsWith("POCS")) {
            logDebug("\t### Pickoff caught stealing")
            val throwoutAdvance = if (!play.contains("E")) List(previousBase(play.charAt(4)).get + "X" + play.charAt(4)) else Nil
            val allAdvances = Bases.merge(throwoutAdvance, advances)
            bases.update(hitter, pitcher, allAdvances: _*)

          } else if (play.startsWith("PO")) {
            if (play.contains("E")) {
              logDebug("\t### Pickoff error")
              bases.update(hitter, pitcher, advances: _*) // error on pickoff attempt, no out
            } else {
              logDebug("\t### Pickoff")
              bases.update(hitter, pitcher, Bases.merge(List(play.charAt(2) + "X" + play.charAt(2)), advances): _*)
            }

          } else if (play.startsWith("DI")) {
            logDebug("\t### Defensive indifference")
            bases.update(hitter, pitcher, advances: _*)

          } else if (play.startsWith("BK")) {
            logDebug("\t### Balk")
            bases.update(hitter, pitcher, advances: _*)

          } else if (play.startsWith("PB") || play.startsWith("WP")) {
            logDebug("\t### Passed ball / wild pitch ")
            bases.update(hitter, pitcher, advances: _*)

          } else if (play.startsWith("W") || play.startsWith("HP") || play.startsWith("I")) {
            logDebug("\t### Walk")
            hitter.addAtBat
            hitter.addWalk
            pitcher.addWalkAgainst

            if (play.startsWith("W+")) update(nextLine.replaceAllLiterally("W+", ""))
            else if (play.startsWith("I+")) update(nextLine.replaceAllLiterally("I+", ""))
            else if (play.startsWith("IW+")) update(nextLine.replaceAllLiterally("IW+", ""))
            else hitter.addRBI(bases.update(hitter, pitcher, advances: _*))

            if (!advances.exists(_.startsWith("B-"))) bases.update(hitter, pitcher, "B-1") // implied that batter reaches 1st base

          } else if (play.startsWith("S")) {
            logDebug("\t### Single")
            hitter.addAtBat
            hitter.addSingle
            pitcher.addHitAgainst
            val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-1") else Nil // implied that batter reaches 1st base
            val allAdvances = Bases.merge(batterAdvance, advances)
            hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

          } else if (play.startsWith("D")) {
            logDebug("\t### Double")
            hitter.addAtBat
            hitter.addDouble
            pitcher.addHitAgainst
            val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-2") else Nil // implied that batter reaches 2nd base
            val allAdvances = Bases.merge(batterAdvance, advances)
            hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

          } else if (play.startsWith("T")) {
            logDebug("\t### Triple")
            hitter.addAtBat
            hitter.addTriple
            pitcher.addHitAgainst
            val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-3") else Nil // implied that batter reaches 3rd base
            val allAdvances = Bases.merge(batterAdvance, advances)
            hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

          } else if (play.startsWith("H")) {
            logDebug("\t### Home run")
            hitter.addAtBat
            hitter.addHR
            pitcher.addHitAgainst
            hitter.addRBI(bases.update(hitter, pitcher, advances: _*))
            if (!advances.exists(_.startsWith("B-"))) {
              hitter.addRBI(1)
              hitter.addRun
            }

          } else if (play.startsWith("K")) {
            logDebug("\t### Strikeout")
            hitter.addAtBat
            pitcher.addStrikeout

            val cleanedAdvances = advances.filterNot(_.startsWith("BX"))

            if (play.startsWith("K+")) {
              if (!advances.exists(_.startsWith("B-"))) recordOuts(1)
              update(nextLine.replaceAllLiterally("K+", ""))
            } else if (play.startsWith("K23+")) { // dropped 3rd strike, throwout at 1st base
              recordOuts(1)
              update(nextLine.replaceAllLiterally("K23+", ""))
            } else if (cleanedAdvances.exists(_.contains("X")) && outsThisInning == 2) { // runner out on (dropped?) 3rd strike --- ignore runner out to avoid double-counting 3rd out
              recordOuts(1)
            } else {
              recordOuts(1)
              bases.update(hitter, pitcher, cleanedAdvances: _*)
            }

          } else if (play == "C") {
            logDebug("\t### Catcher interference")
            hitter.addAtBat
            val batterAdvance = if (!advances.exists(_.startsWith("B-"))) List("B-1") else Nil // implied that batter reaches 1st base
            val runnerOutAdvances = play.substringsBetween("(", ")").map { base => base + "X" + base } // runners called out
            val allAdvances = Bases.merge(batterAdvance, runnerOutAdvances, advances)
            hitter.addRBI(bases.update(hitter, pitcher, allAdvances: _*))

          } else if (play.startsWith("OA")) {
            logDebug("\t### Other baserunner advance")
            bases.update(hitter, pitcher, advances: _*)

          } else if (play.startsWith("NP")) {
            logDebug("\t### No play")
          }
        }

      case Array("data", "er", playerID, earnedRuns) =>
        logDebug(nextLine)
        visitingTeamPlayers.find(_.playerID == playerID).orElse(homeTeamPlayers.find(_.playerID == playerID)) match {
          case Some(player) => player match {
            case pitcher: PitcherGameStats => pitcher.earnedRuns = earnedRuns.toInt
            case hitter: HitterGameStats   => logDebug(s"Hitter ($hitter) was a pitcher --- ignoring pitching stats: $nextLine")
            case _                         => logDebug(s"WARNING: Player matching this event has unexpected type: $nextLine")
          }
          case None => logDebug(s"WARNING: Couldn't find player matching this event: $nextLine")
        }

      case _ => // ignore
    }
    this
  }

  private def deconstruct(event: String): (Play, List[Modifier], List[Advance]) = (event.takeWhile(_ != '.').indexOf('/'), event.indexOf('.')) match {
    case (-1, -1)             => (event, Nil, Nil)
    case (modStart, -1)       => (event.substring(0, modStart), event.substring(modStart + 1).split("/").toList, Nil)
    case (-1, advStart)       => (event.substring(0, advStart), Nil, event.substring(advStart + 1).split(";").toList)
    case (modStart, advStart) => (event.substring(0, modStart), event.substring(modStart + 1, advStart).split("/").toList, event.substring(advStart + 1).split(";").toList)
  }

  def toGame: Game = Game(date.get, visitingTeam.get, homeTeam.get, gameNumber, dayGame.get, usedDesignatedHitter.get,
    homePlateUmpireID.get, temperature, windDirection, windSpeed, precipitation, winningPitcher.get, losingPitcher.get,
    savePitcher, visitingTeamPlayers.toList, homeTeamPlayers.toList)
}