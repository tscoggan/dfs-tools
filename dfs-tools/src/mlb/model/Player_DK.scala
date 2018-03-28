package mlb.model

import CustomTypes._
import utils.StringUtils._
import mlb._
import utils.Logger._

/**
 * DraftKings player data for a given slate
 */
case class Player_DK(
    position: String,
    name: String,
    salary: Int,
    game: String,
    fptsPerGame: Double,
    team: Team) {

  val id: String = s"$name-$team" // is this unique?
  
  val opponent: Team = {
    val atIndex = game.indexOf('@')
    if (atIndex <= 0) log(s"WARNING: Unexpected format for DK game info: $game")
    val awayTeam = Teams.get(game.substring(0,atIndex))
    val homeTeam = Teams.get(game.substringsBetween("@", " ").head)
    if (awayTeam == team) homeTeam else awayTeam
  }

  val player: Option[Player] = {
    Players.playerMappings.find(_.dkNameAndTeam == id) match {
      case Some(mapping) =>
        Players.retrosheetPlayers.find(_.id == mapping.retrosheetID).orElse {
          Players.newPlayers.find(_.id == mapping.retrosheetID)
        }
      case None =>
        Players.retrosheetPlayers.find(p => p.name.toUpperCase == name.toUpperCase).orElse {// && p.team == team).orElse {
          Players.newPlayers.find(p => p.name.toUpperCase == name.toUpperCase && p.team == team)
        }
    }
  }

  override def toString: String = s"DK[$name ($position, $team, $salary)]"

  def toStringVerbose: String = s"Player_DK[position=$position, name=$name, FPPG=$fptsPerGame, salary=$salary, game=$game, team=$team]"
}

object Player_DK {

  import scala.io.Source

  def parseFrom(rosterFileName: String): List[Player_DK] = Source.fromFile(rosterFileName).getLines.toList.tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>

        val Array(position,
          name,
          salary,
          game,
          fptsPerGame,
          team) = nextLine.splitCSV()

        Player_DK(
          position,
          name,
          salary.toInt,
          game,
          fptsPerGame.toDouble,
          Teams.get(team))
    }
}