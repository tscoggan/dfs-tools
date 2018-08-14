package mlb.model

import CustomTypes._
import utils.StringUtils._
import mlb._

/**
 * FanDuel player data for a given slate
 */
case class Player_FD(
    id: String,
    position: String,
    firstName: String,
    lastName: String,
    nickname: String,
    fptsPerGame: Double,
    gamesPlayed: Int,
    salary: Int,
    game: String,
    team: Team,
    opponent: Team,
    injuryIndicator: Option[String],
    injuryDetails: Option[String],
    probablePitcher: Option[Boolean],
    battingOrder: Option[Int]) {

  val alternateName = nickname.substringBefore(" Jr.").replaceAll("\\.", "").trim
  
  val mlbPlayerID: Option[MLBPlayerID] = {
    Players.playerMappings.find(_.fdPlayerID == this.id) match {
      case Some(mapping) => Some(mapping.mlbPlayerID)
      case None =>
        Players.mlbDotComPlayers.find(mlb => mlb.team == this.team &&
          (this.nickname.toUpperCase == mlb.name.toUpperCase ||
            this.alternateName == mlb.alternateName)).map(_.id)
    }
  }

  override def toString: String = s"FD[$nickname ($position, $team, $salary)]"

  def toStringVerbose: String = s"Player_FD[id=$id, position=$position, firstName=$firstName, lastName=$lastName, nickname=$nickname, " +
    s"FPPG=$fptsPerGame, gamesPlayed=$gamesPlayed, salary=$salary, game=$game, team=$team, opponent=$opponent, " +
    s"injuryIndicator=${injuryIndicator.getOrElse("")}, injuryDetails=${injuryDetails.getOrElse("")}, " +
    s"probablePitcher=${probablePitcher.getOrElse("")}, battingOrder=${battingOrder.getOrElse("")}}]"
}

object Player_FD {

  import scala.io.Source

  def parseFrom(rosterFileName: String): List[Player_FD] = Source.fromFile(rosterFileName).getLines.toList.tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>

        val Array(id,
          position,
          firstName,
          nickname,
          lastName,
          fptsPerGame,
          gamesPlayed,
          salary,
          game,
          team,
          opponent,
          injuryIndicator,
          injuryDetails,
          tier,
          probablePitcher,
          battingOrder) = nextLine.splitCSV()

        Player_FD(
          id.substringAfter("-"),
          position,
          firstName,
          lastName,
          nickname,
          fptsPerGame.toDouble,
          gamesPlayed.toInt,
          salary.toInt,
          game,
          Teams.get(team),
          Teams.get(opponent),
          injuryIndicator.trim.isEmpty match {
            case true  => None
            case false => Some(injuryIndicator.trim)
          },
          injuryDetails.trim.isEmpty match {
            case true  => None
            case false => Some(injuryDetails.trim)
          },
          probablePitcher.toUpperCase match {
            case "YES" => Some(true)
            case "NO"  => Some(false)
            case _     => None
          },
          battingOrder.trim.isEmpty match {
            case true  => None
            case false => Some(battingOrder.trim.toInt)
          })
    }
}