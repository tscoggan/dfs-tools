package mlb

import retrosheet._
import model._
import model.CustomTypes._
import utils.FileUtils._
import utils.Logger._
import utils.StringUtils._
import mlb.model._
import scala.io.Source

object Players {

  case class PlayerMapping(retrosheetID: PlayerID, fdPlayerID: String, dkPlayerID: String)

  val playerMappings: List[PlayerMapping] = Source.fromFile(Configs.playerMappingsFile).getLines.toList.tail
    .map(_.substringBefore("//").trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>
        val Array(retrosheetID, fdPlayerID, dkPlayerID) = nextLine.splitCSV()
        PlayerMapping(retrosheetID, fdPlayerID.substringAfter("-"), dkPlayerID)
    }
  log(s"Found ${playerMappings.length} player mappings")

  val retrosheetPlayers: List[Player] = {
    getListOfFiles(Configs.Retrosheet.dataFileDir_2017, ".ROS").flatMap { file => PlayerData.parseFrom(file.getPath) }
      .groupBy(_.id).map { case (k, v) => v.head }.toList // if duplicates because same player played on multiple teams, pick one
  }
  log(s"Found ${retrosheetPlayers.length} Retrosheet players")

  val newPlayers: List[Player] = PlayerData.parseFrom(Configs.newPlayersFile)
  log(s"Found ${newPlayers.length} new players")

  (retrosheetPlayers ++ newPlayers).groupBy(_.id).foreach {
    case (id, players) => if (players.length > 1) log(s"WARNING: ${players.length} Retrosheet players with same id: $id")
  }
  (retrosheetPlayers ++ newPlayers).groupBy(_.name).foreach {
    case (name, players) => if (players.length > 1) log(s"WARNING: ${players.length} Retrosheet players with same name: ${players.mkString(", ")}")
  }

  val fanduelPlayers: List[Player_FD] = {
    val file = getListOfFiles(Configs.dfsSalaryFileDir, ".csv")
      .filter(_.getName.startsWith("FanDuel-MLB-"))
      .sortBy(_.getName.trimPrefix("FanDuel-MLB-").take(10).toDate())
      .last
    log("Loading FD players from file " + file)
    Player_FD.parseFrom(file.getPath)
  } //.filterNot { p => p.position == "P" && !p.probablePitcher.exists(_ == true) } // ignore non-starting pitchers
  log(s"Found ${fanduelPlayers.length} FD players")

  fanduelPlayers.groupBy(_.nickname).foreach {
    case (name, players) => if (players.length > 1) log(s"WARNING: ${players.length} FD players with same name: ${players.mkString(", ")}")
  }

  val draftkingsPlayers: List[Player_DK] = {
    val file = getListOfFiles(Configs.dfsSalaryFileDir, ".csv")
      .filter(_.getName.startsWith("DraftKings-MLB-"))
      .sortBy(_.getName.trimPrefix("DraftKings-MLB-").take(10).toDate())
      .last
    log("Loading DK players from file " + file)
    Player_DK.parseFrom(file.getPath)
  } //.filterNot { p => p.position == "P" && !p.probablePitcher.exists(_ == true) } // ignore non-starting pitchers
  log(s"Found ${draftkingsPlayers.length} DK players")

  draftkingsPlayers.groupBy(_.name).foreach {
    case (name, players) => if (players.length > 1) log(s"WARNING: ${players.length} DK players with same name: ${players.mkString(", ")}")
  }

  val allPlayers: List[Player] = (retrosheetPlayers ++ newPlayers) map { player =>
    val fanduel = fanduelPlayers.find(_.player.map(_.id).getOrElse("") == player.id)
    val draftkings = draftkingsPlayers.find(_.player.map(_.id).getOrElse("") == player.id)
    val (newName, newPosition) = fanduel match {
      case Some(fd) => (fd.nickname, fd.position: Position)
      case None => draftkings match {
        case Some(dk) => (dk.name, dk.position: Position)
        case None     => (player.name, player.position)
      }
    }
    val newTeam = draftkings.map(_.team).orElse(fanduel.map(_.team)).getOrElse(player.team)
    val newOpponent = draftkings.map(_.opponent).orElse(fanduel.map(_.opponent))
    val newBattingPosition = fanduel.flatMap(_.battingOrder) match {
      case None if (fanduel.flatMap(_.probablePitcher).getOrElse(false) == true) =>
        val filledSpots = fanduelPlayers.filter(_.team == player.team).flatMap(_.battingOrder).filterNot(_ == 0)
        val unfilledSpots = (1 to 9).toList.diff(filledSpots)
        //println(s"$player --> ${filledSpots.length} filled lineup spots: ${filledSpots.sorted.mkString(",")}\n\tunfilled: ${unfilledSpots.sorted.mkString(",")}")
        if (unfilledSpots.length == 1) Some(unfilledSpots.head)
        else None
      case bp => bp
    }
    player.copy(name = newName, position = newPosition, team = newTeam, opponent = newOpponent,
      fanduel = fanduel.map(p => PlayerSiteInfo(p.nickname, p.team, p.position, p.salary,
        p.probablePitcher.orElse(p.battingOrder.map(_ > 0)), newBattingPosition)),
      draftkings = draftkings.map(p => PlayerSiteInfo(p.name, p.team, p.position, p.salary, None, None)))
  }

  val playersByID: Map[PlayerID, Player] = allPlayers.map { p => (p.id, p) }.toMap

  val playersByTeam: Map[Team, List[Player]] = allPlayers.groupBy(_.team)

  val startingPlayers: List[Player] = allPlayers.filter(_.isStarting)

  val startingPlayersByTeam: Map[Team, List[Player]] = startingPlayers.groupBy(_.team)

  println("\nStarters: \n" + startingPlayersByTeam.map {
    case (team, players) =>
      s"$team:\n\t" + players.sortBy(_.battingPosition.getOrElse(0))
        .map(p => p.battingPosition.getOrElse(0) + ") " + p).mkString("\n\t")
  }.mkString("\n"))

  def get(playerID: String): Player = playersByID.get(playerID).get // throws exception if playerID is invalid
}