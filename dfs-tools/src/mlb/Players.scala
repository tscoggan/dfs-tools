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

  case class PlayerMapping(retrosheetID: PlayerID, fdPlayerID: String, dkNameAndTeam: String)

  val playerMappings: List[PlayerMapping] = Source.fromFile(Configs.playerMappingsFile).getLines.toList.tail
    .map(_.trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>
        val Array(retrosheetID, fdPlayerID, dkNameAndTeam) = nextLine.splitCSV()
        PlayerMapping(retrosheetID, fdPlayerID, dkNameAndTeam)
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
    val fd = fanduelPlayers.find(_.player.map(_.id).getOrElse("") == player.id)
    val dk = draftkingsPlayers.find(_.player.map(_.id).getOrElse("") == player.id)
    val newTeam = dk.map(_.team).orElse(fd.map(_.team)).getOrElse(player.team)
    val newOpponent = dk.map(_.opponent).orElse(fd.map(_.opponent))
    //val newPosition = dk.map(_.position).orElse(fd.map(_.)).map(Teams.get(_)).getOrElse(player.team)
    player.copy(team = newTeam, opponent = newOpponent,
      fanduel = fd.map(p => PlayerSiteInfo(p.nickname, p.team, p.position, p.salary, p.battingOrder.map(_ > 0).orElse(p.probablePitcher), p.battingOrder)),
      draftkings = dk.map(p => PlayerSiteInfo(p.name, p.team, p.position, p.salary, None, None)))
  }

  val startingPlayers = allPlayers.filter(_.fanduel.flatMap(_.starter).getOrElse(false))

  println("Starters: \n" + startingPlayers.sortBy(p => p.team.id + p.fanduel.flatMap(_.battingPosition).getOrElse(0)).mkString("\n"))

  val playersByID: Map[PlayerID, Player] = allPlayers.map { p => (p.id, p) }.toMap

  val playersByName: Map[String, Player] = allPlayers.map { p => (p.name, p) }.toMap

  def get(playerID: String): Player = playersByID.get(playerID).get // throws exception if playerID is invalid
}