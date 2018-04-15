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

  val teamsNotPlaying: List[Team] = Source.fromFile(Configs.teamsNotPlayingFile).getLines.toList
    .map(_.substringBefore("//").trim)
    .filter(_.nonEmpty)
    .map { case teamID => Teams.get(teamID) }
  log(s"Teams not playing: ${teamsNotPlaying.mkString(", ")}")

  val playerMappings: List[PlayerMapping] = Source.fromFile(Configs.playerMappingsFile).getLines.toList.tail
    .map(_.substringBefore("//").trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>
        val Array(retrosheetID, fdPlayerID, dkNameAndTeam) = nextLine.splitCSV()
        PlayerMapping(retrosheetID, fdPlayerID.substringAfter("-"), dkNameAndTeam)
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
    val retrosheet = PlayerSiteInfo(player.name, player.team, player.position, 0, None, None)
    val fanduel = fanduelPlayers.find(_.player.map(_.id).getOrElse("") == player.id)
    val draftkings = draftkingsPlayers.find(_.player.map(_.id).getOrElse("") == player.id)

    val (newName, newPosition) = fanduel match {
      case Some(fd) => (fd.nickname, fd.position: Position)
      case None => draftkings match {
        case Some(dk) => (dk.name, dk.position: Position)
        case None     => (player.name, player.position)
      }
    }

    val newTeam = fanduel.map(_.team).orElse(draftkings.map(_.team)).getOrElse(player.team)

    val newOpponent = fanduel.map(_.opponent).orElse(draftkings.map(_.opponent))

    val newBattingPosition = fanduel.flatMap(_.battingOrder) match {
      case None if (fanduel.flatMap(_.probablePitcher).getOrElse(false) == true) =>
        val filledSpots = fanduelPlayers.filter(_.team == player.team).flatMap(_.battingOrder).filterNot(_ == 0)
        val unfilledSpots = (1 to 9).toList.diff(filledSpots)
        //println(s"$player --> ${filledSpots.length} filled lineup spots: ${filledSpots.sorted.mkString(",")}\n\tunfilled: ${unfilledSpots.sorted.mkString(",")}")
        if (unfilledSpots.length == 1) Some(unfilledSpots.head)
        else None
      case bp => bp
    }

    val visitingOrHomeTeam: Option[VisitingOrHomeTeam] = fanduel.map(_.game) match {
      case Some(gameInfo) =>
        val visitingTeam = Teams.get(gameInfo.trim.substringBefore("@"))
        val homeTeam = Teams.get(gameInfo.trim.substringAfter("@"))
        fanduel.map(_.team) match {
          case Some(team) =>
            if (team == visitingTeam) Some(Visiting)
            else if (team == homeTeam) Some(Home)
            else None
          case None => throw new Exception(player + " has no FD team!")
        }
      case None => draftkings.map(_.game) match {
        case Some(gameInfo) =>
          val visitingTeam = Teams.get(gameInfo.trim.substringBefore("@"))
          val homeTeam = Teams.get(gameInfo.trim.substringsBetween("@", " ").head)
          draftkings.map(_.team) match {
            case Some(team) =>
              if (team == visitingTeam) Some(Visiting)
              else if (team == homeTeam) Some(Home)
              else None
            case None => throw new Exception(player + " has no DK team!")
          }
        case None => None
      }
    }

    player.copy(name = newName, position = newPosition, team = newTeam, opponent = newOpponent,
      fanduel = fanduel.map(p => PlayerSiteInfo(p.nickname, p.team, p.position, p.salary,
        p.probablePitcher.orElse(p.battingOrder.map(_ > 0)), newBattingPosition)),
      draftkings = draftkings.map(p => PlayerSiteInfo(p.name, p.team, p.position, p.salary, None, None)),
      retrosheet = Some(retrosheet),
      visitingOrHomeTeam = visitingOrHomeTeam)
  }

  val playersByID: Map[PlayerID, Player] = allPlayers.map { p => (p.id, p) }.toMap

  val playersByTeam: Map[Team, List[Player]] = allPlayers.groupBy(_.team)

  val startingPlayers: List[Player] = allPlayers.filter(p => p.isStarting && !teamsNotPlaying.contains(p.team))

  startingPlayers.filter { p => p.fanduel.isEmpty && p.draftkings.isEmpty } match {
    case Nil              => // OK
    case unmatchedPlayers => throw new Exception("Starting players with no FD or DK matches: \n\t" + unmatchedPlayers.mkString("\n\t"))
  }

  val (startingPitchers: List[Player], startingHitters: List[Player]) = startingPlayers.partition(_.position == Pitcher)

  val startingPlayersByTeam: Map[Team, List[Player]] = startingPlayers.groupBy(_.team)

  val startingHittersByTeam: Map[Team, List[Player]] = startingHitters.groupBy(_.team).map { case (team, hitters) => (team, hitters.sortBy(_.battingPosition.getOrElse(10))) }

  //    println("\nStarters: \n" + startingPlayersByTeam.map {
  //      case (team, players) =>
  //        s"$team:\n\t" + players.sortBy(_.battingPosition.getOrElse(0))
  //          .map(p => p.battingPosition.getOrElse(0) + ") " + p).mkString("\n\t")
  //    }.mkString("\n"))

  def get(playerID: String): Player = playersByID.get(playerID).get // throws exception if playerID is invalid
}