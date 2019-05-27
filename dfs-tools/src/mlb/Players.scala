package mlb

import mlbdotcom._
import model._
import model.CustomTypes._
import utils.FileUtils._
import utils.Logger._
import utils.StringUtils._
import utils.DateTimeUtils._
import mlb.model._
import scala.io.Source
import java.util.Date

object Players {

  var projectionsDate: Date = today

  case class PlayerMapping(fdPlayerID: String, dkNameAndTeam: String, mlbPlayerID: MLBPlayerID)

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
        val Array(mlbPlayerID, fdPlayerID, dkNameAndTeam) = nextLine.splitCSV()
        PlayerMapping(fdPlayerID.substringAfter("-"), dkNameAndTeam, mlbPlayerID)
    }
  log(s"Found ${playerMappings.length} player mappings")

  val mlbDotComPlayers: List[Player_MLB] = Player_MLB.allPlayers
  log(s"Found ${mlbDotComPlayers.length} MLB.com players")

  val mlbDotComPlayersByID: Map[MLBPlayerID, Player_MLB] = mlbDotComPlayers.map(p => (p.id, p)).toMap

  //  mlbDotComPlayers.groupBy(_.name).foreach {
  //    case (name, players) => if (players.length > 1) log(s"WARNING: ${players.length} MLB.com players with same name: ${players.mkString(", ")}")
  //  }

  val fanduelPlayers: List[Player_FD] = {
    getListOfFiles(Configs.dfsSalaryFileDir, ".csv")
      .filter(_.getName.startsWith("FanDuel-MLB-")) match {
        case Nil => Nil
        case files =>
          val latestFile = files.sortBy { _.getName.trimPrefix("FanDuel-MLB-").take(10).toDate() }.last
          projectionsDate = latestFile.getName.trimPrefix("FanDuel-MLB-").take(10).toDate()
          log("Loading FD players from file " + latestFile)
          Player_FD.parseFrom(latestFile.getPath)
      }
  } //.filterNot { p => p.position == "P" && !p.probablePitcher.exists(_ == true) } // ignore non-starting pitchers
  log(s"Found ${fanduelPlayers.length} FD players")

  //  fanduelPlayers.groupBy(_.nickname).foreach {
  //    case (name, players) => if (players.length > 1) log(s"WARNING: ${players.length} FD players with same name: ${players.mkString(", ")}")
  //  }

  val draftkingsPlayers: List[Player_DK] = {
    getListOfFiles(Configs.dfsSalaryFileDir, ".csv")
      .filter(_.getName.startsWith("DraftKings-MLB-")) match {
        case Nil => Nil
        case files => {
          val file = files.sortBy(_.getName.trimPrefix("DraftKings-MLB-").take(10).toDate()).last
          log("Loading DK players from file " + file)
          Player_DK.parseFrom(file.getPath)
        }
      }
  } //.filterNot { p => p.position == "P" && !p.probablePitcher.exists(_ == true) } // ignore non-starting pitchers
  log(s"Found ${draftkingsPlayers.length} DK players")

  //  draftkingsPlayers.groupBy(_.name).foreach {
  //    case (name, players) => if (players.length > 1) log(s"WARNING: ${players.length} DK players with same name: ${players.mkString(", ")}")
  //  }

  val allPlayers: List[Player] = mlbDotComPlayers map (_.toPlayer)

  fanduelPlayers.filter(_.mlbPlayerID.isEmpty).foreach { p => log(s"WARNING: No MLB player found for FD player $p with alternate name [${p.alternateName}]") }

  val playersByID: Map[MLBPlayerID, Player] = allPlayers.map { p => (p.id, p) }.toMap

  val playersByTeam: Map[Team, List[Player]] = allPlayers.groupBy(_.team)

  val startingPlayers: List[Player] = allPlayers.filter(p => p.isStarting && !teamsNotPlaying.contains(p.team))

  startingPlayers.filter { p => p.fanduel.isEmpty && p.draftkings.isEmpty } match {
    case Nil              => // OK
    case unmatchedPlayers => throw new Exception("Starting players with no FD or DK matches: \n\t" + unmatchedPlayers.mkString("\n\t"))
  }

  val (startingPitchers: List[Player], startingHitters: List[Player]) =
    startingPlayers.partition(p => p.fanduel.map(_.position).orElse(p.draftkings.map(_.position)).getOrElse(p.position) == Pitcher)

  val startingPlayersByTeam: Map[Team, List[Player]] = startingPlayers.groupBy(_.team)

  val startingHittersByTeam: Map[Team, List[Player]] = startingHitters.groupBy(_.team).map { case (team, hitters) => (team, hitters.sortBy(_.battingPosition.getOrElse(10))) }

  //  log("\nStarters: \n" + startingPlayersByTeam.map {
  //    case (team, players) =>
  //      s"$team:\n\t" + players.sortBy(_.battingPosition.getOrElse(0))
  //        .map(p => p.battingPosition.getOrElse(0) + ") " + p).mkString("\n\t")
  //  }.mkString("\n"))

  val teamsOnSlate = startingPlayersByTeam.keys.toList

  val hitters_FD = startingHitters.filter(_.fanduel.map(_.salary).nonEmpty)
  val hitters_DK = startingHitters.filter(_.draftkings.map(_.salary).nonEmpty)

  val expensiveHitters_FD = hitters_FD.filter(_.fanduel.map(_.salary).get >= 3500)
  val midrangeHitters_FD = hitters_FD.filter(p => p.fanduel.map(_.salary).get < 3500 && p.fanduel.map(_.salary).get >= 2500)
  val cheapHitters_FD = hitters_FD.filter(p => p.fanduel.map(_.salary).get < 2500)

  val expensiveHitters_DK = hitters_DK.filter(_.draftkings.map(_.salary).get >= 4500)
  val midrangeHitters_DK = hitters_DK.filter(p => p.draftkings.map(_.salary).get < 4500 && p.draftkings.map(_.salary).get >= 3500)
  val cheapHitters_DK = hitters_DK.filter(p => p.draftkings.map(_.salary).get < 3500)

  teamsOnSlate.filter(t => !startingPitchers.map(_.team).contains(t)) match {
    case Nil      => // OK
    case notFound => println("\n\nWARNING: No starting pitcher found for " + notFound.mkString(", "))
  }

  teamsOnSlate.foreach { team =>
    val pitchers = startingPitchers.filter(_.team == team)
    if (pitchers.isEmpty) throw new Exception("No starting pitcher for " + team)
    if (pitchers.length > 1) throw new Exception(s"Multiple starting pitchers for $team: ${pitchers.mkString(",")}")
  }

  def find(playerName: String, team: Team): Option[Player] = {
    val alternateName = playerName.substringBefore(" Jr.").replaceAll("\\.", "").trim
    playersByTeam(team).find { p =>
      (p.team == team) && (p.name.toUpperCase == playerName.toUpperCase ||
        p.fanduel.map(_.name.toUpperCase).getOrElse("") == playerName.toUpperCase ||
        p.draftkings.map(_.name.toUpperCase).getOrElse("") == playerName.toUpperCase ||
        p.alternateName.toUpperCase == alternateName.toUpperCase)
    }
  }

  def get(playerID: String): Player = playersByID.get(playerID).get // throws exception if playerID is invalid
}