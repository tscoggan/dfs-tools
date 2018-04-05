package mlb.rg

import utils.FileUtils._
import utils.Logger._
import utils.StringUtils._
import mlb.model._
import mlb.model.CustomTypes._
import mlb._
import scala.io.Source
import scala.annotation.tailrec

object StartingLineups {

  case class PlayerMapping(retrosheetID: PlayerID, rgPlayerName: String)

  private val playerMappings: List[PlayerMapping] = Source.fromFile(Configs.Rotogrinders.playerMappingsFile).getLines.toList.tail
    .map(_.substringBefore("//").trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>
        val Array(retrosheetID, rgPlayerName) = nextLine.splitCSV()
        PlayerMapping(retrosheetID, rgPlayerName)
    }
  log(s"Found ${playerMappings.length} Retrosheet-to-RG player mappings")

  lazy val battingOrderByTeam: Map[Team, BattingOrder] = {
    val lines = Source.fromFile(Configs.Rotogrinders.projectedStartersFile).getLines.toList.map(_.substringBefore("//").trim).filter(_.nonEmpty)
    if (lines.length % 10 != 0) throw new Exception("Configs.Rotogrinders.projectedStartersFile has invalid # of lines: " + lines.length)

    @tailrec
    def parseNext(remaining: List[String], result: List[BattingOrder]): List[BattingOrder] = remaining match {
      case Nil => result
      case teamID :: lines =>
        val team = Teams.get(teamID)
        val batters = lines.take(9).map { line =>
          val position :: first :: last :: otherStuff = line.split(" ").toList
          Players.playersByTeam(team).find { p =>
            playerMappings.find(_.rgPlayerName.toUpperCase == s"$first $last".toUpperCase) match {
              case Some(mapping) =>
                // mapping found --> use it
                p.id == mapping.retrosheetID
              case None => {
                // no Retrosheet-to-RG mapping found --> try to match a player by name
                p.name.toUpperCase == s"$first $last".toUpperCase ||
                  p.fanduel.map(_.name.toUpperCase).getOrElse("") == s"$first $last".toUpperCase ||
                  p.draftkings.map(_.name.toUpperCase).getOrElse("") == s"$first $last".toUpperCase
              }
            }
          } match {
            case Some(player) => player
            case None         => throw new Exception(s"Couldn't find player named $first $last on $team --> please add to ${Configs.Rotogrinders.playerMappingsFile}")
          }
        }
        parseNext(lines.drop(9), BattingOrder(team, batters) :: result)
    }

    parseNext(lines, Nil).map(bo => (bo.team, bo)).toMap
  }

  lazy val all: List[BattingOrder] = battingOrderByTeam.values.toList

  def isStarting(player: Player): Boolean = battingOrderByTeam.get(player.team) match {
    case Some(battingOrder) => battingOrder.contains(player)
    case None               => false
  }

  def battingPositionOf(player: Player): Option[Int] = battingOrderByTeam.get(player.team) match {
    case Some(battingOrder) => battingOrder.positionOf(player)
    case None               => None
  }

}

case class BattingOrder(team: Team, batters: List[Player]) {

  def contains(player: Player): Boolean = batters.contains(player)

  def batter(position: Int): Player = {
    require(position >= 1 && position <= 9, s"Invalid batting position: $position")
    batters(position)
  }

  def positionOf(batter: Player): Option[Int] = batters.indexOf(batter) match {
    case -1       => None
    case position => Some(position + 1)
  }

  override def toString: String = team + " batting order:\n\t" + batters.map(p => positionOf(p).get + ") " + p).mkString("\n\t")

}