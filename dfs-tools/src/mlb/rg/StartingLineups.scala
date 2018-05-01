package mlb.rg

import utils.FileUtils._
import utils.Logger._
import utils.StringUtils._
import mlb.model._
import mlb.model.CustomTypes._
import mlb._
import scala.io.Source
import scala.annotation.tailrec

/**
 * rg_starting_lineups.txt syntax example:
 *
 * MIL
 * 1 Lorenzo Cain OF R  0
 * 2 Christian Yelich OF L  0
 * 3 Ryan Braun 1B R  0
 * 4 Travis Shaw 3B L  0
 * 5 Eric Thames 1B L  0
 * 6 Manuel Pina C R  0
 * 7 Jonathan Villar 2B S  0
 * 8 Orlando Arcia SS R  0
 * 9 Zach Davies SP R  0
 *
 * KAN
 * 1 Jon Jay OF L  0
 * 2 Whit Merrifield 2B R  0
 * 3 Mike Moustakas 3B L  0
 * 4 Lucas Duda 1B L  0
 * 5 Paulo Orlando OF R  0
 * 6 Jorge Soler OF R  0
 * 7 Alex Gordon OF L  0
 * 8 Alcides Escobar SS R  0
 * 9 Drew Butera C R  0
 * P Ian Kennedy                  <--- Add this line if pitcher isn't found because he's only on DK and isn't batting
 */
object StartingLineups {

  case class PlayerMapping(mlbPlayerID: MLBPlayerID, rgPlayerName: String, team: Team)

  private val playerMappings: List[PlayerMapping] = Source.fromFile(Configs.Rotogrinders.playerMappingsFile).getLines.toList.tail
    .map(_.substringBefore("//").trim)
    .filter(_.nonEmpty)
    .map {
      case nextLine =>
        val Array(mlbPlayerID, rgPlayerName, teamID) = nextLine.splitCSV()
        PlayerMapping(mlbPlayerID, rgPlayerName, Teams.get(teamID))
    }
  log(s"Found ${playerMappings.length} MLB-to-RG player mappings")

  lazy val battingOrderByTeam: Map[Team, BattingOrder] = {
    val lines = Source.fromFile(Configs.Rotogrinders.projectedStartersFile).getLines.toList.map(_.substringBefore("//").trim)
    if (lines.filter(l => l.nonEmpty && !l.startsWith("P ")).length % 10 != 0) throw new Exception("Configs.Rotogrinders.projectedStartersFile has invalid # of lines: " + lines.length)

    @tailrec
    def parseNext(remaining: List[String], result: List[BattingOrder]): List[BattingOrder] = remaining match {
      case Nil => result
      case teamID :: lines =>
        val team = Teams.get(teamID)
        val batters = lines.takeWhile(_.nonEmpty).map { line =>
          val position :: first :: last :: otherStuff = line.splitOnSpace().toList
          (playerMappings.find(m => m.rgPlayerName.toUpperCase == s"$first $last".toUpperCase && m.team == team) match {
            case Some(mapping) =>
              // mapping found --> use it
              Players.playersByID.get(mapping.mlbPlayerID)
            case None => {
              // no MLB-to-RG mapping found --> try to match a player by name & team
              Players.find(s"$first $last", team)
            }
          }) match {
            case Some(player) => player
            case None         => throw new Exception(s"Couldn't find player named $first $last on $team --> please add to ${Configs.Rotogrinders.playerMappingsFile}")
          }
        }
        parseNext(lines.drop(batters.length).dropWhile(_.isEmpty), BattingOrder(team, batters) :: result)
    }

    parseNext(lines, Nil).map(bo => (bo.team, bo)).toMap
  }

  lazy val all: List[BattingOrder] = battingOrderByTeam.values.toList
  
  //log(all.mkString("\n\n"))

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

  override def toString: String = team + " batting order:\n\t" + batters.take(9).map(p => positionOf(p).get + ") " + p).mkString("\n\t")

}