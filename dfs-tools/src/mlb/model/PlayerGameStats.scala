package mlb.model

import mlb._
import CustomTypes._
import utils.Logger._
import java.util.Date

/**
 * Single-game stats for a player
 */
trait PlayerGameStats {
  var game: Option[Game] = None
  val gameDate: Date
  val playerID: PlayerID
  val isStarter: Boolean
  var battingPosition: Int

  val hittingStats: HittingStats = new HittingStats

  lazy val player: Player = Players.get(playerID)

  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float

  override def toString: String = Players.get(playerID).toString

  def printStats: String
}

case class HitterGameStats(gameDate: Date, playerID: PlayerID, isStarter: Boolean, var battingPosition: Int) extends PlayerGameStats {

  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = scoringSystem.calculateFantasyPoints(this.hittingStats)

  override def printStats: String = battingPosition + ") " + this.toString +
    s" - AB: ${hittingStats.atBats}, 1B: ${hittingStats.singles}, 2B: ${hittingStats.doubles}, 3B: ${hittingStats.triples}, " +
    s"HR: ${hittingStats.homeRuns}, RBI: ${hittingStats.rbi}, R: ${hittingStats.runs}, SB: ${hittingStats.stolenBases}, W: ${hittingStats.walks} " +
    s"[FPTS: ${fantasyPoints()}]"

}

case class PitcherGameStats(gameDate: Date, playerID: PlayerID, isStarter: Boolean, var battingPosition: Int) extends PlayerGameStats {
  val pitchingStats: PitchingStats = new PitchingStats
  val hittingStatsAgainst: Map[Handedness, HittingStats] = Map(Left -> new HittingStats, Right -> new HittingStats, Switch -> new HittingStats)

  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = scoringSystem.calculateFantasyPoints(this.pitchingStats)

  // FPTS scored by batters against this pitcher
  def fantasyPointsAgainst(scoringSystem: DFSScoringSystem, hitterHandedness: Option[Handedness] = None): Float = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).fantasyPoints(scoringSystem)
    case None    => hittingStatsAgainst.values.map(_.fantasyPoints(scoringSystem)).sum
  }

  def atBatsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).atBats
    case None    => hittingStatsAgainst.values.map(_.atBats).sum
  }

  def singlesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).singles
    case None    => hittingStatsAgainst.values.map(_.singles).sum
  }

  def doublesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).doubles
    case None    => hittingStatsAgainst.values.map(_.doubles).sum
  }

  def triplesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).triples
    case None    => hittingStatsAgainst.values.map(_.triples).sum
  }

  def homeRunsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).homeRuns
    case None    => hittingStatsAgainst.values.map(_.homeRuns).sum
  }

  def rbisAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).rbi
    case None    => hittingStatsAgainst.values.map(_.rbi).sum
  }

  def runsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).runs
    case None    => hittingStatsAgainst.values.map(_.runs).sum
  }

  def stolenBasesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).stolenBases
    case None    => hittingStatsAgainst.values.map(_.stolenBases).sum
  }

  def walksAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).walks
    case None    => hittingStatsAgainst.values.map(_.walks).sum
  }

  override def printStats: String = "P) " + this.toString +
    s" - Outs: ${pitchingStats.outs}, H: ${pitchingStats.hitsAgainst}, W: ${pitchingStats.walksAgainst}, ER: ${pitchingStats.earnedRuns}, " +
    s"K: ${pitchingStats.strikeouts}" + { if (pitchingStats.win > 0) ", Win" else "" } + { if (pitchingStats.loss > 0) ", Loss" else "" } +
    { if (pitchingStats.save > 0) ", Save" else "" } + { if (pitchingStats.qStart > 0) ", Q-Start" else "" } + s" [FPTS: ${fantasyPoints()}]"
}

trait PlayerStats {
  
  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = scoringSystem.calculateFantasyPoints(this)

}

class HittingStats extends PlayerStats {
  var atBats = 0
  def addAtBat = {
    atBats += 1
  }

  var singles = 0
  def addSingle = {
    singles += 1
  }

  var doubles = 0
  def addDouble = {
    doubles += 1
  }

  var triples = 0
  def addTriple = {
    triples += 1
  }

  var rbi = 0
  def addRBI(runs: Int) = {
    rbi += runs
  }

  var homeRuns = 0
  def addHomeRun = {
    homeRuns += 1
  }

  var runs = 0
  def addRun = {
    runs += 1
  }

  var stolenBases = 0
  def addStolenBase = {
    stolenBases += 1
  }

  var walks = 0
  def addWalk = {
    walks += 1
  }

}

class PitchingStats extends PlayerStats {

  var hitsAgainst = 0
  def addHitAgainst = {
    hitsAgainst += 1
    logDebug(s"$this hitsAgainst += 1")
  }

  var walksAgainst = 0
  def addWalkAgainst = {
    walksAgainst += 1
    logDebug(s"$this walksAgainst += 1")
  }

  var earnedRuns = 0
  def addEarnedRuns(runs: Int) = {
    earnedRuns += runs
    if (runs != 0) logDebug(s"$this earnedRuns += $runs")
  }

  var strikeouts = 0
  def addStrikeout = {
    strikeouts += 1
    logDebug(s"$this strikeouts += 1")
  }

  var outs = 0
  def addOuts(numberOfOuts: Int) = {
    outs += numberOfOuts
    if (numberOfOuts != 0) logDebug(s"$this outs += $numberOfOuts")
  }

  def qStart = if (outs >= 18 && earnedRuns <= 3) 1 else 0

  var win = 0
  var loss = 0
  var save = 0
  var completeGame = 0
}