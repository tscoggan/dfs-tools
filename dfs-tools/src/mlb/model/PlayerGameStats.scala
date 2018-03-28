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

  lazy val player: Player = Players.get(playerID)

  var atBats = 0 // actually a count of plate appearances, because walks and sac flies/bunts are included
  def addAtBat = {
    atBats += 1
    logDebug(s"$this atBats += 1")
  }

  var singles = 0
  def addSingle = {
    singles += 1
    logDebug(s"$this singles += 1")
  }

  var doubles = 0
  def addDouble = {
    doubles += 1
    logDebug(s"$this doubles += 1")
  }

  var triples = 0
  def addTriple = {
    triples += 1
    logDebug(s"$this triples += 1")
  }

  var homeRuns = 0
  def addHR = {
    homeRuns += 1
    logDebug(s"$this homeRuns += 1")
  }

  var rbi = 0
  def addRBI(runs: Int) = {
    rbi += runs
    if (runs != 0) logDebug(s"$this rbi += $runs")
  }

  var runs = 0
  def addRun = {
    runs += 1
    logDebug(s"$this runs += 1")
  }

  var stolenBases = 0
  def addStolenBase = {
    stolenBases += 1
    logDebug(s"$this stolenBases += 1")
  }

  var walks = 0
  def addWalk = {
    walks += 1
    logDebug(s"$this walks += 1")
  }

  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = scoringSystem.calculateFantasyPoints(this)

  override def toString: String = Players.get(playerID).toString

  def printStats: String = battingPosition + ") " + this.toString +
    s" - AB: $atBats, 1B: $singles, 2B: $doubles, 3B: $triples, HR: $homeRuns, RBI: $rbi, R: $runs, SB: $stolenBases, W: $walks [FPTS: ${fantasyPoints()}]"
}

case class HitterGameStats(gameDate: Date, playerID: PlayerID, isStarter: Boolean, var battingPosition: Int) extends PlayerGameStats

case class PitcherGameStats(gameDate: Date, playerID: PlayerID, isStarter: Boolean, var battingPosition: Int) extends PlayerGameStats {
  val hittingStatsAgainst: Map[Handedness, HittingStatsAgainst] = Map(Left -> new HittingStatsAgainst, Right -> new HittingStatsAgainst, Switch -> new HittingStatsAgainst)

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

  // FPTS scored by batters against this pitcher
  def fantasyPointsAgainst(scoringSystem: DFSScoringSystem, hitterHandedness: Option[Handedness] = None): Float = {
    val hittingStats = HitterGameStats(null, "", false, 0)
    hittingStats.atBats = atBatsAgainst(hitterHandedness)
    hittingStats.singles = singlesAgainst(hitterHandedness)
    hittingStats.doubles = doublesAgainst(hitterHandedness)
    hittingStats.triples = triplesAgainst(hitterHandedness)
    hittingStats.homeRuns = homeRunsAgainst(hitterHandedness)
    hittingStats.rbi = rbisAgainst(hitterHandedness)
    hittingStats.runs = runsAgainst(hitterHandedness)
    hittingStats.stolenBases = stolenBasesAgainst(hitterHandedness)
    hittingStats.walks = walksAgainst(hitterHandedness)
    scoringSystem.calculateFantasyPoints(hittingStats)
  }

  def atBatsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).atBatsAgainst
    case None    => hittingStatsAgainst.values.map(_.atBatsAgainst).sum
  }

  def singlesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).singlesAgainst
    case None    => hittingStatsAgainst.values.map(_.singlesAgainst).sum
  }

  def doublesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).doublesAgainst
    case None    => hittingStatsAgainst.values.map(_.doublesAgainst).sum
  }

  def triplesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).triplesAgainst
    case None    => hittingStatsAgainst.values.map(_.triplesAgainst).sum
  }

  def homeRunsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).homeRunsAgainst
    case None    => hittingStatsAgainst.values.map(_.homeRunsAgainst).sum
  }

  def rbisAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).rbiAgainst
    case None    => hittingStatsAgainst.values.map(_.rbiAgainst).sum
  }

  def runsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).runsAgainst
    case None    => hittingStatsAgainst.values.map(_.runsAgainst).sum
  }

  def stolenBasesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).stolenBasesAgainst
    case None    => hittingStatsAgainst.values.map(_.stolenBasesAgainst).sum
  }

  def walksAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAgainst(h).walksAgainst
    case None    => hittingStatsAgainst.values.map(_.walksAgainst).sum
  }

  override def printStats: String = "P) " + this.toString +
    s" - Outs: $outs, H: $hitsAgainst, W: $walksAgainst, ER: $earnedRuns, K: $strikeouts" + { if (win > 0) ", Win" else "" } +
    { if (loss > 0) ", Loss" else "" } + { if (save > 0) ", Save" else "" } + { if (qStart > 0) ", Q-Start" else "" } + s" [FPTS: ${fantasyPoints()}]"
}

class HittingStatsAgainst {
  var atBatsAgainst = 0
  def addAtBatAgainst = {
    atBatsAgainst += 1
  }

  var singlesAgainst = 0
  def addSingleAgainst = {
    singlesAgainst += 1
  }

  var doublesAgainst = 0
  def addDoubleAgainst = {
    doublesAgainst += 1
  }

  var triplesAgainst = 0
  def addTripleAgainst = {
    triplesAgainst += 1
  }

  var rbiAgainst = 0
  def addRBIAgainst(runs: Int) = {
    rbiAgainst += runs
  }

  var homeRunsAgainst = 0
  def addHomeRunAgainst = {
    homeRunsAgainst += 1
  }

  var runsAgainst = 0
  def addRunAgainst = {
    runsAgainst += 1
  }

  var stolenBasesAgainst = 0
  def addStolenBaseAgainst = {
    stolenBasesAgainst += 1
  }

  var walksAgainst = 0
  def addWalkAgainst = {
    walksAgainst += 1
  }
}