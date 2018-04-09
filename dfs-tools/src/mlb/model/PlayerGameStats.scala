package mlb.model

import mlb._
import CustomTypes._
import utils.Logger._
import java.util.Date
import scala.collection.mutable

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

  def addAtBatAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addAtBat
    pitcher.pitchingStats.addAtBat
    pitcher.pitchingStatsAgainst(player, true).addAtBat
    pitcher.hittingStatsAllowedTo(player, true).addAtBat
    logDebug(s"$this atBats += 1")
  }

  def addStrikeoutAgainst(pitcher: PitcherGameStats) = {
    pitcher.pitchingStats.addStrikeout
    pitcher.pitchingStatsAgainst(player, true).addStrikeout
  }

  def addOutsAgainst(numberOfOuts: Int, pitcher: PitcherGameStats) = {
    pitcher.pitchingStats.addOuts(numberOfOuts)
    pitcher.pitchingStatsAgainst(player, true).addOuts(numberOfOuts)
  }

  def addSingleAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addSingle
    pitcher.pitchingStats.addHitAgainst
    pitcher.pitchingStatsAgainst(player, true).addHitAgainst
    pitcher.hittingStatsAllowedTo(player, true).addSingle
    logDebug(s"$this singles += 1")
  }

  def addDoubleAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addDouble
    pitcher.pitchingStats.addHitAgainst
    pitcher.pitchingStatsAgainst(player, true).addHitAgainst
    pitcher.hittingStatsAllowedTo(player, true).addDouble
    logDebug(s"$this doubles += 1")
  }

  def addTripleAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addTriple
    pitcher.pitchingStats.addHitAgainst
    pitcher.pitchingStatsAgainst(player, true).addHitAgainst
    pitcher.hittingStatsAllowedTo(player, true).addTriple
    logDebug(s"$this triples += 1")
  }

  def addHomeRunAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addHomeRun
    pitcher.pitchingStats.addHitAgainst
    pitcher.pitchingStatsAgainst(player, true).addHitAgainst
    pitcher.hittingStatsAllowedTo(player, true).addHomeRun
    logDebug(s"$this homeRuns += 1")
  }

  def addRBIAgainst(pitcher: PitcherGameStats, runs: Int) = {
    hittingStats.addRBI(runs)
    pitcher.pitchingStatsAgainst(player, true).addEarnedRuns(runs) // is this correct???
    pitcher.hittingStatsAllowedTo(player, true).addRBI(runs)
    if (runs != 0) logDebug(s"$this rbi += $runs")
  }

  def addRunAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addRun
    pitcher.hittingStatsAllowedTo(player, true).addRun
    logDebug(s"$this runs += 1")
  }

  def addStolenBaseAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addStolenBase
    pitcher.hittingStatsAllowedTo(player, true).addStolenBase
    logDebug(s"$this stolenBases += 1")
  }

  def addWalkAgainst(pitcher: PitcherGameStats) = {
    hittingStats.addWalk
    pitcher.pitchingStats.addWalkAgainst
    pitcher.pitchingStatsAgainst(player, true).addWalkAgainst
    pitcher.hittingStatsAllowedTo(player, true).addWalk
    logDebug(s"$this walks += 1")
  }
}

case class HitterGameStats(gameDate: Date, playerID: PlayerID, isStarter: Boolean, var battingPosition: Int) extends PlayerGameStats {

  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = scoringSystem.calculateFantasyPoints(this.hittingStats)

  override def printStats: String = battingPosition + ") " + this.toString + " - " + hittingStats

}

case class PitcherGameStats(gameDate: Date, playerID: PlayerID, isStarter: Boolean, var battingPosition: Int) extends PlayerGameStats {

  val pitchingStats: PitchingStats = new PitchingStats

  private val pitchingStatsByHitter: mutable.Map[Player, PitchingStats] = mutable.Map.empty

  private val hittingStatsAllowedByHitter: mutable.Map[Player, HittingStats] = mutable.Map.empty

  def pitchingStatsAgainst(hitter: Player, updateStats: Boolean = false): PitchingStats = pitchingStatsByHitter.get(hitter) match {
    case Some(stats) => stats
    case None => synchronized {
      val stats = new PitchingStats
      if (updateStats) pitchingStatsByHitter(hitter) = stats
      stats
    }
  }

  def hittingStatsAllowedTo(hitter: Player, updateStats: Boolean = false): HittingStats = hittingStatsAllowedByHitter.get(hitter) match {
    case Some(stats) => stats
    case None => synchronized {
      val stats = new HittingStats
      if (updateStats) hittingStatsAllowedByHitter(hitter) = stats
      stats
    }
  }
  
  lazy val hittingStatsAllowed: List[HittingStats] = hittingStatsAllowedByHitter.values.toList

  def fantasyPoints(scoringSystem: DFSScoringSystem = Configs.dfsScoringSystem): Float = scoringSystem.calculateFantasyPoints(this.pitchingStats)

  // FPTS scored by batters against this pitcher
  def fantasyPointsAgainst(scoringSystem: DFSScoringSystem, hitterHandedness: Option[Handedness] = None): Float = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.fantasyPoints(scoringSystem)).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.fantasyPoints(scoringSystem)).sum
  }

  def atBatsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.atBats).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.atBats).sum
  }

  def singlesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.singles).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.singles).sum
  }

  def doublesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.doubles).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.doubles).sum
  }

  def triplesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.triples).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.triples).sum
  }

  def homeRunsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.homeRuns).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.homeRuns).sum
  }

  def rbisAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.rbi).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.rbi).sum
  }

  def runsAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.runs).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.runs).sum
  }

  def stolenBasesAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.stolenBases).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.stolenBases).sum
  }

  def walksAgainst(hitterHandedness: Option[Handedness] = None): Int = hitterHandedness match {
    case Some(h) => hittingStatsAllowedByHitter.filterKeys(_.bats == h).values.map(_.walks).sum
    case None    => hittingStatsAllowedByHitter.values.map(_.walks).sum
  }

  override def printStats: String = "P) " + this.toString + " - " + pitchingStats

  def addEarnedRuns(runs: Int) = {
    pitchingStats.addEarnedRuns(runs)
    if (runs != 0) logDebug(s"$this earnedRuns += $runs")
  }

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

  override def toString: String = s"AB: ${atBats}, 1B: ${singles}, 2B: ${doubles}, 3B: ${triples}, " +
    s"HR: ${homeRuns}, RBI: ${rbi}, R: ${runs}, SB: ${stolenBases}, W: ${walks} [FPTS: ${fantasyPoints()}]"

}

class PitchingStats extends PlayerStats {

  var atBats = 0
  def addAtBat = {
    atBats += 1
  }

  var hitsAgainst = 0
  def addHitAgainst = {
    hitsAgainst += 1
  }

  var walksAgainst = 0
  def addWalkAgainst = {
    walksAgainst += 1
  }

  var earnedRuns = 0
  def addEarnedRuns(runs: Int) = {
    earnedRuns += runs
  }

  var strikeouts = 0
  def addStrikeout = {
    strikeouts += 1
  }

  var outs = 0
  def addOuts(numberOfOuts: Int) = {
    outs += numberOfOuts
  }

  def qStart = if (outs >= 18 && earnedRuns <= 3) 1 else 0

  var win = 0
  var loss = 0
  var save = 0
  var completeGame = 0

  override def toString: String = s"Outs: ${outs}, H: ${hitsAgainst}, W: ${walksAgainst}, ER: ${earnedRuns}, " +
    s"K: ${strikeouts}" + { if (win > 0) ", Win" else "" } + { if (loss > 0) ", Loss" else "" } +
    { if (save > 0) ", Save" else "" } + { if (qStart > 0) ", Q-Start" else "" } + s" [FPTS: ${fantasyPoints()}]"
}