package mlb.model

import mlb.model.CustomTypes._

trait DFSScoringSystem {
  def calculateFantasyPoints(player: PlayerStats): Float
}

object FanDuelMLB extends DFSScoringSystem {
  override def toString: String = "FanDuel MLB"

  def calculateFantasyPoints(player: PlayerStats): Float = player match {
    case hitter: HittingStats =>
      (hitter.singles * 3) + (hitter.doubles * 6) + (hitter.triples * 9) + (hitter.homeRuns * 12) + (hitter.rbi * 3.5f) +
        (hitter.runs * 3.2f) + (hitter.walks * 3) + (hitter.stolenBases * 6)

    case pitcher: PitchingStats =>
      (pitcher.win * 6) + (pitcher.qStart * 4) + (pitcher.strikeouts * 3) + (pitcher.outs * 1) + (pitcher.earnedRuns * -3)
  }
}

object DraftKingsMLB extends DFSScoringSystem {
  override def toString: String = "DraftKings MLB"

  def calculateFantasyPoints(player: PlayerStats): Float = player match {
    case hitter: HittingStats =>
      (hitter.singles * 3) + (hitter.doubles * 5) + (hitter.triples * 8) + (hitter.homeRuns * 10) + (hitter.rbi * 2) +
        (hitter.runs * 2) + (hitter.walks * 2) + (hitter.stolenBases * 5)

    case pitcher: PitchingStats =>
      (pitcher.win * 4) + (pitcher.loss * -2) + (pitcher.strikeouts * 2) + (pitcher.outs * 0.75f) + (pitcher.earnedRuns * -2) +
        (pitcher.hitsAgainst * -0.6f) + (pitcher.walksAgainst * -0.6f) + (pitcher.completeGame * 2.5f) +
        { if (pitcher.completeGame == 1 && pitcher.earnedRuns == 0) 2.5f else 0 } + // complete game shutout
        { if (pitcher.completeGame == 1 && pitcher.hitsAgainst == 0) 10 else 0 } // no hitter
  }
}