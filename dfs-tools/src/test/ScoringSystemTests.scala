package test

import org.scalatest.FunSuite
import mlb.model._
import utils.FloatUtils._

class ScoringSystemTests extends FunSuite {
  
  val date = java.util.Calendar.getInstance.getTime

  test("FanDuel MLB scoring system works for hitter") {
    val p = HitterGameStats(date, "bellj005", true, 3)
    p.hittingStats.atBats = 5
    p.hittingStats.singles = 1
    p.hittingStats.doubles = 1
    p.hittingStats.triples = 1
    p.hittingStats.homeRuns = 1
    p.hittingStats.rbi = 1
    p.hittingStats.runs = 1
    p.hittingStats.stolenBases = 1
    p.hittingStats.walks = 1
    assert(p.fantasyPoints(FanDuelMLB) ~= 45.7)
  }
  
  test("FanDuel MLB scoring system works for hitter 2") {
    val p = HitterGameStats(date, "bellj005", true, 3)
    p.hittingStats.atBats = 4
    p.hittingStats.triples = 1
    p.hittingStats.homeRuns = 1
    p.hittingStats.rbi = 2
    p.hittingStats.runs = 1
    assert(p.fantasyPoints(FanDuelMLB) ~= 31.2)
  }

  test("DraftKings MLB scoring system works for hitter") {
    val p = HitterGameStats(date, "bellj005", true, 3)
    p.hittingStats.atBats = 5
    p.hittingStats.singles = 1
    p.hittingStats.doubles = 1
    p.hittingStats.triples = 1
    p.hittingStats.homeRuns = 1
    p.hittingStats.rbi = 1
    p.hittingStats.runs = 1
    p.hittingStats.stolenBases = 1
    p.hittingStats.walks = 1
    assert(p.fantasyPoints(DraftKingsMLB) ~= 37)
  }

  test("FanDuel MLB scoring system works for pitcher with qStart & win") {
    val p = PitcherGameStats(date, "gonzg003", true, 0)
    p.pitchingStats.hitsAgainst = 6
    p.pitchingStats.walksAgainst = 3
    p.pitchingStats.earnedRuns = 3
    p.pitchingStats.win = 1
    p.pitchingStats.strikeouts = 7
    p.pitchingStats.outs = 18
    assert(p.fantasyPoints(FanDuelMLB) ~= 40)
  }
  
  test("FanDuel MLB scoring system works for pitcher without qStart & win") {
    val p = PitcherGameStats(date, "gonzg003", true, 0)
    p.hittingStats.atBats = 5
    p.hittingStats.singles = 1
    p.hittingStats.doubles = 1
    p.hittingStats.triples = 1
    p.hittingStats.homeRuns = 1
    p.hittingStats.rbi = 1
    p.hittingStats.runs = 1
    p.hittingStats.stolenBases = 1
    p.hittingStats.walks = 1
    
    p.pitchingStats.hitsAgainst = 6
    p.pitchingStats.walksAgainst = 3
    p.pitchingStats.earnedRuns = 4
    p.pitchingStats.loss = 1
    p.pitchingStats.strikeouts = 5
    p.pitchingStats.outs = 20
    assert(p.fantasyPoints(FanDuelMLB) ~= 23)
  }
  
  test("FanDuel MLB scoring system works for pitcher with qStart but no win") {
    val p = PitcherGameStats(date, "gonzg003", true, 0)    
    p.pitchingStats.earnedRuns = 1
    p.pitchingStats.strikeouts = 3
    p.pitchingStats.outs = 18
    assert(p.fantasyPoints(FanDuelMLB) ~= 28)
  }
  
  test("DraftKings MLB scoring system works for pitcher with no hitter") {
    val p = PitcherGameStats(date, "gonzg003", true, 0)
    p.hittingStats.atBats = 5
    p.hittingStats.singles = 1
    p.hittingStats.doubles = 1
    p.hittingStats.triples = 1
    p.hittingStats.homeRuns = 1
    p.hittingStats.rbi = 1
    p.hittingStats.runs = 1
    p.hittingStats.stolenBases = 1
    p.hittingStats.walks = 1
    
    p.pitchingStats.hitsAgainst = 0
    p.pitchingStats.walksAgainst = 1
    p.pitchingStats.earnedRuns = 0
    p.pitchingStats.win = 1
    p.pitchingStats.strikeouts = 11
    p.pitchingStats.outs = 27
    p.pitchingStats.completeGame = 1
    println(p.fantasyPoints(DraftKingsMLB))
    assert(p.fantasyPoints(DraftKingsMLB) ~= 60.65)
  }

}