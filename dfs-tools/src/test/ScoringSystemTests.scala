package test

import org.scalatest.FunSuite
import mlb.model._
import utils.FloatUtils._

class ScoringSystemTests extends FunSuite {

  test("FanDuel MLB scoring system works for hitter") {
    val p = HitterGameStats("bellj005", true, 3)
    p.atBats = 5
    p.singles = 1
    p.doubles = 1
    p.triples = 1
    p.homeRuns = 1
    p.rbi = 1
    p.runs = 1
    p.stolenBases = 1
    p.walks = 1
    assert(p.fantasyPoints(FanDuelMLB) ~= 45.7)
  }
  
  test("FanDuel MLB scoring system works for hitter 2") {
    val p = HitterGameStats("bellj005", true, 3)
    p.atBats = 4
    p.triples = 1
    p.homeRuns = 1
    p.rbi = 2
    p.runs = 1
    assert(p.fantasyPoints(FanDuelMLB) ~= 31.2)
  }

  test("DraftKings MLB scoring system works for hitter") {
    val p = HitterGameStats("bellj005", true, 3)
    p.atBats = 5
    p.singles = 1
    p.doubles = 1
    p.triples = 1
    p.homeRuns = 1
    p.rbi = 1
    p.runs = 1
    p.stolenBases = 1
    p.walks = 1
    assert(p.fantasyPoints(DraftKingsMLB) ~= 37)
  }

  test("FanDuel MLB scoring system works for pitcher with qStart & win") {
    val p = PitcherGameStats("gonzg003", true, 0)
    p.hitsAgainst = 6
    p.walksAgainst = 3
    p.earnedRuns = 3
    p.win = 1
    p.strikeouts = 7
    p.outs = 18
    assert(p.fantasyPoints(FanDuelMLB) ~= 40)
  }
  
  test("FanDuel MLB scoring system works for pitcher without qStart & win") {
    val p = PitcherGameStats("gonzg003", true, 0)
    p.atBats = 5
    p.singles = 1
    p.doubles = 1
    p.triples = 1
    p.homeRuns = 1
    p.rbi = 1
    p.runs = 1
    p.stolenBases = 1
    p.walks = 1
    
    p.hitsAgainst = 6
    p.walksAgainst = 3
    p.earnedRuns = 4
    p.loss = 1
    p.strikeouts = 5
    p.outs = 20
    assert(p.fantasyPoints(FanDuelMLB) ~= 23)
  }
  
  test("FanDuel MLB scoring system works for pitcher with qStart but no win") {
    val p = PitcherGameStats("gonzg003", true, 0)    
    p.earnedRuns = 1
    p.strikeouts = 3
    p.outs = 18
    assert(p.fantasyPoints(FanDuelMLB) ~= 28)
  }
  
  test("DraftKings MLB scoring system works for pitcher with no hitter") {
    val p = PitcherGameStats("gonzg003", true, 0)
    p.atBats = 5
    p.singles = 1
    p.doubles = 1
    p.triples = 1
    p.homeRuns = 1
    p.rbi = 1
    p.runs = 1
    p.stolenBases = 1
    p.walks = 1
    
    p.hitsAgainst = 0
    p.walksAgainst = 1
    p.earnedRuns = 0
    p.win = 1
    p.strikeouts = 11
    p.outs = 27
    p.completeGame = 1
    println(p.fantasyPoints(DraftKingsMLB))
    assert(p.fantasyPoints(DraftKingsMLB) ~= 60.65)
  }

}