package test

import org.scalatest.FunSuite
import mlb.mlbdotcom.MLBGameParser._
import utils.StringUtils._
import mlb.model.FanDuelMLB
import mlb.Players

class MLBParserTests extends FunSuite {

  val game = mlb.mlbdotcom.Game_MLB.loadGameFromURL("""http://gd2.mlb.com/components/game/mlb/year_2019/month_07/day_06/gid_2019_07_06_texmlb_minmlb_1/""").get
  val game2 = mlb.mlbdotcom.Game_MLB.loadGameFromURL("""http://gd2.mlb.com/components/game/mlb/year_2019/month_07/day_04/gid_2019_07_04_minmlb_oakmlb_1/""").get
  val game3 = mlb.mlbdotcom.Game_MLB.loadGameFromURL("""http://gd2.mlb.com/components/game/mlb/year_2019/month_06/day_21/gid_2019_06_21_miamlb_phimlb_1/""").get
  val game4 = mlb.mlbdotcom.Game_MLB.loadGameFromURL("""http://gd2.mlb.com/components/game/mlb/year_2019/month_07/day_07/gid_2019_07_07_anamlb_houmlb_1/""").get

  test("cleanName works") {
    assert(cleanName("Tim Smith") == "Tim Smith" && cleanName("Tim Smith Jr") == "Tim Smith" &&
      cleanName("Tim Smith Jr.") == "Tim Smith")
  }

  test("Correct date") {
    assert(game.date == "2019-07-06".toDate())
  }

  test("Correct teams") {
    assert(game.visitingTeam.toString == "TEX" && game.homeTeam.toString == "MIN")
  }

  test("Correct game number") {
    assert(game.gameNumber == 1)
  }

  test("Correct winning, losing, & save pitchers") {
    assert(game.winningPitcher.id == "501381" && game.losingPitcher.id == "445926" && game.savePitcher.get.id == "573124")
  }

  test("Correct team runs") {
    assert(game.visitingTeamRuns == 4 && game.homeTeamRuns == 7)
  }

  test("Correct pitcher FPTS") {
    val p1 = game.visitingTeamStartingPitcher // Jesse Chavez
    val p2 = game.homeTeamStartingPitcher // Michael Pineda
    assert(game.statsFor(p1).get.fantasyPoints(FanDuelMLB) == 12 &&
      game.statsFor(p2).get.fantasyPoints(FanDuelMLB) == 52)
  }

  test("Correct hitter FPTS") {
    val p1 = Players.get("593934") // Miguel Sano
    val p2 = Players.get("425783") // Shin-Soo Choo
    val p3 = Players.get("664774") // LaMonte Wade
    val p4 = Players.get("570731") // Jonathan Schoop
    val p5 = Players.get("503556") // Marwin Gonzalez
    assert(game.statsFor(p1).get.fantasyPoints(FanDuelMLB) == 18.4f &&
      game.statsFor(p2).get.fantasyPoints(FanDuelMLB) == 12f &&
      game.statsFor(p3).get.fantasyPoints(FanDuelMLB) == 6.2f &&
      game.statsFor(p4).get.fantasyPoints(FanDuelMLB) == 18.9f &&
      game.statsFor(p5).get.fantasyPoints(FanDuelMLB) == 18.7f)
  }

  test("Correct hitter FPTS with runner scoring but no RBI") {
    val p1 = Players.get("592192") // Mark Canha
    //println(game2.statsFor(p1).get.printStats)
    assert(game2.statsFor(p1).get.fantasyPoints(FanDuelMLB) == 3f)
  }
  
  test("Correct hitter FPTS with runner scoring on wild pitch") {
    val p1 = Players.get("518960") // Jonathan Lucroy
    println(game4.statsFor(p1).get.printStats)
    assert(game4.statsFor(p1).get.fantasyPoints(FanDuelMLB) == 21.7f)
  }

  test("Correct hitter FPTS with stolen base") {
    val p1 = Players.get("621439") // Byron Buxton
    val p2 = Players.get("596451") // Roman Quinn
    val p3 = Players.get("516416") // Jean Segura
    println(game2.statsFor(p1).get.printStats)
    println(game3.statsFor(p2).get.printStats)
    println(game3.statsFor(p3).get.printStats)
    assert(game2.statsFor(p1).get.fantasyPoints(FanDuelMLB) == 12f &&
      game3.statsFor(p2).get.fantasyPoints(FanDuelMLB) == 18.2f &&
      game3.statsFor(p3).get.fantasyPoints(FanDuelMLB) == 9f)
  }
  
  test("Correct hitter FPTS with grand slam") {
    val p1 = Players.get("493329") // Yuli Gurriel
    println(game4.statsFor(p1).get.printStats)
    assert(game4.statsFor(p1).get.fantasyPoints(FanDuelMLB) == 44.6f)
  }

}