package test

import org.scalatest.FunSuite
import rotogrinders._

class LineupTests extends FunSuite {

  val p1 = Player(1, "RB 1", 8000, "DAL", "RB", "NYJ", Some(6.0f), Some(10.0f), 8.0f)
  val p2 = Player(2, "RB/WR 2", 6000, "NYG", "RB/WR", "MIA", Some(5.0f), Some(9.0f), 7.0f)
  val p3 = Player(3, "TE 3", 5500, "NYG", "TE", "MIA", Some(4.0f), Some(9.5f), 7.2f)
  val p4 = Player(4, "RB 4", 3500, "MIA", "RB", "NYG", Some(4.0f), Some(9.5f), 7.2f)
  val p5 = Player(5, "RB 5", 3500, "MIA", "RB", "NYG", Some(4.0f), Some(9.5f), 7.2f)
  val p6 = Player(6, "WR 6", 50000, "NYJ", "RB", "DAL", Some(4.0f), Some(9.5f), 7.2f)
  val p7 = Player(7, "RB 7", 5500, "NYG", "TE", "MIA", Some(4.0f), Some(9.5f), 7.2f)
  val rbSlot = LineupSlot(1, "RB", List("RB"))
  val rbSlot2 = LineupSlot(2, "RB", List("RB"))
  val wrSlot = LineupSlot(3, "WR", List("WR"))
  val flexSlot = LineupSlot(4, "FLEX", List("RB", "WR", "TE"))
  val contest = ContestType("FD", "NFL", 60000, 2, 2, 1, List(rbSlot, rbSlot2, wrSlot, flexSlot), 0, 0)
  val lineup1 = Lineup(1, contest, Map(rbSlot -> p1, rbSlot2 -> p2))
  val lineup2 = Lineup(2, contest, Map(rbSlot -> p1, rbSlot2 -> p2, flexSlot -> p4))

  test("Lineup is valid") {
    val validLineup = Lineup(1, contest, Map(rbSlot -> p1, rbSlot2 -> p4, wrSlot -> p2, flexSlot -> p3))
    assert(validLineup.isValid)
  }
  
  test("Lineup is not valid") {
    val duplicate = Lineup(1, contest, Map(rbSlot -> p1, rbSlot2 -> p1, wrSlot -> p2, flexSlot -> p3))
    val tooExpensive = Lineup(2, contest, Map(rbSlot -> p1, rbSlot2 -> p4, wrSlot -> p6, flexSlot -> p3))
    val wrongPosition = Lineup(3, contest, Map(rbSlot -> p1, rbSlot2 -> p4, wrSlot -> p3, flexSlot -> p3))
    val tooManyPlayersPerTeam = Lineup(4, contest, Map(rbSlot -> p7, rbSlot2 -> p4, wrSlot -> p2, flexSlot -> p3))
    val notEnoughGames = Lineup(5, contest, Map(rbSlot -> p5, rbSlot2 -> p4, wrSlot -> p2, flexSlot -> p3))
    assert(!lineup1.isValid && !duplicate.isValid && !tooExpensive.isValid && !wrongPosition.isValid 
        && !tooManyPlayersPerTeam.isValid && !notEnoughGames.isValid)
  }
  
  test("Lineup has correct openSlots") {
    assert(lineup1.openSlots.size == 2 && lineup1.openSlots.contains(wrSlot) && lineup1.openSlots.contains(flexSlot) &&
        lineup2.openSlots.size == 1 && lineup2.openSlots.contains(wrSlot))
  }
  
  test("Lineup has correct remainingSalary") {
    assert(lineup1.remainingSalary == 46000 && lineup2.remainingSalary == 42500)
  }
  
  test("Player can be added to lineup") {
    assert(p3.canBeAddedTo(lineup1) && p4.canBeAddedTo(lineup1) && !p1.canBeAddedTo(lineup1) && !p3.canBeAddedTo(lineup2))
  }
  
  test("Player correctly added to lineup with open slot") {
    assert(lineup1.add(p4).identicalTo(lineup2))
  }
  
  test("Player correctly added to lineup with player bumped") {
    val updated = lineup2.add(p5)
    //println(s"BEFORE: \n${lineup2.filledSlots.toList.sortBy(_._1.id).mkString("\n")}\nAFTER: \n${updated.filledSlots.toList.sortBy(_._1.id).mkString("\n")}")
    assert(updated.contains(p1) && updated.contains(p2) && updated.contains(p4) && updated.contains(p5))
  }

}