package test

import org.scalatest.FunSuite
import rotogrinders._

class PlayerTests extends FunSuite {

  val p1 = Player(1, "Player 1", 8000, "DAL", "RB", "NYJ", Some(6.0f), Some(10.0f), 8.0f)
  val p2 = Player(2, "Player 2", 6000, "NYG", "RB/WR", "MIA", Some(5.0f), Some(9.0f), 7.0f)
  val rbSlot = LineupSlot(1, "RB", List("RB"))
  val wrSlot = LineupSlot(2, "WR", List("WR"))
  val flexSlot = LineupSlot(3, "FLEX", List("RB", "WR", "TE"))

  test("Player has correct ptsPerSalary") {
    assert(p1.projValue == 1.0f)
  }

  test("Player has correct positions") {
    assert(p1.positions.size == 1 && p1.positions.contains("RB") &&
      p2.positions.size == 2 && p2.positions.contains("RB") && p2.positions.contains("WR"))
  }

  test("Player can fill correct slots") {
    assert(p1.canFill(rbSlot) && p1.canFill(flexSlot) && !p1.canFill(wrSlot) &&
      p2.canFill(rbSlot) && p2.canFill(flexSlot) && p2.canFill(wrSlot))
  }

}