package test

import org.scalatest.FunSuite
import mlb.retrosheet.Bases._

class BasesTests extends FunSuite {

  test("Advance lengths are correct for safe advances") {
    assert(lengthOf("B-1") == 1 && lengthOf("B-2") == 2 && lengthOf("B-3") == 3 && lengthOf("B-H") == 4 &&
      lengthOf("1-2") == 1 && lengthOf("1-3") == 2 && lengthOf("1-H") == 3 &&
      lengthOf("2-3") == 1 && lengthOf("2-H") == 2 && lengthOf("3-H") == 1)
  }

  test("Advance lengths are correct for outs") {
    assert(lengthOf("BX1") == 1 && lengthOf("BX2") == 2 && lengthOf("BX3") == 3 && lengthOf("BXH") == 4 &&
      lengthOf("1X2") == 1 && lengthOf("1X3") == 2 && lengthOf("1XH") == 3 &&
      lengthOf("2X3") == 1 && lengthOf("2XH") == 2 && lengthOf("3XH") == 1)
  }

  test("Merge works") {
    val adv1 = List("2-3", "1-2")
    val adv2 = List("2-H", "1-3", "3XH")
    assert(merge(adv1, adv2) == List("3XH", "2-H", "1-3"))
  }

  test("Merge works with empty 1st or 2nd list") {
    val adv1 = List("2-3", "1-2")
    val adv2 = List("3XH", "2-H", "1-3")
    assert(merge(adv1, Nil) == adv1 && merge(Nil, adv2) == adv2)
  }

  test("Merge works with equal length advances") {
    val adv1 = List("2-3", "1-2")
    val adv2 = List("2X3", "1-3")
    assert(merge(adv1, adv2) == List("2X3", "1-3"))
  }

  test("baseNumberOf works") {
    assert(baseNumberOf(BATTER) == 0 && baseNumberOf(FIRST_BASE) == 1 && baseNumberOf(SECOND_BASE) == 2 &&
      baseNumberOf(THIRD_BASE) == 3 && baseNumberOf(HOME_BASE) == 4)
  }

  test("baseWithNumber works") {
    assert(baseWithNumber(0) == BATTER && baseWithNumber(1) == FIRST_BASE && baseWithNumber(2) == SECOND_BASE &&
      baseWithNumber(3) == THIRD_BASE && baseWithNumber(4) == HOME_BASE)
  }

}