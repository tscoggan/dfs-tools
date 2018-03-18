package test

import org.scalatest.FunSuite
import utils._

class TopNArrayTests extends FunSuite {

  test("Can add elements") {
    val integers = new TopNArray[Int](5)
    List(1, 10, 2, 9, 3, 8, 4, 7, 5, 6).foreach { integers.add(_) }
    assert(integers.array.toSet == Set(6,7,8,9,10) && integers.minElement == 6)
  }

}