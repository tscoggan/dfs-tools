package test

import org.scalatest.FunSuite
import utils.DoubleUtils._

class DoubleUtilsTests extends FunSuite {

  test("~= works for Float") {
    assert((1.456 ~= 1.45600f) && !(5.67 ~= 5.671f))
  }

  test("~= works for Double") {
    assert((1.456 ~= 1.45600d) && !(5.67 ~= 5.671d))
  }

  test("~= works for Int") {
    assert((1.000 ~= 1) && !(1.001 ~= 1))
  }

  test("rounded works") {
    val d = 12345.78901
    val d2 = -12345.78941
    assert(d.rounded(2) == "12345.79" && d2.rounded(3) == "-12345.789")
  }

}