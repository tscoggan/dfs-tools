package test

import org.scalatest.FunSuite
import utils.FloatUtils._

class FloatUtilsTests extends FunSuite {
  
  test("~= works for Float") {
    assert((1.456f ~= 1.45600f) && !(5.67f ~= 5.671f))
  }
  
  test("~= works for Double") {
    assert((1.456f ~= 1.45600d) && !(5.67f ~= 5.671d))
  }
  
  test("~= works for Int") {
    assert((1.000f ~= 1) && !(1.001f ~= 1))
  }

}