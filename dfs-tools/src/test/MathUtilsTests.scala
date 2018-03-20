package test

import org.scalatest.FunSuite
import utils._
import utils.FloatUtils._

class MathUtilsTests extends FunSuite {

  test("Standard deviation works") {
    val integers = List(600, 470, 170, 430, 300)
    println(MathUtils.stdDev(integers))
    assert(MathUtils.stdDev(integers).toFloat ~= 147.32277)
  }

}