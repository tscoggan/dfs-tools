package test

import org.scalatest.FunSuite
import utils._
import utils.FloatUtils._
import utils.DoubleUtils._

class MathUtilsTests extends FunSuite {

  test("Standard deviation works") {
    val integers = List(600, 470, 170, 430, 300)
    println(MathUtils.stdDev(integers))
    assert(MathUtils.stdDev(integers).toFloat ~= 147.32277)
  }

  test("Percent works") {
    val integers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    assert(MathUtils.percent(integers, (i: Int) => i > 3) == 70.0)
  }

  test("Downside deviation works") {
    val i = List(10, 12, 13, 10, 11, 12, 19, 10, 10, 10, 11, 12, 20, 10, 10, 10, 10, 10, 10)
    val j = List(10, 5, 25, 8, 21, 7, 31, 10, 10, 10, 11, 12, 20, 8, 10, 10, 10, 10, 10)
    val k = List(10, 0, 12, 0, 11, 0, 11, 10, 10, 5, 11, 12, 8, 10, 10, 1, 10, 3, 10)
    val example = List(0.1, -0.02, 0.03, 0.05, 0.02, -0.04)
    //println(MathUtils.downsideDev(example, 0.01))
    //println(MathUtils.stdDev(i)+" "+MathUtils.downsideDev(i, 10))
    //println(MathUtils.stdDev(j)+" "+MathUtils.downsideDev(j, 10))
    //println(MathUtils.stdDev(k)+" "+MathUtils.downsideDev(k, 10))
    assert(MathUtils.downsideDev(i, 10) < MathUtils.downsideDev(j, 10) && MathUtils.downsideDev(j, 10) < MathUtils.downsideDev(k, 10)
      && (MathUtils.downsideDev(example, 0.01) ~= (0.0238, 0.0001)))
  }

  test("Pearson correlation works") {
    val data = Map(
      ("1", (43, 99)),
      ("2", (21, 65)),
      ("3", (25, 79)),
      ("4", (42, 75)),
      ("5", (57, 87)),
      ("6", (59, 81)))
    val ds1 = data.map{case (k,(v1,v2)) => (k,v1.toDouble)}
    val ds2 = data.map{case (k,(v1,v2)) => (k,v2.toDouble)}
    val correlation = MathUtils.pearsonCorrelation(ds1,ds2).get
    println("Pearson correlation: "+correlation)
    assert(correlation ~= 0.529809)
  }

}