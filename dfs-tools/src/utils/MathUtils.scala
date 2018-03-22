package utils

object MathUtils {

  import Numeric.Implicits._

  def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

  def variance[T: Numeric](xs: Iterable[T]): Double = {
    val avg = mean(xs)

    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  def stdDev[T: Numeric](xs: Iterable[T]): Double = math.sqrt(variance(xs))

  /**
   * A "downside deviation" measure that is similar to standard deviation but only measures variance below a specified
   * minimum threshold. See https://www.managedfuturesinvesting.com/a-better-measure-of-risk-standard-deviation-or-downside-deviation/
   */
  def downsideDev[T: Numeric](xs: Iterable[T], minAcceptableValue: T): Double = {
    val xsAdjusted = xs.map { value => scala.math.min(value.toDouble - minAcceptableValue.toDouble, 0) }
    math.sqrt(xsAdjusted.map(math.pow(_, 2)).sum / xsAdjusted.size)
  }
  
  /**
   * A "upside deviation" measure that is similar to standard deviation but only measures variance above a specified
   * benchmark. See https://www.managedfuturesinvesting.com/a-better-measure-of-risk-standard-deviation-or-downside-deviation/
   */
  def upsideDev[T: Numeric](xs: Iterable[T], benchmark: T): Double = {
    val xsAdjusted = xs.map { value => scala.math.max(value.toDouble - benchmark.toDouble, 0) }
    math.sqrt(xsAdjusted.map(math.pow(_, 2)).sum / xsAdjusted.size)
  }

  /**
   * Returns % of elements in the specified list for which the `isTrue` function is true.
   */
  def percent[T](xs: Iterable[T], isTrue: T => Boolean): Double = {
    var total = 0
    var matchCount = 0
    xs.foreach { x =>
      total += 1
      if (isTrue(x)) matchCount += 1
    }
    (matchCount.toDouble / total.toDouble) * 100d
  }

}