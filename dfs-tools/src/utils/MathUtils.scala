package utils

object MathUtils {

  import Numeric.Implicits._

  def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

  def median[T: Numeric](xs: Iterable[T]): Double = {
    val (lower, upper) = xs.toList.sorted.splitAt(xs.size / 2)
    if (xs.size % 2 == 0) (lower.last.toDouble + upper.head.toDouble) / 2.0 else upper.head.toDouble
  }

  type Weight = Int

  def weightedAvg[T: Numeric](xs: (T, Weight)*): Double = {
    val values = xs.flatMap { case (value, weight) => (0 until weight).toList.map(i => value) }
    mean(values)
  }

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
    if (xs.isEmpty) 0.0 else {
      val xsAdjusted = xs.map { value => scala.math.min(value.toDouble - minAcceptableValue.toDouble, 0) }
      math.sqrt(xsAdjusted.map(math.pow(_, 2)).sum / xsAdjusted.size)
    }
  }

  /**
   * A "upside deviation" measure that is similar to standard deviation but only measures variance above a specified
   * benchmark. See https://www.managedfuturesinvesting.com/a-better-measure-of-risk-standard-deviation-or-downside-deviation/
   */
  def upsideDev[T: Numeric](xs: Iterable[T], benchmark: T): Double = {
    if (xs.isEmpty) 0.0 else {
      val xsAdjusted = xs.map { value => scala.math.max(value.toDouble - benchmark.toDouble, 0) }
      math.sqrt(xsAdjusted.map(math.pow(_, 2)).sum / xsAdjusted.size)
    }
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

  /**
   * The Pearson correlation coefficient can range from +1 to -1. A value of 0 indicates that there is no association between the two variables.
   * A value greater than 0 indicates a positive association; that is, as the value of one variable increases, so does the value of the other variable.
   *
   * In the data set maps, key = ID and value = data value that should correlate.
   */
  def pearsonCorrelation(
    dataSet1: Map[String, Double],
    dataSet2: Map[String, Double]): Option[Double] = {

    def commonMapKeys[A, B](a: Map[A, B], b: Map[A, B]): Set[A] = a.keySet.intersect(b.keySet)

    val commonDataElements = commonMapKeys(dataSet1, dataSet2).toSeq
    val n = commonDataElements.size
    if (n == 0) return None

    val commonElementsFromDS1 = dataSet1.filterKeys(e => commonDataElements.contains(e))
    val commonElementsFromDS2 = dataSet2.filterKeys(e => commonDataElements.contains(e))

    val sum1 = commonElementsFromDS1.values.sum
    val sum2 = commonElementsFromDS2.values.sum

    val sum1Sq = commonElementsFromDS1.values.foldLeft(0.0)(_ + Math.pow(_, 2))
    val sum2Sq = commonElementsFromDS2.values.foldLeft(0.0)(_ + Math.pow(_, 2))

    val pSum = commonDataElements.foldLeft(0.0)((accum, element) => accum + commonElementsFromDS1(element) * commonElementsFromDS2(element))

    // calculate the pearson score
    val numerator = pSum - (sum1 * sum2 / n)
    val denominator = Math.sqrt((sum1Sq - Math.pow(sum1, 2) / n) * (sum2Sq - Math.pow(sum2, 2) / n))
    if (denominator == 0) None else Some(numerator / denominator)
  }

}