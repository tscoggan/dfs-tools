package test

import org.scalatest.FunSuite
import utils.StringUtils._
import utils.DateTimeUtils._

class DateTimeUtilsTests extends FunSuite {

  val d1 = "2017-11-06".toDate()
  val d2early = "2017-11-07 03:00:00".toDate("yyyy-MM-dd HH:mm:ss")
  val d2late = "2017-11-07 04:00:00".toDate("yyyy-MM-dd HH:mm:ss")
  val d3 = "2017-11-08".toDate()
  val d7 = "2017-11-12".toDate()

  test("plusDays works") {
    assert("2017-11-05".toDate().plusDays(3) == "2017-11-08".toDate() &&
      "2018-03-10".toDate().plusDays(7) == "2018-03-17".toDate())
  }

  test("minusDays works") {
    assert("2017-11-08".toDate().minusDays(3) == "2017-11-05".toDate() &&
      "2018-03-17".toDate().minusDays(7) == "2018-03-10".toDate())
  }

  test("nextDay works") {
    assert("2017-11-05".toDate().nextDay == "2017-11-06".toDate() &&
      "2018-03-11".toDate().nextDay == "2018-03-12".toDate())
  }

  test("previousDay works") {
    assert("2017-11-06".toDate().previousDay == "2017-11-05".toDate() &&
      "2018-03-12".toDate().previousDay == "2018-03-11".toDate())
  }

  test("isBefore works") {
    assert(d1.isBefore(d2early) && d1.isBefore(d3) && !d3.isBefore(d1) && !d1.isBefore(d1))
  }

  test("isAfter works") {
    assert(!d1.isAfter(d2early) && !d1.isAfter(d3) && d3.isAfter(d1) && !d1.isAfter(d1))
  }

  test("isSameDayAs works") {
    assert(!d1.isSameDayAs(d2early) && !d1.isSameDayAs(d3) && d3.isSameDayAs(d3) && d2early.isSameDayAs(d2late))
  }

  test("getDatesBetween works") {
    val datesInclusive = List("2017-11-06".toDate(), "2017-11-07".toDate(), "2017-11-08".toDate(), "2017-11-09".toDate(), "2017-11-10".toDate(),
      "2017-11-11".toDate(), "2017-11-12".toDate())
    val datesExclusive = List("2017-11-07".toDate(), "2017-11-08".toDate(), "2017-11-09".toDate(), "2017-11-10".toDate(), "2017-11-11".toDate())
    assert(getDatesBetween(d1, d7) == datesInclusive && getDatesBetween(d1, d7, false) == datesExclusive)
  }
  
  test("year works") {
    assert(d1.year == 2017)
  }

}