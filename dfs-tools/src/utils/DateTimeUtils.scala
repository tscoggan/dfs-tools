package utils

import java.util.{ Date, Calendar }
import java.text.SimpleDateFormat
import scala.collection.mutable
import Logger._

object DateTimeUtils {

  def timeInSeconds[T](code: => T): (T, Int) = {
    val t1 = System.currentTimeMillis
    val result = code
    val elapsed = ((System.currentTimeMillis - t1) / 1000).toInt
    (result, elapsed)
  }

  private val dateFormatCache: mutable.Map[String, SimpleDateFormat] = mutable.Map.empty

  def getDateFormat(format: String): SimpleDateFormat = dateFormatCache.get(format) match {
    case Some(cached) => cached
    case None => {
      log(s"Date format $format not found in cache --> adding")
      val df = new SimpleDateFormat(format)
      dateFormatCache += (format -> df)
      df
    }
  }

  implicit class EnrichedDate(d: Date) {

    def print(format: String = "yyyy-MM-dd"): String = getDateFormat(format).format(d)

  }

}