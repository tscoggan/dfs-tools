package utils

import com.typesafe.config.{ ConfigFactory, Config }
import scala.collection.JavaConversions._

object Configs {
  private val conf = ConfigFactory.load.getConfig("utils")

  val logDebug: Boolean = conf.getBoolean("log_debug")

}