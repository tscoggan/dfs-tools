package mlb

import com.typesafe.config.{ ConfigFactory, Config }
import scala.collection.JavaConversions._
import model._
import utils.Logger._

object Configs {
  private val conf = ConfigFactory.load.getConfig("mlb")

  val dataFileDir: String = conf.getString("data_file_dir")
  
  val teamsFileName: String = conf.getString("teams_file")
  
  val dfsScoringSystem: DFSScoringSystem = conf.getString("dfs_scoring_system") match{
    case "FanDuel MLB" => FanDuelMLB
    case "DraftKings MLB" => DraftKingsMLB
    case _ => throw new Exception("Invalid \"dfs_scoring_system\" value in application.conf!")
  }
  log(s"Using $dfsScoringSystem scoring system")

}