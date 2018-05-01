package mlb

import com.typesafe.config.{ ConfigFactory, Config }
import scala.collection.JavaConversions._
import model._
import utils.Logger._
import utils.StringUtils._
import java.util.Date

object Configs {
  private val conf = ConfigFactory.load.getConfig("mlb")

  object Retrosheet {
    private val conf = Configs.conf.getConfig("retrosheet")

    val dataFileDir_2017: String = conf.getString("data_file_dir_2017")

    val teamsFileName: String = conf.getString("teams_file")
  }
  
  object MlbDotCom {
    private val conf = Configs.conf.getConfig("mlbdotcom")
    
    val baseURL: String = conf.getString("base_url")
    
    val seasonStartDate: Date = conf.getString("season_start_date").toDate("yyyy-MM-dd")
    val lastSeasonEndDate: Date = conf.getString("last_season_end_date").toDate("yyyy-MM-dd")
    
    val dataFileDir: String = conf.getString("data_file_dir")
    
    val runSanityChecks: Boolean = conf.getBoolean("run_sanity_checks")
    if (runSanityChecks) log("#### Running MLB.com sanity checks ####")
  }
  
  object Rotogrinders {
    private val conf = Configs.conf.getConfig("rotogrinders")
    
    val projectedStartersFile: String = conf.getString("projected_starters_file")
    
    val playerMappingsFile: String = conf.getString("player_mappings_file")
  }
  
  val blogFormat: String = conf.getString("blog_format")
  
  val dfsSalaryFileDir: String = conf.getString("dfs_salary_file_dir")

  val dfsScoringSystem: DFSScoringSystem = conf.getString("dfs_scoring_system") match {
    case "FanDuel MLB"    => FanDuelMLB
    case "DraftKings MLB" => DraftKingsMLB
    case _                => throw new Exception("Invalid \"dfs_scoring_system\" value in application.conf!")
  }
  log(s"Using $dfsScoringSystem scoring system")
  
  val teamMappingsFile: String = conf.getString("team_mappings_file")
  
  val playerMappingsFile: String = conf.getString("player_mappings_file")
  
  val newPlayersFile: String = conf.getString("new_players_file")
  
  val teamsNotPlayingFile: String = conf.getString("teams_not_playing_file")

}