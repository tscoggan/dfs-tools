package mlb

import com.typesafe.config.{ ConfigFactory, Config }
import scala.collection.JavaConversions._
import model._
import utils.Logger._
import utils.StringUtils._
import utils.DoubleUtils._
import utils.DateTimeUtils._
import java.util.Date

object Configs {
  private val conf = ConfigFactory.load.getConfig("mlb")

  object Retrosheet {
    private val conf = Configs.conf.getConfig("retrosheet")

    val teamsFileName: String = conf.getString("teams_file")
  }

  object MlbDotCom {
    private val conf = Configs.conf.getConfig("mlbdotcom")

    val baseURL: String = conf.getString("base_url")

    val seasonStartDate: Date = conf.getString("season_start_date").toDate("yyyy-MM-dd")
    val lastSeasonStartDate: Date = conf.getString("last_season_start_date").toDate("yyyy-MM-dd")
    val lastSeasonEndDate: Date = conf.getString("last_season_end_date").toDate("yyyy-MM-dd")
    val previousSeasonStartDate: Date = conf.getString("previous_season_start_date").toDate("yyyy-MM-dd")
    val previousSeasonEndDate: Date = conf.getString("previous_season_end_date").toDate("yyyy-MM-dd")

    val dataFileDir: String = conf.getString("data_file_dir")

    val runSanityChecks: Boolean = conf.getBoolean("run_sanity_checks")
    if (runSanityChecks) log("#### Running MLB.com sanity checks ####")

    val ignoreSanityChecksForDates: List[Date] = conf.getStringList("ignore_sanity_checks_for_dates").map(_.toDate("yyyy-MM-dd").trimTime).toList
  }

  object Rotogrinders {
    private val conf = Configs.conf.getConfig("rotogrinders")

    val projectedStartersFile: String = conf.getString("projected_starters_file")

    val playerMappingsFile: String = conf.getString("player_mappings_file")
  }

  val blogFormat: String = conf.getString("blog_format")

  val stackSize: Int = conf.getInt("stack_size")
  val numberOfStacks: Int = conf.getInt("number_of_stacks")

  val stackValueScoreFptsMultiplier: Double = conf.getDouble("stack_value_score_fpts_multiplier")

  val overweightRecent: Boolean = conf.getBoolean("overweight_recent_performance")
  val recentDaysToWeightHigher: Int = conf.getInt("recent_number_of_days_to_weight_higher")
  val higherWeight: Double = conf.getString("higher_weight").trim.trimSuffix("x").toDouble
  val recentDaysToWeightHighest: Int = conf.getInt("recent_number_of_days_to_weight_highest")
  val highestWeight: Double = conf.getString("highest_weight").trim.trimSuffix("x").toDouble

  require(recentDaysToWeightHighest < recentDaysToWeightHigher)
  log(s"overweightRecent: $overweightRecent --- ${highestWeight.rounded(1)}x for past ${recentDaysToWeightHighest} days, " +
    s"${higherWeight.rounded(1)}x for past ${recentDaysToWeightHigher} days")

  val dfsSalaryFileDir: String = conf.getString("dfs_salary_file_dir")

  val projectionsHistoryDir: String = conf.getString("projections_history_dir")

  val dfsScoringSystem: DFSScoringSystem = conf.getString("dfs_scoring_system") match {
    case "FanDuel MLB"    => FanDuelMLB
    case "DraftKings MLB" => DraftKingsMLB
    case _                => throw new Exception("Invalid \"dfs_scoring_system\" value in application.conf!")
  }
  log(s"Using $dfsScoringSystem scoring system")

  val teamMappingsFile: String = conf.getString("team_mappings_file")

  val playerMappingsFile: String = conf.getString("player_mappings_file")

  val teamsNotPlayingFile: String = conf.getString("teams_not_playing_file")

}