package rotogrinders

import com.typesafe.config.{ ConfigFactory, Config }
import scala.collection.JavaConversions._

object Configs {
  private val conf = ConfigFactory.load.getConfig("rotogrinders")

  object LineupBuilder {
    private val conf = Configs.conf.getConfig("lineup_builder")

    val dataFileDir: String = conf.getString("data_file_dir")

    val minPlayersDifferentPerLineup: Int = conf.getInt("min_players_different_per_lineup")

    val maxExposurePercentage: Int = conf.getInt("max_exposure_percentage")

    val contestTypes: List[ContestType] = conf.getConfigList("contest_types").toList.map { cfg =>

      case class FlexSlot(slot: String, positions: List[String])

      val flexSlots = cfg.hasPath("flex_slots") match {
        case true => cfg.getConfigList("flex_slots").map { fs =>
          FlexSlot(fs.getString("slot"), fs.getString("accepted_positions").split(",").toList.map(_.trim))
        }
        case false => Nil
      }

      val slots = cfg.getString("slots").split(",").toList.map(_.trim).zipWithIndex.map {
        case (slot, id) =>
          flexSlots.find(_.slot == slot) match {
            case Some(flexSlot) => LineupSlot(id, slot, flexSlot.positions)
            case None           => LineupSlot(id, slot, List(slot)) // slot name is a single player position
          }
      }

      val minPlayerValue = cfg.hasPath("min_player_value") match {
        case true  => cfg.getDouble("min_player_value")
        case false => 0d
      }
      
      val minPlayerFPTS = cfg.hasPath("min_player_fpts") match {
        case true  => cfg.getDouble("min_player_fpts")
        case false => 0d
      }

      ContestType(cfg.getString("site").trim.toUpperCase, cfg.getString("sport").trim.toUpperCase, cfg.getInt("max_salary"), cfg.getInt("max_players_per_team"),
        cfg.getInt("players_from_min_games"), cfg.getInt("players_from_min_teams"), slots, minPlayerValue, minPlayerFPTS)
    }

  }

}