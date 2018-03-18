package rotogrinders

import scala.collection.mutable
import utils.Logger._

case class LineupFilters(lineups: mutable.ArrayBuffer[Lineup]) {

  def differBy(minPlayersDifferentPerLineup: Int): LineupFilters = {
    if (minPlayersDifferentPerLineup > 1) {
      log(s"Choosing lineups that differ by at least $minPlayersDifferentPerLineup players...")
      val newLineups: mutable.ArrayBuffer[Lineup] = new mutable.ArrayBuffer[Lineup]()
      lineups.sortBy(_.projectedPoints).reverse.foreach { lineup =>
        if (newLineups.forall { unique => lineup.diff(unique).size >= minPlayersDifferentPerLineup }) {
          newLineups += lineup
        }
      }
      log(s"Produced ${newLineups.length} lineups")
      LineupFilters(newLineups)
    } else LineupFilters(lineups)
  }
  
  def maxExposure(maxExposurePercentage: Int, targetLineupCount: Int): LineupFilters = {
    println(s"maxExposure($maxExposurePercentage, $targetLineupCount)")
    if (maxExposurePercentage > 0 && maxExposurePercentage < 100) {
      log(s"Ensuring no player has > ${maxExposurePercentage}% exposure...")
      val newLineups: mutable.ArrayBuffer[Lineup] = new mutable.ArrayBuffer[Lineup]()
      val tracker = ExposureTracker(maxExposurePercentage, targetLineupCount)
      lineups.sortBy(_.projectedPoints).reverse.foreach { lineup =>
        if (tracker.lineupCanBeAdded(lineup)) {
          newLineups += lineup
          tracker.add(lineup)
        }
      }
      log(s"Produced ${newLineups.length} lineups")
      LineupFilters(newLineups)
    } else LineupFilters(lineups)
  }

}