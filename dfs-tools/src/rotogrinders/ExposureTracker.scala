package rotogrinders

import scala.collection.mutable

case class ExposureTracker(maxPlayerExposurePercentage: Int, targetLineupCount: Int) {

  private var numberOfLineups = 0 // total number of lineups built

  private val playerLineupCounts: mutable.Map[Player, Int] = mutable.Map.empty

  def add(lineup: Lineup): Unit = playerLineupCounts.synchronized {
    lineup.filledSlots.foreach {
      case (slot, player) =>
        playerLineupCounts.get(player) match {
          case Some(count) => playerLineupCounts.update(player, count + 1)
          case None        => playerLineupCounts += (player -> 1)
        }
    }
    numberOfLineups = numberOfLineups + 1
  }

  def lineupCanBeAdded(lineup: Lineup): Boolean = lineup.filledSlots.forall {
    case (slot, player) => (getLineupCount(player) + 1).toFloat / targetLineupCount.toFloat <= (maxPlayerExposurePercentage.toFloat / 100f)
  }

  def getLineupCount(player: Player): Int = playerLineupCounts.getOrElse(player, 0)

  def getExposurePercentage(player: Player): Float = getLineupCount(player) / numberOfLineups

}