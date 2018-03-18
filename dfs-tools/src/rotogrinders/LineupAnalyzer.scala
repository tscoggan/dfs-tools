package rotogrinders

object LineupAnalyzer {

  type ExposurePercentage = Float

  def playerExposures(lineups: List[Lineup]): Map[Player, ExposurePercentage] = {
    lineups.flatMap(_.filledSlots.values).groupBy(p => p).map {
      case (player, instances) => (player, (instances.length.toFloat / lineups.length.toFloat) * 100f)
    }
  }

  def playerExposuresString(lineups: List[Lineup]): String = {
    s"Player exposures:\n\t${
      playerExposures(lineups).toList.sortBy(_._2).reverse.map {
        case (player, exposure) => s"$player --- " + f"$exposure%.1f" + "%"
      }.mkString("\n\t")
    }"
  }

}