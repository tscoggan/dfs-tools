package mlb.model

case class PitcherStatsAgainst(
  pitcher: Player,
  atBatsAgainst: Int,
  fptsAgainst_FD: Double,
  fptsPerAtBatAgainst_FD: Double,
  fptsAgainst_DK: Double,
  fptsPerAtBatAgainst_DK: Double,
  batterHandedness: Option[Handedness])