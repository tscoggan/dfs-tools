package rotogrinders

case class ContestType(site: String, sport: String, maxSalary: Int, maxPlayersPerTeam: Int, minNumberOfGames: Int, minNumberOfTeams: Int, slots: List[LineupSlot])

case class LineupSlot(id: Int, name: String, positionsAccepted: List[String]) {

  def acceptsPosition(position: String): Boolean = positionsAccepted.contains(position)

  def samePositionAs(player: Player): Boolean = player.positions.exists(this.acceptsPosition(_))

  override def toString: String = name

}