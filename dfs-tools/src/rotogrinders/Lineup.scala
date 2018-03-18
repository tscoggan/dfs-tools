package rotogrinders

case class Lineup(id: Int, contest: ContestType, filledSlots: Map[LineupSlot, Player] = Map.empty) extends Ordered[Lineup] {

  lazy val openSlots: Set[LineupSlot] = contest.slots.toSet.diff(filledSlots.keySet)

  lazy val players: Set[Player] = filledSlots.values.toSet

  lazy val projectedPoints: Float = players.map(_.projPoints).sum
  
  lazy val salary: Int = players.toList.map(_.salary).sum

  lazy val remainingSalary: Int = contest.maxSalary - salary

  lazy val maxPlayersPerTeam: Int = players.groupBy(_.team).values.map(_.size).max

  lazy val numberOfGames: Int = players.map(p => List(p.team, p.opponent).sorted).size
  
  lazy val numberOfTeams: Int = players.map(_.team).size

  lazy val isFull: Boolean = openSlots.isEmpty

  lazy val isValid: Boolean = isFull && // no empty slots
    (remainingSalary >= 0) && // salary within cap
    (players.size == filledSlots.size) && // no duplicate players
    filledSlots.forall { case (slot, player) => player.canFill(slot) } && // players have correct positions
    maxPlayersPerTeam <= contest.maxPlayersPerTeam && // not too many players on same team
    numberOfGames >= contest.minNumberOfGames && // players from minimum # of games
    numberOfTeams >= contest.minNumberOfTeams // players from minimum # of teams

  def contains(player: Player): Boolean = players.contains(player)

  def identicalTo(other: Lineup): Boolean = {
    (this.contest == other.contest) && (this.players == other.players)
  }

  def diff(other: Lineup): Set[(LineupSlot, Player)] = this.filledSlots.toSet.diff(other.filledSlots.toSet)

  def canAdd(player: Player): Boolean = {
    (player.salary <= remainingSalary) && !players.contains(player) &&
      (openSlots.exists(_.samePositionAs(player)) // this player fits into an open slot...
        // ...or an existing player can move to a different slot to make room:
        || filledSlots.exists {
          case (slot, otherPlayer) =>
            slot.samePositionAs(player) && openSlots.exists(_.samePositionAs(otherPlayer))
        })
  }

  def add(player: Player): Lineup = {
    if (canAdd(player)) {
      openSlots.find(_.samePositionAs(player)) match {
        case Some(slot) => this.copy(filledSlots = filledSlots + (slot -> player))
        case None => {
          // bump an existing player to a different slot...
          filledSlots.find {
            case (slot, otherPlayer) => slot.samePositionAs(player) && openSlots.exists(_.samePositionAs(otherPlayer))
          } match {
            case Some((slot, otherPlayer)) => this.copy(filledSlots = (filledSlots - slot) + (slot -> player)).add(otherPlayer)
            case None                      => throw new Exception(s"Player ${player.id} cannot be added to lineup $id")
          }
        }
      }
    } else throw new Exception(s"Player ${player.id} cannot be added to lineup $id")
  }

  def compare(that: Lineup) = ((this.projectedPoints - that.projectedPoints) * 100).round

  override def toString: String = s"Lineup(id: $id, salary: $salary)[${filledSlots.toList.sortBy(_._1.toString).map { case (slot, player) => s"$slot: $player" }.mkString(", ")}]"
}