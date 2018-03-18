package rotogrinders

import ContestType._

case class Player(
    id: Int,
    name: String,
    salary: Int,
    team: String,
    position: String,
    opponent: String,
    projFloor: Option[Float],
    projCeiling: Option[Float],
    projPoints: Float) {

  val ptsPerSalary: Double = projPoints / salary

  val positions: Set[String] = position.contains("/") match {
    case true  => position.split("/").toSet
    case false => Set(position)
  }

  def canFill(slot: LineupSlot): Boolean = slot.samePositionAs(this)

  def canBeAddedTo(lineup: Lineup): Boolean = lineup.canAdd(this)

  override def toString: String = s"$name ($position)" //, id: $id, salary: $salary, team: $team, opponent: $opponent, projFloor: $projFloor, projCeiling: $projCeiling, projPoints: $projPoints, ptsPerSalary: $ptsPerSalary)"

}

object Player {

  import scala.io.Source

  def parseFrom(projectionsFileName: String): List[Player] = Source.fromFile(projectionsFileName).getLines.toList
    .map(_.trim)
    .filter(_.nonEmpty)
    .zipWithIndex
    .map {
      case (line, id) =>
        val values = line.split(",").map(_.trim)

        if (values.length != 8) throw new Exception(s"Invalid projections file format: $projectionsFileName")

        Player(id, values(0), values(1).toInt, values(2), values(3), values(4),
          values(6).isEmpty match {
            case true  => None
            case false => Some(values(6).toFloat)
          },
          values(5).isEmpty match {
            case true  => None
            case false => Some(values(5).toFloat)
          },
          values(7).toFloat)
    }

}