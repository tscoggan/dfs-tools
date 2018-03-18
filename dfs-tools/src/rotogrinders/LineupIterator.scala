package rotogrinders

import utils.Logger._
import scala.annotation.tailrec

case class LineupIterator(players: List[Player], contestType: ContestType) {

  type PositionGroup = Set[String]
  type PositionGroupID = Int
  type PlayerID = Int
  type SlotID = Int
  type PlayerCombo = List[PlayerID] // players for a single position group
  type PlayerComboIndex = Int // index into an Array[PlayerCombo], e.g. from the `playerCombosByPGID` map
  type RosterCombo = List[(PositionGroupID, PlayerComboIndex)] // assignment of players to a position group
  type FilledSlot = (SlotID, PlayerID)
  type Roster = List[FilledSlot]

  private val slotsByID: Map[SlotID, LineupSlot] = contestType.slots.map { s => (s.id, s) }.toMap
  private val playersByID: Map[PlayerID, Player] = players.map { p => (p.id, p) }.toMap
  private val positionGroupsByID: Map[PositionGroupID, PositionGroup] = contestType.slots.groupBy(_.positionsAccepted.toSet).keys.zipWithIndex.map(_.swap).toMap

  private val slotIDsByPGID: Map[PositionGroupID, List[SlotID]] = {
    val pgIDs: Map[PositionGroup, PositionGroupID] = positionGroupsByID.toList.map(_.swap).toMap
    contestType.slots.groupBy(_.positionsAccepted.toSet).map { case (pg, slots) => (pgIDs(pg), slots.map(_.id)) }
  }

  private val playerIDsByPGID: Map[PositionGroupID, List[PlayerID]] = slotIDsByPGID.map {
    case (pgID, slotIDs) => (pgID, players.filter(_.canFill(slotsByID(slotIDs.head))).map(_.id))
  }

  // all player combinations of size N for each position group, where N is the # of slots for that position group
  private val playerCombosByPGID: Map[PositionGroupID, Array[PlayerCombo]] = playerIDsByPGID.map {
    case (pgID, playerIDs) => (pgID, playerIDs.combinations(slotIDsByPGID(pgID).length).toArray)
  }

  private val numberOfCombosByPGID: Map[PositionGroupID, Int] = playerCombosByPGID.map { case (pgID, combos) => (pgID, combos.length) }.toMap

  private def playerCombo(pgID: PositionGroupID, index: Int): PlayerCombo = playerCombosByPGID(pgID)(index)

  private class PositionGroupIterator(val pgID: PositionGroupID, var currentIndex: Int, maxIndex: Int) {
    def `++`: Unit = {
      if (atMax) currentIndex = 0
      else currentIndex = currentIndex + 1
    }
    def atMax: Boolean = currentIndex == maxIndex
  }

  private class RosterIterator {
    private val pgIndexes: List[PositionGroupIterator] = numberOfCombosByPGID.toList.map {
      case (pgID, numberOfCombos) => new PositionGroupIterator(pgID, 0, numberOfCombos - 1)
    }

    private var count: Int = 0

    def numberBuilt: Int = count

    private def increment(pgIterators: List[PositionGroupIterator]): Unit = pgIterators match {
      case current :: others => {
        current++

        if (current.currentIndex == 0) increment(others) // the current iterator looped back to 0, so increment the next one too
      }
      case Nil => // done
    }

    def hasNext: Boolean = pgIndexes.exists(!_.atMax) // finished if all position groups have reached their final player combos

    def next: RosterCombo = if (hasNext) {
      if (count > 0) increment(pgIndexes) // don't increment indexes the first time it's called since indexes are initialized to 0
      count = count + 1
      pgIndexes.map(i => (i.pgID, i.currentIndex))
    } else throw new Exception("RosterIterator.next invoked when hasNext == FALSE")

    override def toString: String = s"RosterIterator[${pgIndexes.map(_.currentIndex).mkString("\t")}]"
  }

  private val rosterIterator = new RosterIterator()

  def nextLineup: Option[Lineup] = rosterIterator.hasNext match {
    case true => {
      val rosterCombo: RosterCombo = rosterIterator.next
      val roster: Roster = rosterCombo.flatMap {
        case (pgID, comboIndex) =>
          val slotsIDs = slotIDsByPGID(pgID)
          val playerIDs = playerCombo(pgID, comboIndex)
          slotsIDs.zip(playerIDs)
      }
      Some(Lineup(rosterIterator.numberBuilt, contestType, roster.map { case (slotID, playerID) => (slotsByID(slotID), playersByID(playerID)) }.toMap))
    }
    case false => None
  }
  
  @tailrec
  final def nextValidLineup: Option[Lineup] = nextLineup match {
    case Some(lineup) => if (lineup.isValid) Some(lineup) else nextValidLineup
    case None => None
  }
  
  def hasNext: Boolean = rosterIterator.hasNext
  
  def numberBuilt: Int = rosterIterator.numberBuilt

  override def toString: String = "LineupBuilder[\n" +
    s"  playersByID:\n\t\t${playersByID.toList.sortBy(_._1).mkString("\n\t\t")}\n\n" +
    s"  slotsByID:\n\t\t${slotsByID.toList.sortBy(_._1).mkString("\n\t\t")}\n\n" +
    s"  positionGroupsByID:\n\t\t${positionGroupsByID.toList.sortBy(_._1).mkString("\n\t\t")}\n\n" +
    s"  numberOfCombosByPGID:\n\t\t${numberOfCombosByPGID.toList.sortBy(_._1).mkString("\n\t\t")}\n]"

}