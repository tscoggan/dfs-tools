package mlb.retrosheet

import scala.collection.mutable
import mlb.model._
import utils.Logger._
import mlb.model.CustomTypes._

class Bases {
  import Bases._

  var baserunners: mutable.Map[BaseNumber, PlayerGameStats] = mutable.Map.empty

  def clear: Unit = {
    logDebug("Clearing bases...")
    baserunners = mutable.Map.empty
  }

  def runnerOn(base: Int): Option[PlayerGameStats] = baserunners.get(base)

  /**
   * Returns # of RBIs credited for runs scored
   */
  def update(batter: PlayerGameStats, pitcher: PitcherGameStats, advances: Advance*): RBIs = {
    if (advances.nonEmpty) {
      logDebug("Updating bases: " + advances.mkString(","))

      val cleanedAdvances = advances.map { adv =>
        if (adv.contains("E") && adv.contains('X')) {
          val newAdv = adv.replace('X', '-') // error occurred, which negates the out
          logDebug(s"!!! Changing $adv to $newAdv due to error that occurred")
          newAdv
        } else adv
      }

      val rbi = cleanedAdvances.map { adv =>
        if (adv.contains('-')) { // safe advance
          val delim = adv.indexOf('-')
          val fromBase = adv.charAt(delim - 1)
          val toBase = adv.charAt(delim + 1)

          if (fromBase != toBase) {
            (fromBase, toBase) match {
              case (BATTER, HOME_BASE) =>
                batter.addRun
                if (adv.contains("NR") || adv.contains("NORBI")) 0 else 1
              case (BATTER, to) =>
                if (baserunners.contains(to.asDigit)) logDebug(s"WARNING: Destination base ($to) already has a runner (${baserunners(to.asDigit)})")
                baserunners(to.asDigit) = batter
                0
              case (from, HOME_BASE) =>
                if (!baserunners.contains(from.asDigit)) logDebug(s"WARNING: Source base ($from) has no runner")
                baserunners(from.asDigit).addRun
                baserunners.remove(from.asDigit)
                if (adv.contains("NR") || adv.contains("NORBI")) 0 else 1
              case (from, to) =>
                if (baserunners.contains(to.asDigit)) logDebug(s"WARNING: Destination base ($to) already has a runner (${baserunners(to.asDigit)})")
                if (!baserunners.contains(from.asDigit)) logDebug(s"WARNING: Source base ($from) has no runner")
                baserunners(to.asDigit) = baserunners(from.asDigit)
                baserunners.remove(from.asDigit)
                0
            }
          } else 0
        } else if (adv.contains('X')) { // out
          val delim = adv.indexOf('X')
          val fromBase = adv.charAt(delim - 1)
          if (fromBase.isDigit) baserunners.remove(fromBase.asDigit)
          pitcher.addOuts(1)
          0
        } else throw new Exception(s"Invalid baserunner advance: $adv")
      }.sum
      logDebug(s"Bases updated --- $rbi RBI's: ${this.toString}")
      rbi
    } else 0
  }

  override def toString: String = s"Bases[1st: ${baserunners.get(1)}, 2nd: ${baserunners.get(2)}, 3rd: ${baserunners.get(3)}]"
}

object Bases {
  type BaseNumber = Int // 0 (batter), 1, 2, 3, or 4 (home) 
  type Base = Char // 'B', '1', '2', '3', or 'H'
  type RBIs = Int

  val BATTER: Base = 'B'
  val FIRST_BASE: Base = '1'
  val SECOND_BASE: Base = '2'
  val THIRD_BASE: Base = '3'
  val HOME_BASE: Base = 'H'

  def baseNumberOf(base: Base): BaseNumber = base match {
    case BATTER      => 0
    case FIRST_BASE  => 1
    case SECOND_BASE => 2
    case THIRD_BASE  => 3
    case HOME_BASE   => 4
  }

  def baseWithNumber(baseNumber: BaseNumber): Base = baseNumber match {
    case 0 => BATTER
    case 1 => FIRST_BASE
    case 2 => SECOND_BASE
    case 3 => THIRD_BASE
    case 4 => HOME_BASE
  }

  def nextBase(base: Base): Option[Base] = base match {
    case BATTER      => Some(FIRST_BASE)
    case FIRST_BASE  => Some(SECOND_BASE)
    case SECOND_BASE => Some(THIRD_BASE)
    case THIRD_BASE  => Some(HOME_BASE)
    case HOME_BASE   => None
  }

  def previousBase(base: Base): Option[Base] = base match {
    case BATTER      => None
    case FIRST_BASE  => Some(BATTER)
    case SECOND_BASE => Some(FIRST_BASE)
    case THIRD_BASE  => Some(SECOND_BASE)
    case HOME_BASE   => Some(THIRD_BASE)
  }

  def lengthOf(adv: Advance): Int = {
    val delim = Math.max(adv.indexOf('-'), adv.indexOf('X'))
    val fromBase = adv.charAt(delim - 1)
    val toBase = adv.charAt(delim + 1)
    baseNumberOf(toBase) - baseNumberOf(fromBase)
  }

  /**
   * Merges multiple lists of base advances into a single list with one advance (the longest one) per baserunner.
   * If there are multiple advances with the same length (example: 2-3 and 2X3) the advance from the subsequent list is chosen.
   */
  def merge(advances: List[Advance]*): List[Advance] = {
    val longestAdvancePerBase = advances.reduceLeft(_ ++ _).groupBy(adv => baseNumberOf(adv.head)) map {
      case (fromBase, advances) => (fromBase, advances.sortBy(lengthOf(_)).last)
    }
    longestAdvancePerBase.toList.sortBy { case (fromBase, adv) => fromBase }.reverse.map(_._2)
  }
}