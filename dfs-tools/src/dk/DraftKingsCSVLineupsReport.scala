package dk

import scala.io.Source

object DraftKingsCSVLineupsReport extends App {

  val filename = """C:\Users\Tom\Desktop\draftkings nba 730pm- Classic (Turbo)- 2 Games (2).csv"""

  val lines = Source.fromFile(filename).getLines().toList
  val playerCounts = lines.tail.flatMap(_.split(",")).groupBy { p => p }.mapValues(_.length)

  playerCounts.toList.sortBy { case (player, count) => count }.reverse
    .foreach { case (p, c) => println(s"$c\t$p") }
}