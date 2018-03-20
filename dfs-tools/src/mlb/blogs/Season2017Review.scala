package mlb.blogs

import mlb._
import mlb.model._
import mlb.retrosheet._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._

object Season2017Review extends App {

  val games = FileUtils.getListOfFiles(Configs.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

  val season = new Season(2017, games)

  // Who were the top-scoring players in 2017?
  log("\n### Most FD points per plate appearance (min 100 plate appearances): ###")
  val mostPointsPerAtBat: List[(PlayerSeasonStats, Float)] = season.allHitters.filter(_.atBats >= 100).map(p => (p, p.fptsPerAtBat()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerAtBat.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(1)} FPTS (${p.atBats} plate appearances)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

  log("\n### Most FD points per game started (min 25 games started): ###")
  val mostPointsPerGameStarted: List[(PlayerSeasonStats, Float)] = season.allHitters.filter(_.numberOfGamesStarted >= 25).map(p => (p, p.fptsPerGameAsStarter()))
    .sortBy(_._2).reverse.take(10)
  mostPointsPerGameStarted.map { case (p, fpts) => s"${p.player} - ${fpts.rounded(1)} FPTS (${p.numberOfGamesStarted} games started)" }
    .zipWithIndex.map { case (str, i) => s"${i + 1}) $str" }.foreach(log(_))

}