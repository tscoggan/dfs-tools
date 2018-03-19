package mlb

import retrosheet._
import model._
import utils._

object MLBDataAnalyzerApp extends App {

  var totalGames = 0

  FileUtils.getListOfFiles(Configs.dataFileDir, ".EVA", ".EVN").foreach { file =>
    val parser = new EventFileParser(file.getPath)
    totalGames += parser.games.length
    parser.games.zipWithIndex.foreach { case (game, i) => println(s"Game ${i + 1}: $game") }
  }

  println(s"\n*** Total games: $totalGames ***")

}