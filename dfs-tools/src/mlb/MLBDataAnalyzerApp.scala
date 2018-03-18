package mlb

import retrosheet._
import model._
import utils._

object MLBDataAnalyzerApp extends App {

  FileUtils.getListOfFiles(Configs.dataFileDir, ".EVA", ".EVN").foreach { file =>
    val parser = new EventFileParser(file.getPath)
    parser.games.zipWithIndex.foreach { case (game, i) => println(s"Game ${i+1}: $game") }
  }

}