package mlb

import retrosheet._
import model._
import utils._

object MLBDataAnalyzerApp extends App {

  //  FileUtils.getListOfFiles(Configs.dataFileDir_2017, ".EVA", ".EVN").foreach { file =>
  //    val parser = new EventFileParser(file.getPath)
  //    totalGames += parser.games.length
  //    parser.games.zipWithIndex.foreach { case (game, i) => println(s"Game ${i + 1}: $game") }
  //  }

  //  val gamesOf2017 = FileUtils.getListOfFiles(Configs.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }
  //  
  //  val season2017 = new Season(2017, gamesOf2017)
  //
  //  println(s"\n*** Total 2017 games: ${gamesOf2017.length} ***")

  val games = FileUtils.getListOfFiles(Configs.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }

  val season = Season(2017, games)

  season.statsByPlayer("santd001").gamesStarted.foreach { g => println(g.game.get.alias + " -> " + g.fantasyPoints()) }

}