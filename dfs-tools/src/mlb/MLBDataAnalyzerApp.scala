package mlb

import retrosheet._
import model._
import utils._

object MLBDataAnalyzerApp extends App {

//  val games = FileUtils.getListOfFiles(Configs.Retrosheet.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }
//
//  val season = Season(2017, games)
//
//  season.statsByPlayer("santd001").gamesStarted.foreach { g => println(g.game.get.alias + " -> " + g.fantasyPoints()) }
  
  Players_FD.allPlayers.sortBy(_.nickname).foreach(p => println(s"$p --> ${Players.playersByName.get(p.nickname)}"))

}