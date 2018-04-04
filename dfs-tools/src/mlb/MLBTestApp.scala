package mlb

import retrosheet._
import mlbdotcom._
import model._
import utils._
import utils.StringUtils._

object MLBTestApp extends App {

  //  val games = FileUtils.getListOfFiles(Configs.Retrosheet.dataFileDir_2017, ".EVA", ".EVN").flatMap { file => new EventFileParser(file.getPath).games }
  //
  //  val season = Season(2017, games)
  //
  //  season.statsByPlayer("santd001").gamesStarted.foreach { g => println(g.game.get.alias + " -> " + g.fantasyPoints()) }

  //Players.fanduelPlayers.filter(_.player.isEmpty).sortBy(_.nickname).foreach(p => println(s"$p ${p.id} --> ${p.player.getOrElse("")} ${p.player.map(_.id).getOrElse("")}"))

  //Teams.allTeams.foreach(println(_))

  //  Players.allPlayers.sortBy(_.name).foreach { p =>
  //    if (p.fanduel.isEmpty || p.draftkings.isEmpty) println(s"$p ${p.id} --> ${p.fanduel} / ${p.draftkings}")
  //  }

//  val gameURLs = MLBWebsiteParser.getGameURLs("2017-06-01".toDate())
//  println("Loading "+gameURLs.head)
//  println(MLBWebsiteParser.getPlayerURLs(gameURLs.head).mkString("\n"))
  
  rg.StartingLineups.all.foreach(println(_))

}