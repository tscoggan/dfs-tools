package mlb

import retrosheet._
import model._
import model.CustomTypes._
import utils.FileUtils._

object Players {

  val allPlayers: List[Player] = {
    getListOfFiles(Configs.dataFileDir_2017, ".ROS").flatMap { file =>
      PlayerData.parseFrom(file.getPath)
    }.distinct
  }

  val playersByID: Map[PlayerID, Player] = allPlayers.map { p => (p.id, p) }.toMap 
  
  def get(playerID: String): Player = playersByID.get(playerID).get // throws exception if playerID is invalid
}