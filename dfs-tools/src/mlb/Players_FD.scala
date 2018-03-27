package mlb

import model._
import model.CustomTypes._
import utils.FileUtils._
import utils.StringUtils._

object Players_FD {

  val allPlayers: List[Player_FD] = {
    val file = getListOfFiles(Configs.dfsSalaryFileDir, ".csv")
      .filter(_.getName.startsWith("FanDuel-MLB-"))
      .sortBy(_.getName.trimPrefix("FanDuel-MLB-").take(10).toDate())
      .last
    Player_FD.parseFrom(file.getPath)
  }

  val playersByID: Map[String, Player_FD] = allPlayers.map { p => (p.id, p) }.toMap

  def get(playerID: String): Player_FD = playersByID.get(playerID).get // throws exception if playerID is invalid
}