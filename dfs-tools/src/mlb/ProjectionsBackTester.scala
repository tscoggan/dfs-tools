package mlb

import mlb._
import mlb.model._
import utils.FileUtils
import utils.Logger._
import utils.FloatUtils._
import utils.DoubleUtils._
import utils.DateTimeUtils._
import utils.MathUtils._
import utils.StringUtils._
import mlb.Players._

object ProjectionsBackTester extends App {

  import mlb.StatsSinceStartOfLastSeason.stats._
  mlb.StatsSinceStartOfLastSeason.stats.logSummary

  val gamesThisSeason: List[Game] = season.games.filter(g => !g.date.trimTime.before(Configs.MlbDotCom.seasonStartDate.trimTime))
  log(gamesThisSeason.length + " games this season")

  case class PlayerGameData(id: String, projectedFPTS: Option[Map[Weights, Double]], actualFPTS: Option[Double]) {

    //override def toString: String = s"$id --- projectedFPTS: ${projectedFPTS.map(_.rounded(1)).getOrElse("???")}, actualFPTS: ${actualFPTS.map(_.rounded(1)).getOrElse("???")}"
  }

  case class Weights(batterWeight: Int, pitcherWeight: Int) {
    override def toString: String = s"batterWeight=$batterWeight, pitcherWeight=$pitcherWeight"
  }

  val weights = List(100).flatMap { bWeight =>
    List(100).map { pWeight =>
      Weights(bWeight, pWeight)
    }
  }

  case class HitterProjection(p: Player, game: Game, opposingPitcher: Player, weights: List[Weights]) {
    //log(s"Creating projection for $p vs $opposingPitcher")

    val visitingOrHomeTeam: Option[VisitingOrHomeTeam] = {
      if (game.visitingTeamStartingHitters.contains(p)) Some(Visiting)
      else if (game.homeTeamStartingHitters.contains(p)) Some(Home)
      else None
    }

    val projAtBats: Double = visitingOrHomeTeam match {
      case Some(vh) => vh match {
        case Visiting => game.statsFor(p) match {
          case Some(stats) => leagueAvgStatsByBattingPosition_VisitingTeam.get(stats.battingPosition).map(_.atBatsPerGame).getOrElse(0.0)
          case None        => 0.0
        }
        case Home => game.statsFor(p) match {
          case Some(stats) => leagueAvgStatsByBattingPosition_HomeTeam.get(stats.battingPosition).map(_.atBatsPerGame).getOrElse(0.0)
          case None        => 0.0
        }
      }
      case None => 0.0
    }

    val projAtBatsVsOpposingPitcher: Double = {
      val pitcherStats = startingPitcherStats.get(opposingPitcher)
      pitcherStats.flatMap(_.pitcherAtBatsPerStart).getOrElse(24.0) / 9.0
    }
    val projAtBatsVsBullpen: Double = if (projAtBatsVsOpposingPitcher >= projAtBats) 0 else projAtBats - projAtBatsVsOpposingPitcher

    val hitterTotalAtBats: Int = season.hitterStatsAgainstPitcherType(opposingPitcher.throws, p).map(_.atBats).sum
    val pitcherTotalAtBats: Int = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.atBatsAgainst).getOrElse(0)
    }

    val ballparkFactor: Double = p.visitingOrHomeTeam match {
      case Some(vOrH) =>
        if (vOrH == Home) hitterBallparkFactor_HomeTeam(p.team).forHitter(p)
        else hitterBallparkFactor_VisitingTeam(p.opponent.get).forHitter(p)
      case None => 1.0
    }

    val hitterSeasonStatsFD: Option[PlayerSeasonStats] = hitterStats_FD.get(p).map(_._1)
    val hitterDeviationStatsFD: Option[DeviationStats] = hitterStats_FD.get(p).map(_._2)
    val hitterFptsPerAtBatFD: Option[Double] = season.hitterFptsPerAB_vs_PitcherType(opposingPitcher.throws, p, FanDuelMLB).map(_.fptsPerAB)
    val hitterVsPitcherStatsFD: Option[BatterVsPitcherStats] = season.hitterFptsPerAB_vs_Pitcher(opposingPitcher, p, FanDuelMLB)
    val pitcherFptsPerAtBatAllowedFD: Option[Double] = p.bats match {
      case Left   => pitcherStatsAllowedToLefties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Right  => pitcherStatsAllowedToRighties.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
      case Switch => pitcherStatsAllowedToSwitchHitters.get(opposingPitcher).map(_.fptsPerAtBatAgainst_FD).orElse(hitterFptsPerAtBatFD)
    }
    val bullpenFptsPerAtBatAllowedFD: Double = bullpenStatsAllowedToAllHitters(opposingPitcher.team).fptsPerAtBatAgainst_FD
    val projFptsFD: Option[Map[Weights, Double]] = hitterFptsPerAtBatFD.map { fptsPerAB =>
      weights.map { weight =>

        val hitterWeight = List(weight.batterWeight, hitterTotalAtBats).min
        val pitcherWeight = List(weight.pitcherWeight, pitcherTotalAtBats).min
        val hitterWeightedFptsPerAB = (0 until hitterWeight).toList.map(i => fptsPerAB * ballparkFactor)
        val pitcherWeightedFptsPerAB = if (pitcherTotalAtBats == 0) Nil else (0 until pitcherWeight).toList.map(i => pitcherFptsPerAtBatAllowedFD.get) // should park factor apply to pitcher?
        val combinedWeightedFptsPerAB = hitterWeightedFptsPerAB ++ pitcherWeightedFptsPerAB
        val fptsVsStarter = mean(combinedWeightedFptsPerAB) * projAtBatsVsOpposingPitcher

        val bullpenWeightedFptsPerAB = (0 until weight.pitcherWeight).toList.map(i => bullpenFptsPerAtBatAllowedFD) // should park factor apply to pitcher?
        val combinedWeightedFptsPerABVsBullpen = hitterWeightedFptsPerAB ++ bullpenWeightedFptsPerAB
        val fptsVsBullpen = mean(combinedWeightedFptsPerABVsBullpen) * projAtBatsVsBullpen

        (weight, fptsVsStarter + fptsVsBullpen)
      }.toMap
    }
  }

  val data = gamesThisSeason.flatMap { game =>
    val gameDateStr = game.date.print()

    game.visitingTeamStartingHitters.map { p =>
      val uniqueKey = s"${p.id}-$gameDateStr"
      val stats = HitterProjection(p, game, game.homeTeamStartingPitcher, weights)
      PlayerGameData(uniqueKey, stats.projFptsFD, game.statsFor(p).map(_.fantasyPoints(FanDuelMLB)))
    } ++
      game.homeTeamStartingHitters.map { p =>
        val uniqueKey = s"${p.id}-$gameDateStr"
        val stats = HitterProjection(p, game, game.visitingTeamStartingPitcher, weights)
        PlayerGameData(uniqueKey, stats.projFptsFD, game.statsFor(p).map(_.fantasyPoints(FanDuelMLB)))
      }

  }.filter(d => d.projectedFPTS.nonEmpty && d.actualFPTS.nonEmpty)

  log(data.length + " player-game data points to test")

  //  val w1 = Weights(200, 200)
  //  val values = data.map { d =>
  //    val actual = d.actualFPTS.get
  //    val proj = d.projectedFPTS.flatMap(_.get(w1)).get
  //    s"${if (proj > actual) actual else proj},$proj"
  //  }
  //  FileUtils.writeLinesToFile("Actual,Projection" :: values,
  //    "C:/Users/Tom/Desktop/correlation.csv",
  //    true)

  weights.foreach { weight =>

    val projections: Map[String, Double] = data.map { d =>
      val key = d.id
      val value = d.projectedFPTS.flatMap(_.get(weight)).get
      (key, value)
    }.toMap

    val actuals: Map[String, Double] = data.map(d => (d.id, d.actualFPTS.get)).toMap

    val correlation = pearsonCorrelation(projections, actuals)

    log(s"### $weight --> Correlation: " + correlation.getOrElse("???"))
  }

}