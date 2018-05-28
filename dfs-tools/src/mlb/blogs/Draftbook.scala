package mlb.blogs

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

object Draftbook extends App {

  import mlb.StatsSinceStartOfLastSeason.stats._
  mlb.StatsSinceStartOfLastSeason.stats.logSummary

  log("Saving projections to file...")
  val projectionsFile = s"${Configs.projectionsHistoryDir}/${Players.projectionsDate.print()}.csv"
  val header = "Player ID, Player Name, Projected FPTS (FD)"
  val projections = startingHitterStats.flatMap {
    case (p, stats) => stats.projFptsFD.map { projFptsFD => s"${p.id},${p.name.replaceAll(",", "")},${projFptsFD.rounded(2)}" }
  }.toList
  FileUtils.writeLinesToFile(header :: projections, projectionsFile, true)

  log("\n**************************************************")
  log("*** All starters ***")
  log("**************************************************\n")

  log("\n\nStarting pitchers: \n" + startingPitchers.sortBy(_.name).map { pitcher =>
    s"$pitcher [${pitcherStatsAllowedToAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("???")} FPTS/AB against (FanDuel), " +
      s"${pitcherStatsAllowedToAllHitters.get(pitcher).map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("???")} FPTS/AB against (DraftKings)] vs: \n\t${
        startingHittersByTeam(pitcher.opponent.get).map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}, " +
                s"${stats.projFptsDK.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueDK.map(_.rounded(2)).getOrElse("???")} value on DK ${hitter.draftkings.map("($" + _.salary + ")").getOrElse("???")} "
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }

        }.mkString("\n\t")
      }"
  }.mkString("\n"))

  log("\n**************************************************")
  log("*** Hitter stacks ***")
  log("**************************************************\n")

  log("\n### Pitchers ranked by FPTS given up per plate appearance: ###\n")
  mlb.Configs.blogFormat.toUpperCase match {
    case "RG" =>
      val pitcherRows = startingPitchers
        .sortBy { p => pitcherStatsAllowedToAllHitters.get(p).map(_.fptsPerAtBatAgainst_FD).getOrElse(0.0d) }.reverse
        .map { p =>
          val vsAll = pitcherStatsAllowedToAllHitters.get(p)
          val vsLeft = pitcherStatsAllowedToLefties.get(p)
          val vsSwitch = pitcherStatsAllowedToSwitchHitters.get(p)
          val vsRight = pitcherStatsAllowedToRighties.get(p)

          s"|${p.toStringTeamOnly}|${p.opponent.get}|" + {
            if (p.fanduel.nonEmpty) {
              s"${vsAll.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsAll.map(_.atBatsAgainst).getOrElse("0")} PA)_%|" +
                s"${vsLeft.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsLeft.map(_.atBatsAgainst).getOrElse("0")} PA)_%|" +
                s"${vsSwitch.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsSwitch.map(_.atBatsAgainst).getOrElse("0")} PA)_%|" +
                s"${vsRight.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsRight.map(_.atBatsAgainst).getOrElse("0")} PA)_%|"
            } else "N/A|N/A|N/A|N/A|"
          } + {
            if (p.draftkings.nonEmpty) {
              s"${vsAll.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsAll.map(_.atBatsAgainst).getOrElse("0")} PA)_%|" +
                s"${vsLeft.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsLeft.map(_.atBatsAgainst).getOrElse("0")} PA)_%|" +
                s"${vsSwitch.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsSwitch.map(_.atBatsAgainst).getOrElse("0")} PA)_%|" +
                s"${vsRight.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} %{color:blue}_(${vsRight.map(_.atBatsAgainst).getOrElse("0")} PA)_%|"
            } else "N/A|N/A|N/A|N/A|"
          }
        }.mkString("\n")
      log(s"|||_\\4. FPTS / PA given up (FanDuel)|_\\4. FPTS / PA given up (DraftKings)|\n" +
        s"|_. Pitcher|_. Opponent|_. vs All|_. vs Lefties|_. vs Switch|_. vs Righties|_. vs All|_. vs Lefties|_. vs Switch|_. vs Righties|\n" +
        pitcherRows)

    case "DRAFTSHOT" =>
      log(toTable(
        List("Pitcher", "Opponent", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
        startingPitchers
          .sortBy { p => pitcherStatsAllowedToAllHitters.get(p).map(_.fptsPerAtBatAgainst_FD).getOrElse(0.0d) }.reverse
          .map { pitcher =>
            val statsAgainst = pitcherStatsAllowedToAllHitters.get(pitcher)
            List(pitcher.toStringTeamOnly,
              pitcher.opponent.get,
              if (pitcher.fanduel.nonEmpty) statsAgainst.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A") else "N/A",
              if (pitcher.draftkings.nonEmpty) statsAgainst.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A") else "N/A",
              statsAgainst.map(_.atBatsAgainst).getOrElse("0"))
          }))
  }

  log("\n### Pitchers ranked by FPTS given up per plate appearance by batter handedness (Minimum 50 PA): ###\n")
  log(toTable(
    List("Pitcher", "Opponent", "Against hitters who bat...", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
    startingPitchers
      .flatMap { pitcher =>
        List(pitcherStatsAllowedToLefties.get(pitcher), pitcherStatsAllowedToRighties.get(pitcher), pitcherStatsAllowedToSwitchHitters.get(pitcher)).flatten
      }
      .sortBy(_.fptsPerAtBatAgainst_FD).reverse
      .filter(s => s.atBatsAgainst >= 50 && s.fptsPerAtBatAgainst_FD > 2.4)
      .map { stats =>
        List(stats.pitcher.toStringTeamOnly,
          stats.pitcher.opponent.get,
          stats.batterHandedness.get.toVerboseString,
          if (stats.pitcher.fanduel.nonEmpty) stats.fptsPerAtBatAgainst_FD.rounded(1) else "N/A",
          if (stats.pitcher.draftkings.nonEmpty) stats.fptsPerAtBatAgainst_DK.rounded(1) else "N/A",
          stats.atBatsAgainst)
      }))

  def fanduelValueOf(stack: List[Player]): Double = {
    val totalSalary = stack.flatMap { hitter => hitter.fanduel.map(_.salary) }.sum
    if (totalSalary == 0) -1.0 else {
      val totalFPTS = stack.map { hitter => startingHitterStats.get(hitter).flatMap(_.projFptsFD).getOrElse(0.0) }.sum
      (totalFPTS / totalSalary) * 1000
    }
  }

  def draftkingsValueOf(stack: List[Player]): Double = {
    val totalSalary = stack.flatMap { hitter => hitter.draftkings.map(_.salary) }.sum
    if (totalSalary == 0) -1.0 else {
      val totalFPTS = stack.map { hitter => startingHitterStats.get(hitter).flatMap(_.projFptsDK).getOrElse(0.0) }.sum
      (totalFPTS / totalSalary) * 1000
    }
  }

  log("\n**************************************************")
  log("*** 5-hitter Stacks ***")
  log("**************************************************\n")

  print("\nh2. My App's Top-Value Hitter Stacks (FanDuel)\n\nbc=.. ")
  teamsOnSlate.map { team =>
    val stack = startingHittersByTeam(team).sortBy { h => startingHitterStats.get(h).flatMap(_.projFptsPlusValueFD).getOrElse(0.0) }.reverse.take(5)
      .sortBy(_.battingPosition.getOrElse(10))
    val rankScore = mean(stack.flatMap(h => startingHitterStats.get(h).map(_.projFptsPlusValueFD.getOrElse(0.0))))
    (stack -> rankScore)
  }.sortBy(_._2).reverse.take(5).map {
    case (stack, rankScore) =>
      s"${stack.head.team} hitters vs ${stack.head.opposingPitcher.toStringTeamOnly} - Score: ${rankScore.rounded(2)}, Value: ${fanduelValueOf(stack).rounded(2)} (FD):\n\t" +
        stack.map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}"
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }
        }.mkString("\n\t") + "\n"
  }.foreach(log(_))

  print("h2. My App's Top-Value Hitter Stacks (DraftKings)\n\nbc=.. ")
  teamsOnSlate.map { team =>
    val stack = startingHittersByTeam(team).sortBy { h => startingHitterStats.get(h).flatMap(_.projFptsPlusValueDK).getOrElse(0.0) }.reverse.take(5)
      .sortBy(_.battingPosition.getOrElse(10))
    val rankScore = mean(stack.flatMap(h => startingHitterStats.get(h).map(_.projFptsPlusValueDK.getOrElse(0.0))))
    (stack -> rankScore)
  }.sortBy(_._2).reverse.take(5).map {
    case (stack, rankScore) =>
      s"${stack.head.team} hitters vs ${stack.head.opposingPitcher.toStringTeamOnly} - Score: ${rankScore.rounded(2)}, Value: ${draftkingsValueOf(stack).rounded(2)} (DK):\n\t" +
        stack.map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsDK.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueDK.map(_.rounded(2)).getOrElse("???")} value on DK ${hitter.draftkings.map("($" + _.salary + ")").getOrElse("???")} "
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }
        }.mkString("\n\t") + "\n"
  }.foreach(log(_))

  log("\n**************************************************")
  log("*** 3-hitter Stacks (Only hitters batting 1-6) ***")
  log("**************************************************\n")

  teamsOnSlate.map { team =>
    val stack = startingHittersByTeam(team)
      .filter(p => p.battingPosition.getOrElse(10) >= 1 && p.battingPosition.getOrElse(10) <= 6)
      .sortBy { h => startingHitterStats.get(h).flatMap(_.projFptsPlusValueFD).getOrElse(0.0) }.reverse.take(3)
      .sortBy(_.battingPosition.getOrElse(10))
    val rankScore = mean(stack.flatMap(h => startingHitterStats.get(h).map(_.projFptsPlusValueFD.getOrElse(0.0))))
    (stack -> rankScore)
  }.sortBy(_._2).reverse.take(5).map {
    case (stack, rankScore) =>
      s"${stack.head.team} hitters vs ${stack.head.opposingPitcher.toStringTeamOnly} - Score: ${rankScore.rounded(2)}, Value: ${fanduelValueOf(stack).rounded(2)} (FD):\n\t" +
        stack.map { hitter =>
          startingHitterStats.get(hitter) match {
            case Some(stats) =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - " +
                s"${stats.projFptsFD.map(_.rounded(2)).getOrElse("???")} proj FPTS & " +
                s"${stats.projValueFD.map(_.rounded(2)).getOrElse("???")} value on FD ${hitter.fanduel.map("($" + _.salary + ")").getOrElse("???")}"
            case None =>
              s"${hitter.battingPosition.getOrElse("?")}) ${hitter.name} (${hitter.bats}) - NO STATS"
          }
        }.mkString("\n\t") + "\n"
  }.foreach(log(_))

  log("\n**************************************************")
  log("*** Value hitters - FD & DK ***")
  log("**************************************************\n")

  log("\nh3. FanDuel – expensive hitters ($3500 or more)\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    expensiveHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\nh3. FanDuel – midrange hitters ($2500 to $3500)\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    midrangeHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\nh3. FanDuel – cheap hitters (less than $2500)\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    cheapHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\nh3. DraftKings – expensive hitters ($4000 or more)\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    expensiveHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\nh3. DraftKings – midrange hitters ($3000 to $4000)\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    midrangeHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\nh3. DraftKings – cheap hitters (less than $3000)\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    cheapHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Top projected hitters - FD ***")
  log("**************************************************\n")

  log("\n### Top 10 hitters ranked by projected FPTS (FanDuel): ###\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    hitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projFptsFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projFptsFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Pitchers ***")
  log("**************************************************\n")

  log("\n### Starting pitchers ranked by value (FanDuel) - minimum 30 FPTS: ###\n")
  log(toTable(
    List("Pitcher", "FD Salary", "Opponent", "Sample Size (BvP PA)", "Projected FPTS", "Value"),
    startingPitcherStats.toList
      .filter { case (p, stats) => p.fanduel.nonEmpty && season.hasStatsFor(p) && stats.projFptsFD.getOrElse(0.0) >= 30 }
      .sortBy(_._2.projValueFD.getOrElse(0.0)).reverse
      .map {
        case (p, stats) =>
          List(p, p.fanduel.map(fd => "$" + fd.salary).get, p.opponent.get, stats.pitcherBvpStats_FD.map(_.atBats).getOrElse(0),
            stats.projFptsFD.map(_.rounded(2)).getOrElse("???"), stats.projValueFD.map(_.rounded(2)).getOrElse("???"))
      }))

  log("\n### Starting pitchers ranked by value (DraftKings) - minimum 20 FPTS: ###\n")
  log(toTable(
    List("Pitcher", "DK Salary", "Opponent", "Sample Size (BvP PA)", "Projected FPTS", "Value"),
    startingPitcherStats.toList
      .filter { case (p, stats) => p.draftkings.nonEmpty && season.hasStatsFor(p) && stats.projFptsDK.getOrElse(0.0) >= 20 }
      .sortBy(_._2.projValueDK.getOrElse(0.0)).reverse
      .map {
        case (p, stats) =>
          List(p, p.draftkings.map(dk => "$" + dk.salary).get, p.opponent.get, stats.pitcherBvpStats_DK.map(_.atBats).getOrElse(0),
            stats.projFptsDK.map(_.rounded(2)).getOrElse("???"), stats.projValueDK.map(_.rounded(2)).getOrElse("???"))
      }))

}