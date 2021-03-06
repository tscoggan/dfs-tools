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

  import mlb.StatsSinceStartOfSeasonBeforeLast.stats._
  mlb.StatsSinceStartOfSeasonBeforeLast.stats.logSummary

  List("FD", "DK").foreach { site =>
    val projectionsFile = s"${Configs.projectionsHistoryDir}/${Players.projectionsDate.print()}_${site}.csv"
    log("Saving projections to file: " + projectionsFile)
    val header = s"Player ID, Player Name, Projected FPTS (${site}), Position (${site}), Salary (${site}), Team, Value"
    val projections = {
      startingHitterStats.flatMap {
        case (p, stats) => site match {
          case "DK" => stats.projFptsDK.map { projFptsDK => s"${p.id},${p.name.replaceAll(",", "")},${projFptsDK.rounded(2)},${p.draftkings.map(_.position).getOrElse("")},${p.draftkings.map(_.salary).getOrElse("99999")},${p.team},${(1000d * (projFptsDK / p.draftkings.map(_.salary).getOrElse(99999).toDouble)).rounded(2)}" }
          case "FD" => stats.projFptsFD.map { projFptsFD => s"${p.id},${p.name.replaceAll(",", "")},${projFptsFD.rounded(2)},${p.fanduel.map(_.position).getOrElse("")},${p.fanduel.map(_.salary).getOrElse("99999")},${p.team},${(1000d * (projFptsFD / p.fanduel.map(_.salary).getOrElse(99999).toDouble)).rounded(2)}" }
          case other =>
            log("ERROR: Invalid projections site: " + other)
            None
        }
      }.toList ++
        startingPitcherStats.flatMap {
          case (p, stats) => site match {
            case "DK" => stats.projFptsDK.map { projFptsDK => s"${p.id},${p.name.replaceAll(",", "")},${projFptsDK.rounded(2)},${p.draftkings.map(_.position).getOrElse("")},${p.draftkings.map(_.salary).getOrElse("99999")},${p.team},${(1000d * (projFptsDK / p.draftkings.map(_.salary).getOrElse(99999).toDouble)).rounded(2)}" }
            case "FD" => stats.projFptsFD.map { projFptsFD => s"${p.id},${p.name.replaceAll(",", "")},${projFptsFD.rounded(2)},${p.fanduel.map(_.position).getOrElse("")},${p.fanduel.map(_.salary).getOrElse("99999")},${p.team},${(1000d * (projFptsFD / p.fanduel.map(_.salary).getOrElse(99999).toDouble)).rounded(2)}" }
            case other =>
              log("ERROR: Invalid projections site: " + other)
              None
          }
        }.toList
    }
    FileUtils.writeLinesToFile(header :: projections, projectionsFile, true)
  }

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

  val teamsWithMissingStarters = startingHittersByTeam.filter { case (team, hitters) => hitters.length < 8 }.map(_._1)
  if (teamsWithMissingStarters.nonEmpty) throw new Exception(teamsWithMissingStarters.mkString(", ") + " don't have enough starting hitters --- try deleting 0's from FD \"Batting Order\" column in Excel")

  log("\n**************************************************")
  log("*** Hitter stacks ***")
  log("**************************************************\n")

  log("\n### Pitchers ranked by FPTS given up per plate appearance: ###\n")
  val pitcherRows = startingPitchers
    .sortBy { p => pitcherStatsAllowedToAllHitters.get(p).map(_.fptsPerAtBatAgainst_FD).getOrElse(0.0d) }.reverse
    .map { p =>
      val vsAll = pitcherStatsAllowedToAllHitters.get(p)
      val vsLeft = pitcherStatsAllowedToLefties.get(p)
      val vsSwitch = pitcherStatsAllowedToSwitchHitters.get(p)
      val vsRight = pitcherStatsAllowedToRighties.get(p)

      mlb.Configs.blogFormat.toUpperCase match {
        case "RG" =>

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

        case "DRAFTSHOT" => s"<tr><td>${p.toStringTeamOnly}</td><td>${p.opponent.get}</td>" + {
          if (p.fanduel.nonEmpty) {
            s"""<td>${vsAll.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsAll.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>""" +
              s"""<td>${vsLeft.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsLeft.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>""" +
              s"""<td>${vsSwitch.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsSwitch.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>""" +
              s"""<td>${vsRight.map(_.fptsPerAtBatAgainst_FD.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsRight.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>"""
          } else "<td>N/A</td><td>N/A</td><td>N/A</td><td>N/A</td>"
        } + {
          if (p.draftkings.nonEmpty) {
            s"""<td>${vsAll.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsAll.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>""" +
              s"""<td>${vsLeft.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsLeft.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>""" +
              s"""<td>${vsSwitch.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsSwitch.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>""" +
              s"""<td>${vsRight.map(_.fptsPerAtBatAgainst_DK.rounded(1)).getOrElse("N/A")} <span style="color: blue;"><em>(${vsRight.map(_.atBatsAgainst).getOrElse("0")} PA)</em></span></td>"""
          } else "<td>N/A</td><td>N/A</td><td>N/A</td><td>N/A</td>"
        } + s"</tr>"
      }
    }.mkString("\n")

  val pitcherTableHeader = mlb.Configs.blogFormat.toUpperCase match {
    case "RG" =>
      s"|||_\\4. FPTS / PA given up (FanDuel)|_\\4. FPTS / PA given up (DraftKings)|\n" +
        s"|_. Pitcher|_. Opponent|_. vs All|_. vs Lefties|_. vs Switch|_. vs Righties|_. vs All|_. vs Lefties|_. vs Switch|_. vs Righties|\n"
    case "DRAFTSHOT" => s"""<div class="table-1">
      |<table width="100%">
      |<tbody>
      |<tr>
      |<td></td>
      |<td></td>
      |<th colspan="4"><span class="caps">FPTS</span> / PA given up (FanDuel)</th>
      |<th colspan="4"><span class="caps">FPTS</span> / PA given up (DraftKings)</th>
      |</tr>
      |<tr>
      |<th>Pitcher</th>
      |<th>Opponent</th>
      |<th>vs All</th>
      |<th>vs Lefties</th>
      |<th>vs Switch</th>
      |<th>vs Righties</th>
      |<th>vs All</th>
      |<th>vs Lefties</th>
      |<th>vs Switch</th>
      |<th>vs Righties</th>
      |</tr>""".stripMargin
  }

  val pitcherTableFooter = mlb.Configs.blogFormat.toUpperCase match {
    case "RG"        => ""
    case "DRAFTSHOT" => s"</tbody></table></div>"
  }

  log(pitcherTableHeader + pitcherRows + pitcherTableFooter)

  log("\n### Pitchers ranked by FPTS given up per plate appearance by batter handedness (Minimum 100 PA): ###\n")
  log(toTable(
    List("Pitcher", "Opponent", "Against hitters who bat...", "FPTS/PA given up (FD)", "FPTS/PA given up (DK)", "# Plate appearances against"),
    startingPitchers
      .flatMap { pitcher =>
        List(pitcherStatsAllowedToLefties.get(pitcher), pitcherStatsAllowedToRighties.get(pitcher), pitcherStatsAllowedToSwitchHitters.get(pitcher)).flatten
      }
      .sortBy(_.fptsPerAtBatAgainst_FD).reverse
      .filter(s => s.atBatsAgainst >= 100 && s.fptsPerAtBatAgainst_FD > 2.4)
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
  log("*** Hitter Stacks ***")
  log("**************************************************\n")

  print("\nh2. My App's Top-Value Hitter Stacks (FanDuel)\n\nbc=.. ")
  teamsOnSlate.map { team =>
    val stack = startingHittersByTeam(team)
      .sortBy { h => startingHitterStats.get(h).flatMap(_.valueScoreFD).getOrElse(0.0) }.reverse.take(Configs.stackSize)
      .sortBy(_.battingPosition.getOrElse(10))
    val rankScore = mean(stack.flatMap(h => startingHitterStats.get(h).map(_.valueScoreFD.getOrElse(0.0))))
    (stack -> rankScore)
  }.sortBy(_._2).reverse.take(Configs.numberOfStacks).map {
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
    val stack = startingHittersByTeam(team)
      .sortBy { h => startingHitterStats.get(h).flatMap(_.valueScoreDK).getOrElse(0.0) }.reverse.take(Configs.stackSize)
      .sortBy(_.battingPosition.getOrElse(10))
    val rankScore = mean(stack.flatMap(h => startingHitterStats.get(h).map(_.valueScoreDK.getOrElse(0.0))))
    (stack -> rankScore)
  }.sortBy(_._2).reverse.take(Configs.numberOfStacks).map {
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
  log("*** Value hitters - FD & DK ***")
  log("**************************************************\n")

  log(s"\n${toHeader(3, "FanDuel – expensive hitters ($3500 or more)")}\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    expensiveHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log(s"\n${toHeader(3, "FanDuel – midrange hitters ($2500 to $3500)")}\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    midrangeHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log(s"\n${toHeader(3, "FanDuel – cheap hitters (less than $2500)")}\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    cheapHitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projValueFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log(s"\n${toHeader(3, "DraftKings – expensive hitters ($4500 or more)")}\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    expensiveHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log(s"\n${toHeader(3, "DraftKings – midrange hitters ($3500 to $4500)")}\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    midrangeHitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projValueDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projValueDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log(s"\n${toHeader(3, "DraftKings – cheap hitters (less than $3500)")}\n")
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

  log(s"\n${toHeader(3, "Top 10 hitters ranked by projected FPTS (FanDuel)")}\n")
  log(toTable(
    List("Hitter", "FD Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    hitters_FD.filter(p => startingHitterStats.get(p).flatMap(_.projFptsFD).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projFptsFD.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_FD, p.fanduel.map(fd => "$" + fd.salary).get, stats.opposingPitcher, stats.projFptsFD.get.rounded(2), stats.projValueFD.get.rounded(2))
      }))

  log(s"\n${toHeader(3, "Top 10 hitters ranked by projected FPTS (DraftKings)")}\n")
  log(toTable(
    List("Hitter", "DK Salary", "Opposing Pitcher", "Projected FPTS", "Value"),
    hitters_DK.filter(p => startingHitterStats.get(p).flatMap(_.projFptsDK).nonEmpty)
      .map(p => (p, startingHitterStats.get(p).get))
      .sortBy(_._2.projFptsDK.get).reverse.take(10)
      .map {
        case (p, stats) =>
          List(p.toString_DK, p.draftkings.map(dk => "$" + dk.salary).get, stats.opposingPitcher, stats.projFptsDK.get.rounded(2), stats.projValueDK.get.rounded(2))
      }))

  log("\n**************************************************")
  log("*** Pitchers ***")
  log("**************************************************\n")

  log(s"\n${toHeader(3, "Top starting pitchers by value (FanDuel) - min 25 FPTS")}\n")
  log(toTable(
    List("Pitcher", "Salary (FD)", "Opponent", "BvP Sample Size (# PA)", "Projected FPTS (FD)", "Value (FD)"),
    startingPitcherStats.toList
      .filter { case (p, stats) => p.fanduel.nonEmpty && season.hasStatsFor(p) && stats.projFptsFD.getOrElse(0.0) >= 25 }
      .sortBy(_._2.projValueFD.getOrElse(0.0)).reverse
      .map {
        case (p, stats) =>
          List(p, p.fanduel.map(fd => "$" + fd.salary).get, p.opponent.get, stats.pitcherBvpStats_FD.map(_.atBats).getOrElse(0),
            stats.projFptsFD.map(_.rounded(2)).getOrElse("???"), stats.projValueFD.map(_.rounded(2)).getOrElse("???"))
      }))

  log(s"\n${toHeader(3, "Top starting pitchers by value (DraftKings) - min 10 FPTS")}\n")
  log(toTable(
    List("Pitcher", "Salary (DK)", "Opponent", "BvP Sample Size (# PA)", "Projected FPTS (DK)", "Value (DK)"),
    startingPitcherStats.toList
      .filter { case (p, stats) => p.draftkings.nonEmpty && season.hasStatsFor(p) && stats.projFptsDK.getOrElse(0.0) >= 10 }
      .sortBy(_._2.projValueDK.getOrElse(0.0)).reverse
      .map {
        case (p, stats) =>
          List(p, p.draftkings.map(dk => "$" + dk.salary).get, p.opponent.get, stats.pitcherBvpStats_DK.map(_.atBats).getOrElse(0),
            stats.projFptsDK.map(_.rounded(2)).getOrElse("???"), stats.projValueDK.map(_.rounded(2)).getOrElse("???"))
      }))

}