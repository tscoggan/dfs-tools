package mlb.model

import mlb.Players

object CustomTypes {
  type PlayerID = String
  type TeamID = String
  type GameNumber = Int
  type WindDirection = String
  type DFSScoringSystem = String
  type Play = String
  type Modifier = String
  type Advance = String
  type BattingPosition = Int

  implicit def playerIDToPlayer(playerID: PlayerID): Player = Players.get(playerID)
}