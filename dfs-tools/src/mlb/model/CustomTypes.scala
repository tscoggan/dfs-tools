package mlb.model

import mlb.Players

object CustomTypes {
  type PlayerID = String // Retrosheet player ID
  type MLBPlayerID = String // MLB.com player ID
  type TeamID = String
  type GameNumber = Int
  type DFSScoringSystem = String
  type Play = String
  type Modifier = String
  type Advance = String
  type BattingPosition = Int
}