package mlb.model

sealed trait Position

case object Pitcher extends Position {
  override def toString: String = "P"
}

case object Catcher extends Position {
  override def toString: String = "C"
}

case object FirstBase extends Position {
  override def toString: String = "1B"
}

case object SecondBase extends Position {
  override def toString: String = "2B"
}

case object ThirdBase extends Position {
  override def toString: String = "3B"
}

case object ShortStop extends Position {
  override def toString: String = "SS"
}

case object Outfield extends Position {
  override def toString: String = "OF"
}

case object DesignatedHitter extends Position {
  override def toString: String = "DH"
}

object Position {

  implicit def stringToPosition(s: String): Position = s.trim.toUpperCase match {
    case "P" => Pitcher
    case "C" => Catcher
    case "1B" => FirstBase
    case "2B" => SecondBase
    case "3B" => ThirdBase
    case "SS" => ShortStop
    case "OF" | "RF" | "LF" | "CF" => Outfield
    case "DH" => DesignatedHitter
  }

}