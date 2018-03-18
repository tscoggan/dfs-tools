package mlb.model

sealed trait Handedness

case object Left extends Handedness {
  override def toString: String = "L"
}

case object Right extends Handedness {
  override def toString: String = "R"
}

case object Switch extends Handedness {
  override def toString: String = "S"
}

object Handedness {

  implicit def stringToHandedness(s: String): Handedness = s.trim.toUpperCase match {
    case "L"       => Left
    case "R"       => Right
    case "B" | "S" => Switch
  }

}