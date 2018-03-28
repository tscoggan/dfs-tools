package mlb.model

sealed trait Handedness {
  def toVerboseString: String 
}

case object Left extends Handedness {
  override def toString: String = "L"
  override def toVerboseString: String = "Left-handed"
}

case object Right extends Handedness {
  override def toString: String = "R"
  override def toVerboseString: String = "Right-handed"
}

case object Switch extends Handedness {
  override def toString: String = "S"
  override def toVerboseString: String = "Switch-hitter"
}

object Handedness {

  implicit def stringToHandedness(s: String): Handedness = s.trim.toUpperCase match {
    case "L"       => Left
    case "R"       => Right
    case "B" | "S" => Switch
  }

}