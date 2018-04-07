package mlb.model

sealed trait VisitingOrHomeTeam 

case object Visiting extends VisitingOrHomeTeam {
  override def toString: String = "Visiting"
}

case object Home extends VisitingOrHomeTeam {
  override def toString: String = "Home"
}