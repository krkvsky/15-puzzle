object Presenter {
  def apply(field: Option[Field]) = field match {
    case None => println("You`ve wone! The game is over.")
    case Some(field) => println(field.toString)
  }
}