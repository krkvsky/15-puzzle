class Game private (val field: Field) {
  def play(moves: Iterator[Byte]): Option[Field] = {
    moves
      .scanLeft(Some(field): Option[Field])(
        (f, m) => {
          val newField = f.flatMap(_.move(m))
          Presenter(newField)
          newField
        }
      )
      .find(_.isEmpty)
      .flatten
  }
}

object Game {
  def apply(field: Field) = {
    Presenter(Some(field))
    new Game(field)
  }
}