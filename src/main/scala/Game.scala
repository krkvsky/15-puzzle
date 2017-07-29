class Game private (val field: Field, val input: Iterator[Byte], val presenter: Presenter) {
  def play: Option[Field] = {
    input
      .takeWhile(x => !presenter.outStream.toString.contains("wone"))
      .scanLeft(Some(field): Option[Field])(
        (f, m) => {
          val newField = f.flatMap(_.move(m))
          presenter.show(newField)
          newField
        }
      )
      .find(_.isEmpty)
      .flatten
  }
}

object Game {
  def apply(field: Field, input: Iterator[Byte], presenter: Presenter = Presenter()) = {
    presenter.show(Some(field))
    new Game(field, input, presenter)
  }
}