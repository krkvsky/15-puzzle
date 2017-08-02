
class Game private (val field: Field, val input: Iterator[Byte], val presenter: Presenter) {
  def play(): Unit = {
    input
      .scanLeft(Some(field): Option[Field])(
        (f, m) => {
          presenter.show(
            f.flatMap(_.move(m))
          )
        }
      )
      .find(_.isEmpty)
  }
}

object Game {
  def apply(field: Field, input: Iterator[Byte], presenter: Presenter = Presenter()): Game = {
    presenter.show(Some(field))
    new Game(field, input, presenter)
  }
}
