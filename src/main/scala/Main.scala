import scala.io.Source

object Main extends App{
  val field = Field.create2(Vector[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 15))
  val input = Input.input()
  Game(field, input).play
}