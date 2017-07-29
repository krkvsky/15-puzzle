import scala.io.Source

object Main extends App{
  val field = Field.create()
  val input = Input.input()
  Game(field, input).play
}