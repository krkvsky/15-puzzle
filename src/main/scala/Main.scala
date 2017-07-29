object Main extends App{
  val field = Field.create()
  val game = Game(field)
  game.play(Input.input())
}