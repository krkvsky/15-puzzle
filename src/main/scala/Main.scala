import scala.None
import scala.util.Random.shuffle
import scala.io._

object Main extends App{

  type Field = Vector[Int]
  type Path = List[Int]
  def generateField: Field = {
    def l: Stream[Vector[Int]] = shuffle(1 to 15).toVector #:: l
    l.filter(valid)
      .head :+ 16
  }

  def valid(field: Field): Boolean =
    inversions(field.toList) % 2 == 0

  def inversions(field: List[Int]): Int = field match {
    case Nil => 0
    case x :: xs => xs.count(_ < x) + inversions(xs)
  }

  def isSolved(field: Field): Boolean = {
    field equals field.sorted
  }

  def toString(field: Field): String = {
    field.grouped(4).map(_.mkString("\t")).mkString("\n")
  }

  def apply(field: Option[Field], move: Int): Option[Field] = {
    println(toString(field.get))
    if(move != 5)
      field // here we can apply or not (if move is not valid)
    else
      None // here the game is solved
  }

  def game(field: Field) = {
    if(isSolved(field))
      println("CONGARTS")
    else {

    }
  }
  val startField = generateField
  import scala.io._
  def gamePlay(moves: Iterator[Int]): Option[Field] = {
    moves.foldLeft(Some(startField): Option[Field])((f, n) => apply(f, n.toInt))
  }

  val t = gamePlay(Source.stdin.getLines().map(x => x.toInt))
//  println(toString(generateField))
}
