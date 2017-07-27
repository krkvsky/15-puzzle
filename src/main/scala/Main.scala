import scala.None
import scala.util.Random.shuffle

object Main extends App{

  type Field = Vector[Option[Int]]

  def generateField: Field = {
    def l: Stream[Vector[Int]] = shuffle(1 to 15).toVector #:: l
    l.filter(valid)
      .head
      .map(Some(_)) :+ None
  }

  def valid(field: Vector[Int]): Boolean =
    inversions(field.toList) % 2 == 0

  def inversions(field: List[Int]): Int = field match {
    case Nil => 0
    case x :: xs => xs.count(_ < x) + inversions(xs)
  }

  val f = List(13, 2, 10, 3, 1, 12, 8, 4, 5, 9, 6, 15, 14, 11, 7)
//  println(generateField)
  println(valid(f.toVector))
}
