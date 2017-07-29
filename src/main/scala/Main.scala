import scala.io.Source
import scala.util.Random.shuffle
import scala.util.Try

case class Field(field: Vector[Byte]) {
  def size = field.length / 2
  def squaredSize = field.length

  def move(move: Byte): Option[Field] = {
    def validMove(move: Byte): Option[(Int, Int)]= {
      val indexMove = field.indexOf(move)
      val indexEmpty = field.indexOf(squaredSize)
      val distance = scala.math.abs(indexEmpty-indexMove)
      if(distance == 1 || distance == size)
        Some(indexMove, indexEmpty)
      else
        None
    }
    validMove(move) match {
      case Some((x, y)) => {
        val newField = this.swap(x, y)
        if(newField.isSolved) None else Some(newField)
      }
      case None => Some(this)
    }
  }

  def swap(i1: Int, i2: Int): Field = {
    this.copy(field = field.updated(i1, field(i2)).updated(i2, field(i1)))
  }

  def isSolved: Boolean = {
    field equals field.sorted
  }

  override def toString = {
    field.grouped(4).map(_.mkString("\t")).mkString("\n").replaceAll(squaredSize.toString, "@")
  }
}

object Field {
  def create(size: Byte = 4) = new Field(generateField(size))

  def generateField(size: Byte): Vector[Byte] = {
    def inversions(field: List[Byte]): Int = field match {
      case Nil => 0
      case x :: xs => xs.count(_ < x) + inversions(xs)
    }
    def valid(field: Vector[Byte]): Boolean =
      inversions(field.toList) % 2 == 0
    def l: Stream[Vector[Byte]] = shuffle(1 to 15).map(_.toByte).toVector #:: l
    l.filter(valid).head :+ (size*size).toByte
  }
}

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

object Presenter {
  def apply(field: Option[Field]) = field match {
    case None => println("You`ve wone! The game is over.")
    case Some(field) => println(field.toString)
  }


}

object Input {
  def input: Iterator[Byte] = {
    Source.stdin.getLines().filter(validate).map(x => x.toByte)
  }

  def validate(x: String) = Try(x.toByte).isSuccess
}

object Main extends App{
  val field = Field.create()
  val game = Game(field)
  game.play(Input.input)
}