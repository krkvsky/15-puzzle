import scala.util.Random.shuffle

case class Field(field: Vector[Byte], size: Byte) {
  def squaredSize = size * size

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
    field.grouped(size).map(_.mkString("\t")).mkString("\n").replaceAll(squaredSize.toString, "@")
  }
}

object Field {
  def create(size: Byte = 4) = new Field(generateField(size), size)

  def generateField(size: Byte): Vector[Byte] = {
    def inversions(field: List[Byte]): Int = field match {
      case Nil => 0
      case x :: xs => xs.count(_ < x) + inversions(xs)
    }
    def valid(field: Vector[Byte], size: Byte): Boolean ={
      if(size % 2 == 0)
        inversions(field.toList) % 2 == 0
      else
        inversions(field.toList) % 2 != 0
    }

    def l: Stream[Vector[Byte]] = shuffle(1 to 15).map(_.toByte).toVector #:: l
    l.filter(valid(_, size)).head :+ (size*size).toByte
  }
}