import scala.util.Random.shuffle

case class Field(field: Vector[Byte], size: Byte) {
  def squaredSize: Int = size * size

  def move(move: Byte): Option[Field] = {
    def validMove(move: Byte): Option[(Int, Int)]= {
      val indexMove = field.indexOf(move)
      val indexEmpty = field.indexOf(squaredSize)
      val distance = scala.math.abs(indexEmpty-indexMove)
      Option(indexMove, indexEmpty).filter(_ => distance == 1 || distance == size)
    }
    validMove(move) match {
      case Some((x, y)) =>
        Option(this.swap(x, y)).filter(newField => !Field.isSolved(newField.field))
      case None => Some(this)
    }
  }

  def swap(i1: Int, i2: Int): Field = {
    this.copy(field = field.updated(i1, field(i2)).updated(i2, field(i1)))
  }

  override def toString: String = {
    field.grouped(size).map(_.mkString("\t")).mkString("\n").replaceAll(squaredSize.toString, "@") + "\n"
  }
}

object Field {
  val DefaultSize = 4.toByte

  def create(size: Byte = DefaultSize): Field = new Field(generateField(size), size)

  def createByField(field: Vector[Byte], size: Byte = DefaultSize):Field = new Field(field, size)
  // add extra constructor for testing purposes

  def generateField(size: Byte): Vector[Byte] = {
    def l: Stream[Vector[Byte]] = shuffle(1 to 15).map(_.toByte).toVector #:: l
    l.filter(valid).filterNot(isSolved).head :+ (size*size).toByte
  }

  def valid(field: Vector[Byte]): Boolean ={
    def inversions(field: List[Byte]): Int = field match {
      case Nil => 0
      case x :: xs => xs.count(_ < x) + inversions(xs)
    }
    inversions(field.toList) % 2 == 0
  }

  def isSolved(field: Vector[Byte]): Boolean = {
    field equals field.sorted
  }
}
