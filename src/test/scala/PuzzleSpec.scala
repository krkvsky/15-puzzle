import org.scalatest.{FlatSpec, WordSpec}

class PuzzleSpec extends WordSpec{

  "Field" must{
    val field = Field.create()
    val innerField = field.field

    "generate valid board" in {
      assert(
        !Field.valid(Vector[Byte](1,2,3,4,5,6,7,8,9,10,11,12,13,15,14))
      )
      assert(
        Field.valid(Vector[Byte](1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
      )
    }

    "be printable" in {
      assert(field.toString.length > 0)
    }

    "contain all information from field" in {
      assert(innerField.count(x => field.toString.contains(x.toString)) == (innerField.length - 1))
    }

    "can apply swap on elements" in {
      val first = innerField.updated(2, innerField(1)).updated(1, innerField(2))
      val second = field.swap(1, 2).field
      assert(first equals second)
    }

    "apply move only if it is valid" in {
      assert(! (field.move(innerField(14)).get equals field))
      assert(field.move(innerField(1)).get equals field)
    }

    "check for being solved" in {
      assert(!Field.isSolved(Vector[Byte](
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 15
      )))
      assert(Field.isSolved(Vector[Byte](
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
      )))
    }
  }

  "Presenter" must {
    val stream = new java.io.ByteArrayOutputStream()
    val presenter = Presenter(stream)
    "Print game field" in {
      assert(stream.size == 0)
      val field = Field.create()
      presenter.show(Some(field))
      assert(stream.size != 0)
    }
    "Perform another action if game is ended" in {
      val anotherStream = new java.io.ByteArrayOutputStream()
      val presenter2 = Presenter(anotherStream)
      assert(anotherStream.size == 0)
      presenter2.show(None)
      assert(anotherStream.size != 0)
      assert(anotherStream.size != stream.size)
    }
  }

  "Input" must {
    "validate different occasions" in {
      assert(Input.validate("12"))
      assert(!Input.validate("thisisstring"))
      assert(!Input.validate(""))
      assert(!Input.validate("128"))
    }

    "accept any string iterators" in {
      val etern = Stream.continually("42").toIterator
      assert(Input.input(etern).isInstanceOf[Iterator[Byte]])
    }
  }

  "Gameplay" must {
    "print congats message if moves will lead to win" in {
      val gameStream = new java.io.ByteArrayOutputStream()
      val field = Field.createByField(Vector[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 15))
      val input = List(15.toByte).toIterator
      Game(field, input, new Presenter(gameStream)).play
      assert(gameStream.toString.contains("over"))
    }
  }
}