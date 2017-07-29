import org.scalatest.{FlatSpec, WordSpec}

class PuzzleSpec extends WordSpec{

  "Field" must{
    val field = Field.create()

    "be printable" in {
      assert(field.toString.length > 0)
    }

    "contain all information from field" in {
      field.field
    }

  }
//
//  "Input" should {
//
//  }
//
//  "Gameplay" should {
//
//  }
}
