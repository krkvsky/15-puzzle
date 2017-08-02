import java.io.OutputStream

case class Presenter(outStream: OutputStream = Console.out) {
  def show(field: Option[Field]): Unit = {
    field match {
      case None => {
        print("You`ve wone! The game is over. Enter any number and press enter to end game.")
        outStream.close()
      }
      case Some(f) => print(f.toString)
    }
  }

  def print(message: String): Unit = outStream.write(message.toCharArray.map(_.toByte))
}
