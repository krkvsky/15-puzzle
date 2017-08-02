import java.io.OutputStream

case class Presenter(outStream: OutputStream = Console.out) {
  def show(field: Option[Field]): Option[Field] = {
    field match {
      case None => {
        print("You`ve wone! The game is over. Enter any number and press enter to end game.")
        outStream.close()
      }
      case Some(f) => print(f.toString)
    }
    field
  }

  def print(message: String): Unit = {
    outStream.write(message.getBytes)
  }
}
