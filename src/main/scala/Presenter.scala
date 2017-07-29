import java.io.OutputStream

case class Presenter(val outStream: OutputStream = Console.out) {
  def show(field: Option[Field]) = {
    val message = field match {
      case None => {
        print("You`ve wone! The game is over. Enter any number and press enter to end game.")
        outStream.close()
      }
      case Some(field) => print(field.toString)
    }
  }

  def print(message: String) = outStream.write(message.toCharArray.map(_.toByte))
}