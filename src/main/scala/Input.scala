import scala.io.Source
import scala.util.Try
object Input {
  def input: Iterator[Byte] = {
    Source.stdin.getLines().filter(validate).map(x => x.toByte)
  }
  def validate(x: String) = Try(x.toByte).isSuccess
}