import scala.io.Source
import scala.util.Try
object Input{
  def input(in: Iterator[String] = Source.stdin.getLines): Iterator[Byte] = {
    in.filter(validate).map(x => x.toByte)
  }
  def validate(x: String) = Try(x.toByte).isSuccess
}