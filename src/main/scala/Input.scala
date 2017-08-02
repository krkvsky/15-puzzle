import scala.util.Try
object Input{
  def input(in: Iterator[String] = scala.io.Source.stdin.getLines): Iterator[Byte] = {
    in.filter(validate).map(x => x.toByte)
  }
  def validate(x: String): Boolean = Try(x.toByte).isSuccess
}
