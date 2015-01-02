
import java.io.Reader

// mixin to Reader to interrupt reading
trait Stoppable extends Reader {
  @volatile private[this] var _stopped = false

  def stop() = _stopped = true

  abstract override def read(): Int = {
    if (!_stopped) super.read()
    else -1
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val file = new File("test.txt")
    val reader = new FileReader(file) with Stoppable
  }
}

