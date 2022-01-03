import tastyquery.reader.TastyUnpickler

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import js.typedarray._

object Main {
  @js.native @JSImport("fs", "readFileSync") def readFileSync(name: String): js.typedarray.Uint8Array = js.native

  def main(args: Array[String]): Unit = {
    val filename = "/home/cache-nez/work/scala/sandbox/example/target/scala-3.0.0-M1/classes/mypackage/X.tasty"
    val bytes = IArray.unsafeFromArray(new Int8Array(readFileSync(filename).buffer).toArray)
    val tastyfile = TastyUnpickler.unpickleFile(bytes).toTry.get
  }
}
