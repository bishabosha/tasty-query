package tastyquery.reader

import dotty.tools.tasty.TastyBuffer.NameRef

abstract class NameMap[+T] {
  def apply(ref: NameRef): T
}
