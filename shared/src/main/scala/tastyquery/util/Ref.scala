package tastyquery.util

import dotty.tools.tasty.TastyReader

trait CanShare[T] {
  extension (t: T) def shareRef: Ref[T]
}

object CanShare {
  given CanShare[TastyReader] with {
    extension (reader: TastyReader) {
      def shareRef: Ref[TastyReader] =
        Ref.fromFactory(reader)(reader => reader.subReader(reader.currentAddr, reader.endAddr))
    }
  }
}

/** A Ref is a reference to a mutable object
  * that is able to be shared without affecting upstream producers
  */
trait Ref[+T] {

  /** Produces a value of type `T`, each call to fork should return an object
    * with identical state, but not necessarily an identical reference.
    */
  def fork: T
}

object Ref {
  def fromFactory[T](init: T)(op: T => T): Ref[T] = {
    val local = op(init)
    new {
      def fork = op(local)
    }
  }
}
