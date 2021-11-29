package tastyquery.util

import scala.annotation.showAsInfix

object tagged {

  private[tagged] object Evidence

  /** A default proof type with only one member,
    * can be aliased by an opaque type to create unique states.
    */
  opaque type Proof <: Singleton = Evidence.type

  /** The singleton value of type `Proof` */
  val Proven: Proof = Evidence

  /** This type is used to tag types with proof of some capability, `Ev`.
    * `String @@ Proof` is read as "`String` tagged by `Proof`"
    *
    * A tagged type can only be constructed by providing evidence
    * of the type `Ev`, and that evidence must come from
    * a singleton term.
    *
    * By controlling the construction of terms of type `Ev`, programs
    * can implement state machines that sign values as being in a
    * proven state.
    */
  @showAsInfix opaque type @@[+T, +Ev] = T

  object syntax {
    given tagSyntax: AnyRef with {
      extension [T, Ev <: Singleton](t: T) {

        /** Proves that a value can be tagged by the evidence type */
        def @@(ev: Ev): T @@ ev.type = t
      }
    }
  }

  object @@ {
    extension [T, Ev](tagged: T @@ Ev) {

      /** Forgets the evidence that a value was tagged with */
      def untagged: T = tagged
    }
  }

}
