package tastyquery.reader

import tastyquery.util.tagged, tagged.@@, tagged.syntax.given

/** Provides evidence for tagging a value as representing a TASTy section in some context */
opaque type StdSection = String

object StdSection {

  /** Provides evidence for tagging a value as representing the ASTs section in TASTy */
  opaque type ASTs <: StdSection = "ASTs"

  given SectionASTs: Index[ASTs]()

  /** Provides capabilities to tag types with a section */
  class Index[S <: StdSection: ValueOf] extends Section[S]:

    def name = valueOf[S]

    final def tag[T](t: T)(using SectionUnpickler.CanTag): T @@ S = {
      val ev = valueOf[S]
      t @@ ev
    }
}
