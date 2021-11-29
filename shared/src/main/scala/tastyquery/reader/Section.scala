package tastyquery.reader

import tastyquery.util.tagged.@@

trait Section[S] {
  def name: String
  def tag[T](t: T)(using SectionUnpickler.CanTag): T @@ S
}
