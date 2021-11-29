package tastyquery.reader

import tastyquery.util.{Ref, tagged}, tagged.{@@, Proof, Proven}
import TastyFile.tfile

import dotty.tools.tasty.TastyReader
import dotty.tools.tasty.UnpickleException

object SectionUnpickler {
  opaque type CanTag <: Singleton = Proof

  opaque type Sections = Map[String, Ref[TastyReader]]

  private[reader] def sections(sections: Map[String, Ref[TastyReader]])(using NameUnpickler.ParsedSections): Sections =
    sections

  def get[S](using section: Section[S])(using TastyFile): Result[Ref[TastyReader] @@ S] = {
    val sections: Sections = tfile.sections // TODO: sections must be saved to variable, or else `get` is not found
    sections.get(section.name) match {
      case Some(reader) =>
        given CanTag = Proven
        Right(section.tag(reader))
      case _ =>
        Left(UnpickleException(s"Section ${section.name} not found in TASTy file"))
    }
  }
}
