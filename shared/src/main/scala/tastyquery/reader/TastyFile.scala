package tastyquery.reader

import dotty.tools.tasty.TastyReader
import tastyquery.ast.Names.TermName

final case class TastyFile(names: NameMap[TermName], sections: SectionUnpickler.Sections)

object TastyFile {
  transparent inline def tfile(using tfile: TastyFile): TastyFile = tfile
}
