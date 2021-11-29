package tastyquery.reader

import TastyFile.tfile

object TastyUnpickler {
  def unpickleFile(bytes: IArray[Byte]): Result[TastyFile] =
    NameUnpickler.unpickleHeaderAndNames(bytes)

  def unpickleTrees(using TastyFile): Result[TreeUnpickler] =
    SectionUnpickler.get[StdSection.ASTs].map(TreeUnpickler.make)
}
