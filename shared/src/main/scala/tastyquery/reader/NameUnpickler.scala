package tastyquery.reader

import tastyquery.util.{Ref, CanShare, tagged}, CanShare.given, tagged.*
import tastyquery.ast.Names.*
import tastyquery.ast.Signature
import tastyquery.ast.{ParamSig, TermSig, TypeLenSig}

import scala.collection.mutable
import dotty.tools.tasty.{TastyReader, TastyHeaderUnpickler}
import dotty.tools.tasty.TastyBuffer.NameRef
import dotty.tools.tasty.TastyBuffer.Addr
import dotty.tools.tasty.TastyFormat.NameTags
import dotty.tools.tasty.UnpickleException

object NameUnpickler {

  opaque type ParsedSections <: Singleton = Proof

  class NameTable extends NameMap[TermName] {
    private val names = new mutable.ArrayBuffer[TermName]

    def add(name: TermName): mutable.ArrayBuffer[TermName] = names += name
    def apply(ref: NameRef): TermName = names(ref.index)

    def contents: Iterable[TermName] = names

    def exported: NameMap[TermName] =
      val local = names
      ref => local(ref.index)
  }

  /** Validates that the bytes have a correct TASTy header, and reads the names */
  private[reader] def unpickleHeaderAndNames(bytes: IArray[Byte]): Result[TastyFile] = {
    // ok to convert to mutable Array as code using TastyReader will not mutate it
    val reader = TastyReader(IArray.wrapByteIArray(bytes).unsafeArray)
    try {
      TastyHeaderUnpickler(reader).readFullHeader()
    } catch {
      case ex: UnpickleException =>
        return Left(ex)
    }
    unpickleNames(reader)
  }

  private def unpickleNames(nameReader: TastyReader): Result[TastyFile] = {
    val nameTable = NameTable()
    import nameReader.*

    def readName(): TermName = nameTable(readNameRef())

    def readString(): String = readName().toString

    def readParamSig(): ParamSig = {
      val ref = readInt()
      if (ref < 0)
        TypeLenSig(ref.abs)
      else
        TermSig(nameTable(NameRef(ref)).toTypeName)
    }

    def readNameContents(): TermName = {
      val tag = readByte()
      val length = readNat()
      val start: Addr = nameReader.currentAddr
      val end: Addr = start + length
      val result = tag match {
        case NameTags.UTF8 =>
          nameReader.goto(end)
          termName(bytes, start.index, length)
        case NameTags.QUALIFIED | NameTags.EXPANDED | NameTags.EXPANDPREFIX =>
          new QualifiedName(tag, readName(), readName().asSimpleName)
        case NameTags.UNIQUE =>
          val separator = readName().toString
          val num = readNat()
          val originals = nameReader.until(end)(readName())
          val original = if (originals.isEmpty) EmptyTermName else originals.head
          new UniqueName(separator, original, num)
        case NameTags.DEFAULTGETTER =>
          new DefaultGetterName(readName(), readNat())
        case NameTags.SIGNED | NameTags.TARGETSIGNED =>
          val original = readName()
          val target = if (tag == NameTags.TARGETSIGNED) readName() else original
          val result = readName().toTypeName
          val paramsSig = nameReader.until(end)(readParamSig())
          val sig = Signature(paramsSig, result)
          new SignedName(original, sig, target)
        case NameTags.SUPERACCESSOR | NameTags.INLINEACCESSOR =>
          new PrefixedName(tag, readName())
        case NameTags.BODYRETAINER | NameTags.OBJECTCLASS =>
          new SuffixedName(tag, readName())
        case _ => throw UnpickleException(s"unexpected tag: $tag")
      }
      assert(nameReader.currentAddr == end, s"bad name $result $start ${nameReader.currentAddr} $end")
      result
    }

    var sectionReaders = Map.empty[String, Ref[TastyReader]]
    try {
      nameReader.until(readEnd())(nameTable.add(readNameContents()))
      while (!isAtEnd) {
        val secName = readString()
        val secEnd: Addr = readEnd()
        val secReader = TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
        sectionReaders += (secName -> secReader.shareRef)
        nameReader.goto(secEnd)
      }
      given ParsedSections = Proven
      Right(TastyFile(nameTable.exported, SectionUnpickler.sections(sectionReaders)))
    } catch {
      case ex: UnpickleException => Left(ex)
    }
  }

}
