package tastyquery.reader

import tastyquery.Contexts.*
import tastyquery.ast.Constants.Constant
import tastyquery.ast.Names.*
import tastyquery.ast.Symbols.{ClassSymbol, ClassSymbolFactory, NoSymbol, RegularSymbol, RegularSymbolFactory, Symbol}
import tastyquery.ast.Trees.*
import tastyquery.ast.TypeTrees.*
import tastyquery.ast.Types.*
import tastyquery.util.{Ref, tagged}, tagged.@@
import tastyquery.reader.TastyFile.tfile

import tastyquery.reader.NameMap

import scala.annotation.tailrec
import scala.annotation.constructorOnly

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import dotty.tools.tasty.{TastyBuffer, TastyFormat, TastyReader}
import TastyBuffer.*
import TastyFormat.*

class TreeUnpicklerException(msg: String) extends RuntimeException(msg)

sealed trait AbstractCaseDefFactory[CaseDefType]
case object CaseDefFactory extends AbstractCaseDefFactory[CaseDef]
case object TypeCaseDefFactory extends AbstractCaseDefFactory[TypeCaseDef]

object TreeUnpickler:
  def make(reader: Ref[TastyReader] @@ StdSection.ASTs)(using TastyFile): TreeUnpickler =
    TreeUnpickler(reader.untagged.fork, tfile.names)

class TreeUnpickler private (protected val reader: TastyReader, nameAtRef: NameMap[TermName]) {

  def unpickle(using FileContext): List[Tree] = {
    @tailrec def read(acc: ListBuffer[Tree]): List[Tree] = {
      acc += readTopLevelStat
      if (!reader.isAtEnd) read(acc) else acc.toList
    }

    val symbolFork = fork
    while (!symbolFork.reader.isAtEnd) symbolFork.createSymbols
    read(new ListBuffer[Tree])
  }

  /* This method walks a TASTy file and creates all symbols in it.
   *
   * This is useful for forward references. Example: type parameters in the following case:
   * class ExampleClass[T1 <: T2, T2]
   * The example is equally applicable to methods, which can be arbitrarily nested.
   * The alternative is to create a symbol when we encounter a forward reference, but it is hard to
   * keep track of the owner in this case.
   * */
  def createSymbols(using FileContext): Unit = {
    val start = reader.currentAddr
    val tag = reader.readByte()
    tag match {
      // ---------- tags that trigger symbol creation -----------------------------------
      case PACKAGE =>
        val end = reader.readEnd()
        val pid = readPotentiallyShared({
          assert(reader.readByte() == TERMREFpkg, posErrorMsg)
          ctx.createPackageSymbolIfNew(readName)
        })
        reader.until(end)(createSymbols(using ctx.withOwner(pid)))
      case TYPEDEF =>
        val end = reader.readEnd()
        val name = readName.toTypeName
        val newOwner = if (reader.nextByte == TEMPLATE) {
          ctx.createSymbolIfNew(start, name, ClassSymbolFactory, addToDecls = true)
        } else {
          ctx.createSymbolIfNew(start, name, RegularSymbolFactory, addToDecls = true)
        }
        reader.until(end)(createSymbols(using ctx.withOwner(newOwner)))
      case DEFDEF | VALDEF | PARAM | TYPEPARAM =>
        val end = reader.readEnd()
        val name = if (tag == TYPEPARAM) readName.toTypeName else readName
        val newSymbol =
          ctx.createSymbolIfNew(start, name, RegularSymbolFactory, addToDecls = (tag != TYPEPARAM))
        reader.until(end)(createSymbols(using ctx.withOwner(newSymbol)))
      case BIND =>
        val end = reader.readEnd()
        var name: Name = readName
        if (tagFollowShared == TYPEBOUNDS) name = name.toTypeName
        ctx.createSymbolIfNew(start, name, RegularSymbolFactory)
        // bind is never an owner
        reader.until(end)(createSymbols)

      // ---------- tags with potentially nested symbols --------------------------------
      case tag if firstASTTreeTag <= tag && tag < firstNatASTTreeTag => createSymbols
      case tag if firstNatASTTreeTag <= tag && tag < firstLengthTreeTag =>
        reader.readNat()
        createSymbols
      case TEMPLATE | APPLY | TYPEAPPLY | SUPER | TYPED | ASSIGN | BLOCK | INLINED | LAMBDA | IF | MATCH | TRY | WHILE |
          REPEATED | ALTERNATIVE | UNAPPLY | REFINEDtpt | APPLIEDtpt | LAMBDAtpt | TYPEBOUNDStpt | ANNOTATEDtpt |
          MATCHtpt | CASEDEF =>
        val end = reader.readEnd()
        reader.until(end)(createSymbols)
      case SELECTin =>
        val end = reader.readEnd()
        readName
        reader.until(end)(createSymbols)
      case RETURN | SELECTouter =>
        val end = reader.readEnd()
        reader.readNat()
        reader.until(end)(createSymbols)

      // ---------- no nested symbols ---------------------------------------------------
      case _ => skipTree(tag)
    }
  }

  private def posErrorMsg(using FileContext): String = s"at address ${reader.currentAddr} in file ${ctx.getFile}"
  private def posErrorMsg(atAddr: Addr)(using FileContext): String = s"at address $atAddr in file ${ctx.getFile}"

  def forkAt(start: Addr): TreeUnpickler =
    new TreeUnpickler(reader.subReader(start, reader.endAddr), nameAtRef)

  def fork: TreeUnpickler =
    forkAt(reader.currentAddr)

  def skipTree(tag: Int): Unit =
    if (tag >= firstLengthTreeTag) reader.goto(reader.readEnd())
    else if (tag >= firstNatASTTreeTag) { reader.readNat(); skipTree() }
    else if (tag >= firstASTTreeTag) skipTree()
    else if (tag >= firstNatTreeTag) reader.readNat()

  def skipTree(): Unit = skipTree(reader.readByte())

  def isSharedTag(tag: Int): Boolean = tag == SHAREDtype || tag == SHAREDterm

  /** The tag at the end of SHAREDtype/term chain */
  def tagFollowShared: Int = {
    val tag = reader.nextByte
    if (isSharedTag(tag)) {
      val lookAhead = fork
      // skip SHAREDtype / SHAREDterm tag, read the address
      lookAhead.reader.readByte()
      val addrShared = lookAhead.reader.readAddr()
      forkAt(addrShared).tagFollowShared
    } else {
      tag
    }
  }

  /** Performs the read action as if SHARED tags were transparent:
    *  - follows the SHARED tags to the term or type that is shared
    *  - reads the shared term or type with {@code read} action
    *  - restores the reader to seamlessly continue reading after the SHARED tag we started from
    */
  def readPotentiallyShared[T](read: => T): T =
    if (isSharedTag(reader.nextByte)) {
      reader.readByte()
      val addr = reader.readAddr()
      val returnTo = reader.currentAddr
      reader.goto(addr)
      val result = if (isSharedTag(reader.nextByte)) {
        readPotentiallyShared(read)
      } else {
        read
      }
      reader.goto(returnTo)
      result
    } else {
      read
    }

  def readName: TermName = nameAtRef(reader.readNameRef())

  def readTopLevelStat(using FileContext): Tree = reader.nextByte match {
    case PACKAGE =>
      reader.readByte()
      val packageEnd = reader.readEnd()
      val pid = readPotentiallyShared({
        assert(reader.readByte() == TERMREFpkg, posErrorMsg)
        // Symbol is already created during symbol creation phase
        ctx.getPackageSymbol(readName)
      })
      PackageDef(pid, reader.until(packageEnd)(readTopLevelStat(using ctx.withOwner(pid))))
    case _ => readStat
  }

  def readStats(end: Addr)(using FileContext): List[Tree] =
    reader.until(end)(readStat)

  def readStat(using FileContext): Tree = reader.nextByte match {
    case IMPORT | EXPORT =>
      def readSelector: ImportSelector = {
        assert(reader.nextByte == IMPORTED, posErrorMsg)
        reader.readByte()
        val name = Ident(readName)
        // IMPORTED can be followed by RENAMED or BOUNDED
        reader.nextByte match {
          case RENAMED =>
            reader.readByte()
            val renamed = Ident(readName)
            ImportSelector(name, renamed)
          case BOUNDED =>
            reader.readByte()
            val bound = readTypeTree
            ImportSelector(name, EmptyTree, bound)
          case _ => ImportSelector(name)
        }
      }
      val tag = reader.readByte()
      val end = reader.readEnd()
      val qual = readTerm
      val selectors = reader.until(end)(readSelector)
      if (tag == IMPORT) Import(qual, selectors) else Export(qual, selectors)
    case TYPEDEF =>
      val start = reader.currentAddr
      reader.readByte()
      val end = reader.readEnd()
      val name = readName.toTypeName
      val typedef: Class | TypeMember = if (reader.nextByte == TEMPLATE) {
        val classSymbol = ctx.getSymbol(start, ClassSymbolFactory)
        createSymbolTree(s => Class(name, readTemplate(using ctx.withOwner(s)), s), classSymbol)
      } else {
        val symbol = ctx.getSymbol(start, RegularSymbolFactory)
        if (tagFollowShared == TYPEBOUNDS)
          createSymbolTree(s => TypeMember(name, readTypeBounds(using ctx.withOwner(s)), s), symbol)
        else
          createSymbolTree(s => TypeMember(name, readTypeTree(using ctx.withOwner(s)), s), symbol)
      }
      // TODO: read modifiers
      skipModifiers(end)
      typedef
    case VALDEF | DEFDEF => readValOrDefDef
    case _               => readTerm
  }

  /** Reads type bounds for a synthetic typedef */
  def readTypeBounds(using FileContext): TypeBounds = {
    assert(tagFollowShared == TYPEBOUNDS, posErrorMsg)
    readPotentiallyShared({
      reader.readByte()
      val end = reader.readEnd()
      val low = readType
      if (reader.currentAddr != end && !isModifierTag(reader.nextByte)) {
        val high = readType
        // TODO: read variance (a modifier)
        skipModifiers(end)
        RealTypeBounds(low, high)
      } else {
        skipModifiers(end)
        TypeAlias(low)
      }
    })
  }

  def readTypeParams(using FileContext): List[TypeParam] = {
    def readTypeParamType(using FileContext): TypeBounds | TypeBoundsTree | TypeLambdaTree = {
      def readTypeBoundsType(using FileContext): TypeBounds = {
        assert(reader.readByte() == TYPEBOUNDS, posErrorMsg)
        val end = reader.readEnd()
        val low = readType
        // type bounds for type parameters have to be bounds, not an alias
        assert(reader.currentAddr != end && !isModifierTag(reader.nextByte), posErrorMsg)
        val high = readType
        // TODO: read variance (a modifier)
        skipModifiers(end)
        RealTypeBounds(low, high)
      }

      def readTypeBoundsTree(using FileContext): TypeBoundsTree = {
        assert(reader.readByte() == TYPEBOUNDStpt, posErrorMsg)
        val end = reader.readEnd()
        val low = readTypeTree
        val high = reader.ifBefore(end)(readTypeTree, EmptyTypeTree)
        // assert atEnd: no alias for type parameters
        assert(reader.currentAddr == end, posErrorMsg)
        TypeBoundsTree(low, high)
      }

      def readLambdaTpt(using FileContext): TypeLambdaTree = {
        assert(reader.nextByte == LAMBDAtpt, posErrorMsg)
        readTypeTree.asInstanceOf[TypeLambdaTree]
      }

      readPotentiallyShared(
        if (tagFollowShared == TYPEBOUNDS)
          readTypeBoundsType
        else if (tagFollowShared == TYPEBOUNDStpt)
          readTypeBoundsTree
        else
          readLambdaTpt
      )
    }

    def readTypeParam: TypeParam = {
      val start = reader.currentAddr
      val paramSymbol = ctx.getSymbol(start, RegularSymbolFactory)
      reader.readByte()
      val end = reader.readEnd()
      val name = readName.toTypeName
      val bounds = readTypeParamType(using ctx.withOwner(paramSymbol))
      skipModifiers(end)
      createSymbolTree(symbol => TypeParam(name, bounds, symbol), paramSymbol)
    }
    var acc = new ListBuffer[TypeParam]()
    while (reader.nextByte == TYPEPARAM) {
      acc += readTypeParam
    }
    acc.toList
  }

  /** Reads TYPEBOUNDStpt of a typedef */
  def readBoundedTypeTree(using FileContext): BoundedTypeTree = {
    assert(reader.readByte() == TYPEBOUNDStpt, posErrorMsg)
    val end = reader.readEnd()
    val low = readTypeTree
    val high = reader.ifBefore(end)(readTypeTree, EmptyTypeTree)
    val alias = reader.ifBefore(end)(readTypeTree, EmptyTypeTree)
    BoundedTypeTree(TypeBoundsTree(low, high), alias)
  }

  def readTypeBoundsTree(using FileContext): TypeBoundsTree = {
    assert(tagFollowShared == TYPEBOUNDStpt, posErrorMsg)
    readPotentiallyShared({
      reader.readByte()
      val end = reader.readEnd()
      val low = readTypeTree
      val high = reader.ifBefore(end)(readTypeTree, EmptyTypeTree)
      TypeBoundsTree(low, high)
    })
  }

  // TODO: classinfo of the owner
  def readTemplate(using FileContext): Template = {
    reader.readByte()
    val end = reader.readEnd()
    val tparams = readTypeParams
    val params = readParams
    val parents: List[Apply | Block | TypeTree] =
      reader.collectWhile(reader.nextByte != SELFDEF && reader.nextByte != DEFDEF) {
        reader.nextByte match {
          // class parents of classes and trait parents with arguments are APPLYs, because they specify the constructor
          // BLOCK when the parent constructor has default parameters and the call specifies only some of them
          case APPLY => readTerm.asInstanceOf[Apply]
          case BLOCK => readTerm.asInstanceOf[Block]
          case _     => readTypeTree
        }
      }
    val self = readSelf
    // The first entry is the constructor
    val cstr = readStat.asInstanceOf[DefDef]
    val body = readStats(end)
    Template(cstr, parents, self, tparams ++ params ++ body)
  }

  def readAllParams(using FileContext): List[ParamsClause] =
    reader.nextByte match {
      case PARAM => readParams :: readAllParams
      case EMPTYCLAUSE =>
        reader.readByte()
        Nil :: readAllParams
      case TYPEPARAM => readTypeParams :: readAllParams
      case _         => Nil
    }

  def readParamLists(using FileContext): List[List[ValDef]] = {
    var acc = new ListBuffer[List[ValDef]]()
    while (reader.nextByte == PARAM || reader.nextByte == EMPTYCLAUSE) {
      reader.nextByte match {
        case PARAM => acc += readParams
        case EMPTYCLAUSE =>
          reader.readByte()
          acc += Nil
      }
    }
    acc.toList
  }

  def readParams(using FileContext): List[ValDef] = {
    var acc = new ListBuffer[ValDef]()
    while (reader.nextByte == PARAM) {
      acc += readValOrDefDef.asInstanceOf[ValDef]
    }
    if (reader.nextByte == SPLITCLAUSE) {
      reader.readByte()
    }
    acc.toList
  }

  def readSelf(using FileContext): ValDef =
    if (reader.nextByte != SELFDEF) {
      EmptyValDef
    } else {
      reader.readByte()
      val name = readName
      val tpt = readTypeTree
      // no symbol for self, because it's never referred to by symbol
      ValDef(name, tpt, EmptyTree, NoSymbol)
    }

  def readValOrDefDef(using FileContext): Tree = {
    val start = reader.currentAddr
    val tag = reader.readByte()
    val end = reader.readEnd()
    val name = readName
    // Only for DefDef, but reading works for empty lists
    val symbol = ctx.getSymbol(start, RegularSymbolFactory)
    val insideCtx = ctx.withOwner(symbol)
    val params = readAllParams(using insideCtx)
    val tpt = readTypeTree(using insideCtx)
    val rhs =
      if (reader.currentAddr == end || isModifierTag(reader.nextByte)) EmptyTree
      else readTerm(using insideCtx)
    skipModifiers(end)
    tag match {
      case VALDEF | PARAM =>
        createSymbolTree(s => ValDef(name, tpt, rhs, s), symbol)
      case DEFDEF =>
        createSymbolTree(s => DefDef(name, params, tpt, rhs, s), symbol)
    }
  }

  def readTerms(end: Addr)(using FileContext): List[Tree] =
    reader.until(end)(readTerm)

  def createSymbolTree[S <: Symbol, T <: Tree](createTree: S => T, symbol: S): T = {
    val tree = createTree(symbol)
    symbol.withTree(tree)
    tree
  }

  def readTerm(using FileContext): Tree = reader.nextByte match {
    case IDENT =>
      reader.readByte()
      val name = readName
      val typ = readType
      // TODO: assign type
      Ident(name)
    case APPLY =>
      reader.readByte()
      val end = reader.readEnd()
      val fn = readTerm
      val args = readTerms(end)
      Apply(fn, args)
    case NAMEDARG =>
      reader.readByte()
      NamedArg(readName, readTerm)
    case TYPEAPPLY =>
      reader.readByte()
      val end = reader.readEnd()
      val fn = readTerm
      TypeApply(fn, reader.until(end)(readTypeTree))
    case SELECT =>
      reader.readByte()
      val name = readName
      val qual = readTerm
      Select(qual, name)
    case QUALTHIS =>
      reader.readByte()
      val qualifier = readTypeTree.asInstanceOf[TypeIdent]
      This(Some(qualifier))
    case SUPER =>
      reader.readByte()
      val end = reader.readEnd()
      val qual = readTerm
      val mixin = reader.ifBefore(end)(Some(readTypeTree.asInstanceOf[TypeIdent]), None)
      Super(qual, mixin)
    case SELECTin =>
      reader.readByte()
      val end = reader.readEnd()
      val name = readName
      val qual = readTerm
      // TODO: use owner
      val owner = readType
      Select(qual, name)
    case NEW =>
      reader.readByte()
      val cls = readTypeTree
      New(cls)
    case TYPED =>
      reader.readByte()
      reader.readEnd()
      Typed(readTerm, readTypeTree)
    case THROW =>
      reader.readByte()
      val thrown = readTerm
      Throw(thrown)
    case TRY =>
      reader.readByte()
      val end = reader.readEnd()
      val expr = readTerm
      val catchCases = readCases[CaseDef](CaseDefFactory, end)
      val finalizer = reader.ifBefore(end)(readTerm, EmptyTree)
      Try(expr, catchCases, finalizer)
    case ASSIGN =>
      reader.readByte()
      reader.readEnd()
      Assign(readTerm, readTerm)
    case BLOCK =>
      reader.readByte()
      val end = reader.readEnd()
      val expr = readTerm
      Block(readStats(end), expr)
    case IF =>
      reader.readByte()
      reader.readEnd()
      if (reader.nextByte == INLINE) {
        reader.readByte()
        new InlineIf(readTerm, readTerm, readTerm)
      } else {
        If(readTerm, readTerm, readTerm)
      }
    case LAMBDA =>
      reader.readByte()
      val end = reader.readEnd()
      val method = readTerm
      val tpt = reader.ifBefore(end)(readTypeTree, EmptyTypeTree)
      Lambda(method, tpt)
    case MATCH =>
      reader.readByte()
      val end = reader.readEnd()
      if (reader.nextByte == IMPLICIT) {
        reader.readByte()
        new InlineMatch(EmptyTree, readCases[CaseDef](CaseDefFactory, end))
      } else if (reader.nextByte == INLINE) {
        reader.readByte()
        new InlineMatch(readTerm, readCases[CaseDef](CaseDefFactory, end))
      } else Match(readTerm, readCases[CaseDef](CaseDefFactory, end))
    case BIND =>
      val start = reader.currentAddr
      reader.readByte()
      val end = reader.readEnd()
      val name = readName
      // TODO: use type
      val typ = readType
      val term = readTerm
      skipModifiers(end)
      createSymbolTree(bindSymbol => Bind(name, term, bindSymbol), ctx.getSymbol(start, RegularSymbolFactory))
    case ALTERNATIVE =>
      reader.readByte()
      val end = reader.readEnd()
      Alternative(reader.until(end)(readTerm))
    case UNAPPLY =>
      reader.readByte()
      val end = reader.readEnd()
      val fun = readTerm
      val args = reader.collectWhile(reader.nextByte == IMPLICITarg)({
        assert(reader.readByte() == IMPLICITarg, posErrorMsg)
        readTerm
      })
      // TODO: use pattern type
      val patType = readType
      val patterns = reader.until(end)(readTerm)
      Unapply(fun, args, patterns)
    case REPEATED =>
      reader.readByte()
      val end = reader.readEnd()
      val elemType = readTypeTree
      SeqLiteral(reader.until(end)(readTerm), elemType)
    case WHILE =>
      reader.readByte()
      reader.readEnd()
      While(readTerm, readTerm)
    case RETURN =>
      reader.readByte()
      val end = reader.readEnd()
      val from = readSymRef
      val expr = reader.ifBefore(end)(readTerm, EmptyTree)
      // TODO: always just taking the name?
      // return always returns from a method, i.e. something with a TermName
      Return(expr, Ident(from.name.asInstanceOf[TermName]))
    case INLINED =>
      reader.readByte()
      val end = reader.readEnd()
      val expr = readTerm
      val caller: TypeIdent =
        reader.ifBefore(end)(
          tagFollowShared match {
            // The caller is not specified, this is a binding (or next val or def)
            case VALDEF | DEFDEF => EmptyTypeIdent
            case _               => readTypeTree.asInstanceOf[TypeIdent]
          },
          EmptyTypeIdent
        )
      val bindings = reader.until(end)(readValOrDefDef)
      Inlined(expr, caller, bindings)
    case SHAREDterm =>
      reader.readByte()
      forkAt(reader.readAddr()).readTerm

    // paths
    case THIS =>
      reader.readByte()
      val typ = readType
      // TODO: assign type
      This(None)
    case TERMREF =>
      reader.readByte()
      val name = readName
      val typ = readType
      // TODO: use type
      // TODO: this might be more complicated than Ident
      Ident(name)
    case TERMREFpkg =>
      reader.readByte()
      val name = readName
      // TODO: create a termref and store as a tpe
      new ReferencedPackage(name)
    case TERMREFdirect =>
      reader.readByte()
      val sym = readSymRef
      // TODO: assign type
      assert(sym.name.isInstanceOf[TermName], posErrorMsg)
      Ident(sym.name.asInstanceOf[TermName])
    // TODO: is it always Ident?
    case TERMREFsymbol =>
      reader.readByte()
      val sym = readSymRef
      val typ = readType
      assert(sym.name.isInstanceOf[TermName], posErrorMsg)
      // TODO: assign type
      Ident(sym.name.asInstanceOf[TermName])
    case SHAREDtype =>
      reader.readByte()
      forkAt(reader.readAddr()).readTerm
    case tag if isConstantTag(tag) =>
      Literal(readConstant)
    case tag =>
      throw TreeUnpicklerException(s"Unexpected term tag ${astTagToString(tag)} $posErrorMsg")
  }

  /** The next tag, following through SHARED tags */
  def nextUnsharedTag: Int = {
    val tag = reader.nextByte
    if (tag == SHAREDtype || tag == SHAREDterm) {
      val lookAhead = fork
      lookAhead.reader.readByte()
      forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
    } else tag
  }

  def readCases[T <: CaseDef | TypeCaseDef](factory: AbstractCaseDefFactory[T], end: Addr)(using FileContext): List[T] =
    reader.collectWhile((nextUnsharedTag == CASEDEF) && reader.currentAddr != end) {
      if (reader.nextByte == SHAREDterm) {
        // skip the SHAREDterm tag
        reader.readByte()
        // TODO: changes in the context?
        forkAt(reader.readAddr()).readCaseDef[T](factory)
      }
      // TODO: changes in the context?
      else readCaseDef[T](factory)
    }

  def readCaseDef[T <: CaseDef | TypeCaseDef](factory: AbstractCaseDefFactory[T])(using FileContext): T = {
    assert(reader.readByte() == CASEDEF, posErrorMsg)
    val end = reader.readEnd()
    factory match {
      case CaseDefFactory =>
        val pattern = readTerm
        val body = readTerm
        CaseDef(pattern, reader.ifBefore(end)(readTerm, EmptyTree), body)
      case TypeCaseDefFactory =>
        TypeCaseDef(readTypeTree, readTypeTree)
    }
  }

  def readSymRef(using FileContext): Symbol = {
    val symAddr = reader.readAddr()
    assert(ctx.hasSymbolAt(symAddr), posErrorMsg)
    ctx.getSymbol(symAddr)
  }

  def readType(using FileContext): Type = reader.nextByte match {
    case TYPEREF =>
      reader.readByte()
      val name = readName.toTypeName
      TypeRef(readType, name)
    case TYPEREFin =>
      reader.readByte()
      reader.readEnd()
      val name = readName.toTypeName
      val prefix = readType
      // TODO: use the namespace
      val namespace = readType
      TypeRef(prefix, name)
    case TYPEREFdirect =>
      reader.readByte()
      val sym = readSymRef
      TypeRef(NoPrefix, sym)
    case TYPEREFsymbol =>
      reader.readByte()
      val sym = readSymRef
      TypeRef(readType, sym)
    case TYPEREFpkg =>
      reader.readByte()
      PackageTypeRef(readName)
    case SHAREDtype =>
      reader.readByte()
      forkAt(reader.readAddr()).readType
    case TERMREFdirect =>
      reader.readByte()
      val sym = readSymRef
      TermRef(NoPrefix, sym)
    case TERMREFsymbol =>
      reader.readByte()
      val sym = readSymRef
      TermRef(readType, sym)
    case TERMREFpkg =>
      reader.readByte()
      val name = readName
      new PackageRef(name)
    case TERMREF =>
      reader.readByte()
      val name = readName
      TermRef(readType, name)
    case APPLIEDtype =>
      reader.readByte()
      val end = reader.readEnd()
      val tycon = readType
      // TODO: type operations can be much more complicated
      AppliedType(tycon, reader.until(end)(if (tagFollowShared == TYPEBOUNDS) readTypeBounds else readType))
    case THIS =>
      reader.readByte()
      ThisType(readType.asInstanceOf[TypeRef])
    case QUALTHIS =>
      reader.readByte()
      if (tagFollowShared != IDENTtpt) {
        throw TreeUnpicklerException(s"Unexpected tag after QUALTHIS: ${astTagToString(tagFollowShared)} $posErrorMsg")
      }
      // Skip IDENTtpt tag and name
      reader.readByte()
      readName
      // Type of QUALTHIS is ThisType for the type reference, which is the type of the IDENTtpt
      ThisType(readType.asInstanceOf[TypeRef])
    case ANNOTATEDtype =>
      reader.readByte()
      reader.readEnd()
      val typ = readType
      val annot = readTerm
      AnnotatedType(typ, annot)
    case ANDtype =>
      reader.readByte()
      reader.readEnd()
      AndType(readType, readType)
    case ORtype =>
      reader.readByte()
      reader.readEnd()
      OrType(readType, readType)
    case BYNAMEtype =>
      reader.readByte()
      ExprType(readType)
    case TYPELAMBDAtype =>
      val lambdaAddr = reader.currentAddr
      reader.readByte()
      val end = reader.readEnd()
      val resultUnpickler = fork
      // skip the result type: it might refer to the parameters, which we haven't read yet
      skipTree()
      val params = reader.until(end)({
        val bounds = readTypeBounds
        val name = readName.toTypeName
        // cannot have symbols inside types
        TypeParam(name, bounds, NoSymbol)
      })
      TypeLambda(params, (tl: TypeLambda) => resultUnpickler.readType(using ctx.withEnclosingLambda(lambdaAddr, tl)))
    case PARAMtype =>
      reader.readByte()
      reader.readEnd()
      val lambdaAddr = reader.readAddr()
      val num = reader.readNat()
      TypeParamRef(ctx.getEnclosingLambda(lambdaAddr), num)
    case REFINEDtype =>
      reader.readByte()
      reader.readEnd()
      // TODO: support term refinements, then the name won't always be a type name.
      val refinementName = readName.toTypeName
      val underlying = readType
      if (tagFollowShared != TYPEBOUNDS) {
        throw TreeUnpicklerException(s"Only type refinements are supported. Term refinement $posErrorMsg")
      }
      RefinedType(underlying, refinementName, readTypeBounds)
    case tag if (isConstantTag(tag)) =>
      ConstantType(readConstant)
    case tag =>
      throw TreeUnpicklerException(s"Unexpected type tag ${astTagToString(tag)} $posErrorMsg")
  }

  def readTypeTree(using FileContext): TypeTree = reader.nextByte match {
    case IDENTtpt =>
      reader.readByte()
      val typeName = readName.toTypeName
      val typ = readType
      TypeIdent(typeName).withType(typ)
    case SINGLETONtpt =>
      reader.readByte()
      SingletonTypeTree(readTerm)
    case REFINEDtpt =>
      reader.readByte()
      val end = reader.readEnd()
      RefinedTypeTree(readTypeTree, readStats(end))
    case APPLIEDtpt =>
      reader.readByte()
      val end = reader.readEnd()
      val tycon = readTypeTree
      AppliedTypeTree(
        tycon,
        reader.until(end)(
          if (tagFollowShared == TYPEBOUNDS) readTypeBounds
          else if (tagFollowShared == TYPEBOUNDStpt) readTypeBoundsTree
          else readTypeTree
        )
      )
    case LAMBDAtpt =>
      reader.readByte()
      reader.readEnd()
      TypeLambdaTree(readTypeParams, readTypeTree)
    // select type from a term
    case SELECT =>
      reader.readByte()
      val name = readName.toTypeName
      val qual = readTerm
      SelectTypeFromTerm(qual, name)
    case SELECTtpt =>
      reader.readByte()
      val name = readName.toTypeName
      SelectTypeTree(readTypeTree, name)
    case ANNOTATEDtpt =>
      reader.readByte()
      reader.readEnd()
      AnnotatedTypeTree(readTypeTree, readTerm)
    case MATCHtpt =>
      reader.readByte()
      val end = reader.readEnd()
      val selOrBound = readTypeTree
      val (bound, selector) =
        if (tagFollowShared == CASEDEF)
          (EmptyTypeTree, selOrBound)
        else (selOrBound, readTypeTree)
      MatchTypeTree(bound, selector, readCases[TypeCaseDef](TypeCaseDefFactory, end))
    // TODO: why in TYPEAPPLY?
    // in MATCHtpt, TYPEAPPLY
    case BIND =>
      val start = reader.currentAddr
      reader.readByte()
      val end = reader.readEnd()
      val name = readName.toTypeName
      // TODO: use type bounds
      val typ = readTypeBounds
      /* This is a workaround: consider a BIND inside a MATCHtpt
       * example: case List[t] => t
       * Such a bind has IDENT(_) as its body, which is not a type tree and therefore not expected.
       * Treat it as if it were an IDENTtpt. */
      val body: TypeTree = if (reader.nextByte == IDENT) {
        reader.readByte()
        val typeName = readName.toTypeName
        val typ = readTypeBounds
        // TODO: assign type
        TypeIdent(typeName)
      } else readTypeTree
      skipModifiers(end)
      TypeTreeBind(name, body, ctx.getSymbol(start, RegularSymbolFactory))
    // Type tree for a type member (abstract or bounded opaque)
    case TYPEBOUNDStpt => readBoundedTypeTree
    case BYNAMEtpt =>
      reader.readByte()
      ByNameTypeTree(readTypeTree)
    case SHAREDterm =>
      reader.readByte()
      forkAt(reader.readAddr()).readTypeTree
    case tag if isTypeTreeTag(tag) =>
      throw TreeUnpicklerException(s"Unexpected type tree tag ${astTagToString(tag)} $posErrorMsg")
    case _ => TypeWrapper(readType)
  }

  def readConstant(using FileContext): Constant = reader.readByte() match {
    case UNITconst =>
      Constant(())
    case TRUEconst =>
      Constant(true)
    case FALSEconst =>
      Constant(false)
    case BYTEconst =>
      Constant(reader.readInt().toByte)
    case SHORTconst =>
      Constant(reader.readInt().toShort)
    case CHARconst =>
      Constant(reader.readNat().toChar)
    case INTconst =>
      Constant(reader.readInt())
    case LONGconst =>
      Constant(reader.readLongInt())
    case FLOATconst =>
      Constant(java.lang.Float.intBitsToFloat(reader.readInt()))
    case DOUBLEconst =>
      Constant(java.lang.Double.longBitsToDouble(reader.readLongInt()))
    case STRINGconst =>
      Constant(readName.toString)
    case NULLconst =>
      Constant(null)
    case CLASSconst =>
      Constant(readType)
  }

  // TODO: read modifiers and return them instead
  def skipModifiers(end: Addr)(using FileContext): Unit = {
    def skipModifier(): Unit = reader.readByte() match {
      case PRIVATEqualified =>
        readType
        ()
      case PROTECTEDqualified =>
        readType
        ()
      case ANNOTATION =>
        val end = reader.readEnd()
        reader.goto(end)
      case _ => ()
    }
    while (reader.currentAddr != end && isModifierTag(reader.nextByte)) {
      skipModifier()
    }
  }

  private def isConstantTag(tag: Int): Boolean =
    tag match {
      case UNITconst | FALSEconst | TRUEconst | BYTEconst | SHORTconst | CHARconst | INTconst | LONGconst | FLOATconst |
          DOUBLEconst | STRINGconst | NULLconst | CLASSconst =>
        true
      case _ => false
    }
}
