package tastyquery

import tastyquery.Contexts.*
import tastyquery.ast.Flags.*
import tastyquery.ast.Names.*
import tastyquery.ast.Symbols.*
import tastyquery.ast.Types.*

final class Definitions private[tastyquery] (
  ctx: Context,
  rootPackage: PackageClassSymbol,
  emptyPackage: PackageClassSymbol
):
  private given Context = ctx

  // Core packages

  val RootPackage = rootPackage
  val EmptyPackage = emptyPackage

  val scalaPackage = ctx.createPackageSymbolIfNew(nme.scalaPackageName, RootPackage)
  val javaLangPackage = ctx.createPackageSymbolIfNew(nme.javalangPackageName, RootPackage)

  // Magic symbols that are not found on the classpath, but rather created by hand

  private def markInitialised(cls: ClassSymbol): cls.type =
    cls.initialised = true
    cls

  val AnyClass = markInitialised(ctx.createClassSymbol(typeName("Any"), scalaPackage))

  val NullClass = markInitialised(ctx.createClassSymbol(typeName("Null"), scalaPackage))

  val NothingClass = markInitialised(ctx.createClassSymbol(typeName("Nothing"), scalaPackage))

  val NothingAnyBounds = RealTypeBounds(NothingClass.typeRef, AnyClass.typeRef)

  locally {
    val andOrParamNames = List(typeName("A"), typeName("B"))

    val andTypeAlias = ctx.createSymbol(typeName("&"), scalaPackage)
    andTypeAlias.withFlags(EmptyFlagSet)
    andTypeAlias.withDeclaredType(
      PolyType(andOrParamNames)(
        pt => List(NothingAnyBounds, NothingAnyBounds),
        pt => AndType(pt.paramRefs(0), pt.paramRefs(1))
      )
    )

    val orTypeAlias = ctx.createSymbol(typeName("|"), scalaPackage)
    orTypeAlias.withFlags(EmptyFlagSet)
    orTypeAlias.withDeclaredType(
      PolyType(andOrParamNames)(
        pt => List(NothingAnyBounds, NothingAnyBounds),
        pt => OrType(pt.paramRefs(0), pt.paramRefs(1))
      )
    )

    val AnyRefAlias = ctx.createSymbol(typeName("AnyRef"), scalaPackage)
    AnyRefAlias.withFlags(EmptyFlagSet)
    val ObjectType = TypeRef(javaLangPackage.accessibleThisType, typeName("Object"))
    AnyRefAlias.withDeclaredType(BoundedType(TypeAlias(ObjectType), NoType))
  }

  private def createSpecialPolyClass(
    name: TypeName,
    paramFlags: FlagSet,
    parentConstrs: Type => Seq[Type]
  ): ClassSymbol =
    val cls = ctx.createClassSymbol(name, scalaPackage)

    val tparam = ctx.createSymbol(typeName("T"), cls)
    tparam.withFlags(ClassTypeParam)
    tparam.withDeclaredType(WildcardTypeBounds(NothingAnyBounds))

    cls.withTypeParams(tparam :: Nil, NothingAnyBounds :: Nil)
    cls.withFlags(EmptyFlagSet | Artifact)

    val parents = parentConstrs(TypeRef(NoPrefix, tparam))
    // TODO Set parents in cls

    cls
  end createSpecialPolyClass

  val ByNameParamClass2x: ClassSymbol =
    createSpecialPolyClass(tpnme.ByNameParamClassMagic, Covariant, _ => Seq(AnyType))

  val RepeatedParamClass: ClassSymbol =
    createSpecialPolyClass(tpnme.RepeatedParamClassMagic, Covariant, tp => Seq(ObjectType, SeqTypeOf(tp)))

  // Derived symbols, found on the classpath

  extension (pkg: PackageClassSymbol)
    private def requiredClass(name: String): ClassSymbol = pkg.getDecl(typeName(name)).get.asClass

  private lazy val scalaCollectionPackage = scalaPackage.getPackageDecl(termName("collection")).get

  lazy val ObjectClass = javaLangPackage.requiredClass("Object")

  lazy val AnyValClass = scalaPackage.requiredClass("AnyVal")
  lazy val ArrayClass = scalaPackage.requiredClass("Array")
  lazy val SeqClass = scalaCollectionPackage.requiredClass("Seq")
  lazy val Function0Class = scalaPackage.requiredClass("Function0")

end Definitions
