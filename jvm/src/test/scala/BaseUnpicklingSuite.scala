import tastyquery.Contexts
import tastyquery.Contexts.FileContext
import tastyquery.ast.Trees.Tree
import tastyquery.ast.Types.Type
import tastyquery.reader.{TastyUnpickler, TastyFile}

import java.nio.file.{Files, Paths}

abstract class BaseUnpicklingSuite extends munit.FunSuite {
  val ResourceProperty = "test-resources"

  def unpickle(filename: String)(using ctx: FileContext = Contexts.empty(filename)): Tree = {
    val resourcePath = getResourcePath(filename)
    val tasty = IArray.unsafeFromArray(Files.readAllBytes(Paths.get(resourcePath)))
    val result =
      for
        given TastyFile <- TastyUnpickler.unpickleFile(tasty)
        unpickler <- TastyUnpickler.unpickleTrees
      yield unpickler.unpickle(using ctx).head
    result.toTry.get
  }

  def getResourcePath(name: String): String =
    s"${System.getProperty(ResourceProperty)}/$name.tasty"
}
