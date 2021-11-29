package tastyquery.api

import org.apache.commons.io.{FileUtils, IOUtils}
import tastyquery.Contexts
import tastyquery.ast.Trees.Tree

import tastyquery.reader
import tastyquery.reader.{TastyUnpickler, TastyFile, TreeUnpickler}

import java.io.{File, InputStream}
import java.nio.file.{Files, Paths}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import dotty.tools.tasty.UnpickleException

class ProjectReader {
  def read(classpath: List[String]): TastyQuery = {
    val unpicklingCtx = Contexts.empty
    val trees = {
      classpathToEntries(classpath).flatMap { entry =>
        val (errors, trees) = entry.walkTastyFiles((filename, stream) =>
          getTreeUnpickler(stream).map(_.unpickle(using unpicklingCtx.withFile(filename)))
        )
        // TODO: report errors
        trees
      }
    }
    TastyQuery(unpicklingCtx, TastyTrees(trees))
  }

  private def getTreeUnpickler(fileStream: InputStream): reader.Result[TreeUnpickler] = {
    val tasty = IArray.unsafeFromArray(IOUtils.toByteArray(fileStream))
    for {
      given TastyFile <- TastyUnpickler.unpickleFile(tasty)
      treeunpickler <- TastyUnpickler.unpickleTrees
    } yield treeunpickler
  }

  private def classpathToEntries(classpath: List[String]): List[ClasspathEntry] =
    classpath.map(e =>
      if (Files.exists(Paths.get(e))) {
        if (e.endsWith(".jar")) Jar(e)
        else if (Files.isDirectory(Paths.get(e))) Directory(e)
        else InvalidEntry(e)
      } else {
        InvalidEntry(e)
      }
    )
}

sealed abstract class ClasspathEntry(val path: String) {
  def walkTastyFiles[E](op: (String, InputStream) => Either[E, List[Tree]]): (List[E], List[Tree])
}

final case class Jar(override val path: String) extends ClasspathEntry(path) {
  def getFullPath(filename: String): String = s"$path:$filename"

  override def walkTastyFiles[E](op: (String, InputStream) => Either[E, List[Tree]]): (List[E], List[Tree]) = {
    val jar = JarFile(path)
    val (errors, treess) = jar
      .stream()
      .toScala(List) // force the execution of filter + map on the stream:
      .filter(_.getName().endsWith(".tasty"))
      .partitionMap(je => op(getFullPath(je.getName()), jar.getInputStream(je)))
    (errors, treess.flatten)
  }
}

final case class Directory(override val path: String) extends ClasspathEntry(path) {
  override def walkTastyFiles[E](op: (String, InputStream) => Either[E, List[Tree]]): (List[E], List[Tree]) = {
    val (errors, treess) = FileUtils
      .listFiles(new File(path), Array("tasty"), true)
      .asScala
      .toList
      .partitionMap(f => op(f.getAbsolutePath(), FileUtils.openInputStream(f)))
    (errors, treess.flatten)
  }
}

final case class InvalidEntry(override val path: String) extends ClasspathEntry(path) {
  override def walkTastyFiles[E](op: (String, InputStream) => Either[E, List[Tree]]): (List[E], List[Tree]) =
    (Nil, Nil)
}
