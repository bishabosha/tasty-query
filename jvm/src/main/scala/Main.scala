import java.nio.file.{Files, Paths}
import tastyquery.reader.{TastyUnpickler, TastyFile}
import tastyquery.Contexts
import tastyquery.api.ProjectReader
import tastyquery.ast.Names.TermName

import java.net.URL
import java.net.URLClassLoader

object Main {
  def main(args: Array[String]): Unit =
    args.toList match {
      case Nil =>
        // By default, read this project
        val cl = ClassLoader.getSystemClassLoader
        val classpath = cl.asInstanceOf[URLClassLoader].getURLs.map(_.getFile).toList
        val reader = new ProjectReader
        reader.read(classpath)
      case "-cp" :: classpath =>
        val reader = new ProjectReader
        reader.read(classpath)
      case "--standalone" :: filename :: Nil =>
        val tasty = IArray.unsafeFromArray(Files.readAllBytes(Paths.get(filename)))
        val trees =
          for
            given TastyFile <- TastyUnpickler.unpickleFile(tasty)
            treeunpickler <- TastyUnpickler.unpickleTrees
          yield treeunpickler.unpickle(using Contexts.empty(filename))
        println(trees.toTry.get)
      case _ =>
        println("""Two modes of usage:
                  |--standalone tasty-file
                  |-cp whitespace-separated-classpath-entries
                  |By default, will read the current project.""".stripMargin)
    }
}
