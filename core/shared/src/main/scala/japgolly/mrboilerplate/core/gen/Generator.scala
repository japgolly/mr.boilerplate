package japgolly.mrboilerplate.core.gen

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.mrboilerplate.core.data._
import japgolly.microlibs.stdlib_ext.StdlibExt._

trait Generator { self =>
  type Options

  val title: String

  def initStatements(data: Iterable[TypeDef], opt: Options)(implicit glopt: GlobalOptions): List[String] =
    Nil

  def gen(opt: Options)(implicit glopt: GlobalOptions): TypeDef => List[String]

  final type AndOptions = Generator.AndOptions { val gen: self.type }

  final def andOptions(o: Options): AndOptions =
    new Generator.AndOptions {
      override val gen: self.type = self
      override val options = o
    }

}

object Generator {

  sealed trait AndOptions {
    val gen: Generator
    val options: gen.Options

    final def unify(g: Generator): Option[g.AndOptions] =
      Option.when(g eq gen)(this.asInstanceOf[g.AndOptions])
  }

  final def apply(gens: Iterable[AndOptions], data: Iterable[TypeDef])(implicit go: GlobalOptions): String = {

    val preparedGens: List[TypeDef => List[String]] =
      gens.iterator.map(g => g.gen.gen(g.options)).toList

    def gen(td: TypeDef): Iterator[String] =
      preparedGens.iterator.flatMap(_(td))

    val header: String = {
      val (imports, other) =
        gens.iterator
          .flatMap(g => g.gen.initStatements(data, g.options))
          .partition(_.startsWith("import "))

      val sortedImports =
        MutableArray(imports.flatMap(_.split("\n")).map(_.trim).filter(_.nonEmpty))
          .sort
          .iterator()
          .mkString("\n")

      (Iterator.single(sortedImports) ++ other).mkString("\n\n")
    }

    val body: String =
      if (go.generateCompanions) {
        // Order doesn't matter
        data.iterator.flatMap { td =>
          val decls = gen(td).mkString("\n\n")
          if (decls.nonEmpty)
            s"""
               |object ${td.name} {
               |${decls.indentLines(2)}
               |}
                 """.stripMargin.trim :: Nil
          else
            Nil
        }.mkString("\n\n")


      } else if (go.makeValsLazy) {
        // Order doesn't matter
        data.iterator.flatMap(gen).mkString("\n\n")

      } else {
        // Order matters; generate dependants first
        val r1 = List.newBuilder[String]
        val r2 = List.newBuilder[String]

        data.foreach  {
          case c: Cls        => r1 ++= gen(c)
          case o: Obj        => r1 ++= gen(o)
          case s: SealedBase => r2 ++= gen(s) // needs to be last
        }

        r1 ++= r2.result()
        r1.result().mkString("\n\n")
      }

    val headerAndBody =
      if (header.isEmpty)
        body
      else
        header + seperator + body

    headerAndBody + "\n"
  }

  private val seperator = "\n\n"
//  private val seperator = "\n\n// " + ("="*97) + "\n\n"
}
