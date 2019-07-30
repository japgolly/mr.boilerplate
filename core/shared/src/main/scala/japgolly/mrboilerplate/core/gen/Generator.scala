package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import japgolly.microlibs.stdlib_ext.StdlibExt._

trait Generator { self =>
  type Options

  val title: String

  def helperFns(data: Traversable[TypeDef], opt: Options, glopt: GlobalOptions): List[String] =
    Nil

  def gen(opt: Options, glopt: GlobalOptions): TypeDef => List[String]

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

  final def apply(gens: Traversable[AndOptions], data: Traversable[TypeDef], go: GlobalOptions): String = {

    val preparedGens: List[TypeDef => List[String]] =
      gens.toIterator.map(g => g.gen.gen(g.options, go)).toList

    def gen(td: TypeDef): Iterator[String] =
      preparedGens.toIterator.flatMap(_(td))

    val body: String =
      if (go.generateCompanions) {
        // Order doesn't matter
        data.toIterator.flatMap { td =>
          val decls = gen(td).mkString("\n\n")
          if (decls.nonEmpty)
            s"""
               |object ${td.name} {
               |${decls.indent(2)}
               |}
                 """.stripMargin.trim :: Nil
          else
            Nil
        }.mkString("\n\n")


      } else {
        // Order matters; generate dependants first
        val r1 = List.newBuilder[String]
        val r2 = List.newBuilder[String]

        data.foreach  {
          case c: Cls        => r1 ++= gen(c)
          case s: SealedBase => r2 ++= gen(s) // needs to be last
        }

        r1 ++= r2.result()
        r1.result().mkString("\n\n")
      }

    val header = gens.toIterator.flatMap(g => g.gen.helperFns(data, g.options, go)).mkString("\n\n")

    if (header.isEmpty)
      body
    else
      header + seperator + body
  }

  private val seperator = "\n\n// " + ("="*97) + "\n\n"
}