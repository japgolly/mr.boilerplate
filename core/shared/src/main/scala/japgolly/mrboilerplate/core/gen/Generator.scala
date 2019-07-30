package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import japgolly.microlibs.stdlib_ext.StdlibExt._

trait Generator { self =>
  type Options

  val title: String

  def genCls(cls: Cls, opt: Options, glopt: GlobalOptions): List[String]
  def genSB(sb: SealedBase, opt: Options, glopt: GlobalOptions): List[String]

  final def gen(t: TypeDef, opt: Options, glopt: GlobalOptions): List[String] =
    t match {
      case c: Cls        => genCls(c, opt, glopt)
      case s: SealedBase => genSB(s, opt, glopt)
    }

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

  final def apply(gens: Traversable[AndOptions], data: TraversableOnce[TypeDef], go: GlobalOptions): String = {

    def gen(td: TypeDef): Iterator[String] =
      gens.toIterator.flatMap(g => g.gen.gen(td, g.options, go))

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
        case s: SealedBase => r1 ++= gen(s)
        case c: Cls        => r2 ++= gen(c)
      }

      r1 ++= r2.result()
      r1.result().mkString("\n\n")
    }
  }

}