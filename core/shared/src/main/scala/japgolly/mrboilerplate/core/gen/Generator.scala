package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.InputParser

trait Generator { self =>
  type Options

  val title: String

  def genCls(cls: Cls, opt: Options, glopt: GlobalOptions): List[String]
  def genSB(sb: SealedBase, opt: Options, glopt: GlobalOptions): List[String]

  final def gen(parts: TraversableOnce[InputParser.Element.Success], o: Options, go: GlobalOptions): List[String] =
    if (go.generateCompanions)
      // Order doesn't matter
      parts.toIterator.map(_.value).flatMap {
        case c: Cls         => genCls(c, o, go)
        case sb: SealedBase => genSB(sb, o, go)
      }.toList
    else {
      // Order matters; generate dependants first
      val r1 = List.newBuilder[String]
      val r2 = List.newBuilder[String]
      parts.toIterator.map(_.value).foreach  {
        case c: Cls         => r1 ++= genCls(c, o, go)
        case sb: SealedBase => r2 ++= genSB(sb, o, go)
      }
      r1 ++= r2.result()
      r1.result()
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

}