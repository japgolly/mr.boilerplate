package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import japgolly.microlibs.stdlib_ext.StdlibExt._

trait Generator { self =>
  type Options

  val title: String

  def generate(cls: Cls, opt: Options, glopt: GlobalOptions): List[String]

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