package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.Cls

trait Generator {
  type Options

  val title: String

  val defaultOptions: Options

  def generate(c: Cls, o: Options): List[String]
}
