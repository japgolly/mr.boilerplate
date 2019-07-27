package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.Cls

trait Generator {
  type Options

  val defaultOptions: Options

  val generate: Options => Cls => List[String]
}
