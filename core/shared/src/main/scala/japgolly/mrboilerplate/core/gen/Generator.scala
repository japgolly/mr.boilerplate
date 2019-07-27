package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.Class

trait Generator {
  type Options

  val defaultOptions: Options

  val generate: Options => Class => List[String]
}
