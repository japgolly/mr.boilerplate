package japgolly.mrboilerplate.core

trait Generator {
  type Options

  val defaultOptions: Options

  val generate: Options => Class => List[String]
}
