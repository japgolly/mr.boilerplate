package japgolly.mrboilerplate.core

final case class MaxLen(value: Int) {
  private val fmt = s"%-${value}s"
  private val fmt2 = s"%-${value+2}s"
  def pad(s: String): String = fmt.format(s)
  def pad2(s: String): String = fmt2.format(s)
}

object MaxLen {
  val zero = apply(0)
  def derive(s: IterableOnce[String]): MaxLen =
    if (s.iterator.isEmpty)
      zero
    else
      apply(s.iterator.map(_.length).max)
}
