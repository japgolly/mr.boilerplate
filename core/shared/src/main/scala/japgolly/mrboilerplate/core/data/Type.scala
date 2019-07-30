package japgolly.mrboilerplate.core.data

import japgolly.mrboilerplate.core.StringUtils._
import japgolly.univeq.UnivEq

final case class Type(value: String) {
  override def toString = value
  def withoutWildcards = value.withoutWildcards
  def head = value.takeWhile(_ != '[')
  val isHK = value.contains('[')

  def contains(t: Type): Boolean =
    t.findRegex.pattern.matcher(value).find

  val findRegex =
    if (isHK)
      s"\\b${value.withoutWildcards.quoteForRegex}\\b\\s*\\[".r
    else
      s"\\b${value.quoteForRegex}\\b".r

  def isInstanceOfType(t: Type): Boolean =
    t.head == this.head
}

object Type {
  implicit def univEq: UnivEq[Type] = UnivEq.derive
}
