package japgolly.mrboilerplate.core.data

import japgolly.mrboilerplate.core.StringUtils._
import japgolly.univeq.UnivEq

final case class Type(value: String) {
  override def toString = value
  def withoutWildcards = value.withoutWildcards
  val isHK = value.contains('[') && value.contains('_')

  def contains(t: Type): Boolean =
    t.findRegex.pattern.matcher(value).find

  val findRegex =
    if (isHK)
      s"\\b${value.withoutWildcards.quoteForRegex}\\b\\s*\\[".r
    else
      s"\\b${value.quoteForRegex}\\b".r
}

object Type {
  implicit def univEq: UnivEq[Type] = UnivEq.derive
}
