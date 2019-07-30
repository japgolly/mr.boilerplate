package japgolly.mrboilerplate.core.data

import japgolly.mrboilerplate.core.MaxLen
import japgolly.mrboilerplate.core.StringUtils._
import japgolly.univeq.UnivEq

final case class Field(name: FieldName, typ: Type) {
  override def toString = s"${name.value}: ${typ.value}"
}

object Field {
  implicit def univEq: UnivEq[Field] = UnivEq.derive
}

final case class FieldName(value: String) {
  override def toString = value
  def quote = value.quoted
  def pad(implicit m: MaxLen) = m.pad(value)
  def quotePad(implicit m: MaxLen) = m.pad2(quote)
}

object FieldName {
  implicit def univEq: UnivEq[FieldName] = UnivEq.derive
}
