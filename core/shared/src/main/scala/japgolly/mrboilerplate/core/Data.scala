package japgolly.mrboilerplate.core

import StringUtils._
import japgolly.univeq.UnivEq

final case class Cls(name: String, typeParams: List[Type], fields: List[Field]) {
  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.iterator.map(_.value).mkString("[", ", ", "]")
    val fs = fields.mkString("(", ", ", ")")
    s"class $name$tp$fs"
  }

  implicit lazy val maxFieldLen: MaxFieldLen =
    MaxFieldLen.derive(fields.iterator.map(_.name.value))

  val fieldCount: Int =
    fields.size

  val valDef: String =
    if (typeParams.isEmpty)
      "val"
    else
      "def"

  val typeParamDefs: String =
    if (typeParams.isEmpty)
      ""
    else
      typeParams.mkString("[", ", ", "]")

  val typeParamAp: String =
    if (typeParams.isEmpty)
      ""
    else
      typeParams.map(_.withoutWildcards).mkString("[", ", ", "]")

  def nameWithTypesApplied: String =
    name + typeParamAp

  val fieldNames: String =
    fields.map(_.name).mkString(", ")

  val fieldNameStrs: String =
    fields.map(_.name.quote).mkString(", ")
}

object Cls {
  implicit def univEq: UnivEq[Cls] = UnivEq.derive
}

final case class Field(name: FieldName, typ: Type) {
  override def toString = s"${name.value}: ${typ.value}"
}

object Field {
  implicit def univEq: UnivEq[Field] = UnivEq.derive
}

final case class FieldName(value: String) {
  override def toString = value
  def quote = value.quoted
  def pad(implicit m: MaxFieldLen) = m.pad(value)
  def quotePad(implicit m: MaxFieldLen) = m.pad2(quote)
}

object FieldName {
  implicit def univEq: UnivEq[FieldName] = UnivEq.derive
}

final case class Type(value: String) {
  override def toString = value
  def withoutWildcards = value.withoutWildcards
}

object Type {
  implicit def univEq: UnivEq[Type] = UnivEq.derive
}

final case class MaxFieldLen(value: Int) {
  private val fmt = s"%-${value}s"
  private val fmt2 = s"%-${value+2}s"
  def pad(s: String): String = fmt.format(s)
  def pad2(s: String): String = fmt2.format(s)
}

object MaxFieldLen {
  val zero = apply(0)
  def derive(s: TraversableOnce[String]): MaxFieldLen =
    if (s.isEmpty)
      zero
    else
      apply(s.toIterator.map(_.length).max)
}