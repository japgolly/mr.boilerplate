package japgolly.mrboilerplate.core

import japgolly.univeq.UnivEq

final case class Class(name: String, typeParams: List[Type], fields: List[Field]) {
  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.iterator.map(_.value).mkString("[", ", ", "]")
    val fs = fields.mkString("(", ", ", ")")
    s"class $name$tp$fs"
  }
}

object Class {
  implicit def univEq: UnivEq[Class] = UnivEq.derive
}

final case class Field(name: FieldName, typ: Type) {
  override def toString = s"${name.value}: ${typ.value}"
}

object Field {
  implicit def univEq: UnivEq[Field] = UnivEq.derive
}

final case class FieldName(value: String)

object FieldName {
  implicit def univEq: UnivEq[FieldName] = UnivEq.derive
}

final case class Type(value: String)

object Type {
  implicit def univEq: UnivEq[Type] = UnivEq.derive
}