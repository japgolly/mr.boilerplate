package japgolly.mrboilerplate.core

import StringUtils._
import japgolly.microlibs.utils.Memo
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

  private val polyField: Field => Boolean =
    Memo(f => typeParams.exists(t => t.isHK && f.typ.contains(t)))

  private val typeUsedInMonoField: Type => Boolean =
    Memo(t => fields.exists(f => !polyField(f) && f.typ.contains(t)))

//  if (name == "PolyK2") {
//    println("types:")
//    for (t <- typeParams)
//      println(s"  $t  ---  usedInMono=${typeUsedInMonoField(t)}")
//    println("fields:")
//    for (f <- fields)
//      println(s"  $f  ---  polyField=${polyField(f)}")
//    println("contains:")
//    for {
//      f <- fields
//      t <- typeParams
//    } println(s"  ${f.typ} contains $t via ${t.findRegex}  ==  ${f.typ.contains(t)}")
//  }

  /** `[F[_], A: TC1: TC2, B: TC1: TC2]` */
  def typeParamDefsWithTC(tc1: String, tcN: String*): String =
    if (typeParams.isEmpty)
      ""
    else {
      val constraints = (tc1 +: tcN).map(": " + _).mkString
      def needTC(t: Type): Boolean = !t.isHK && typeUsedInMonoField(t)
      typeParams
        .iterator
        .map(t => if (needTC(t)) t.value + constraints else t.value)
        .mkString("[", ", ", "]")
    }

  def implicitHkEv(tc: String): List[Field] =
    fields
      .iterator
      .filter(f => typeParams.exists(t => t.isHK && f.typ.contains(t)))
      .zipWithIndex
      .map { case (f, i) => Field(FieldName("ev" + (i + 1)), Type(s"$tc[${f.typ}]")) }
      .toList

  /** `(implicit ev1: TC[F[A]])` */
  def implicitHkEvDecl(tc: String): String =
    implicitHkEv(tc) match {
      case Nil => ""
      case evs => evs.mkString("(implicit ", ", ", ")")
    }

  /** `[F[_], A, B: TC](implicit ev1: TC[F[A]])` */
  def typeParamDefsAndEvTC(tc: String): String =
    typeParamDefsWithTC(tc) + implicitHkEvDecl(tc)

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