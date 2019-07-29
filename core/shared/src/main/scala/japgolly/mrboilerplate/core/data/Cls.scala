package japgolly.mrboilerplate.core.data

import japgolly.microlibs.utils.Memo
import japgolly.mrboilerplate.core.MaxLen
import japgolly.univeq.UnivEq

final case class Cls(name      : String,
                     typeParams: List[Type],
                     fields    : List[Field],
                     superTypes: List[Type]) extends TypeDef {

  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.mkString("[", ", ", "]")
    val fs = fields.mkString("(", ", ", ")")
    val ex = if (superTypes.isEmpty) "" else superTypes.mkString(" extends ", " with ", "")
    s"class $name$tp$fs$ex"
  }

  implicit lazy val maxFieldLen: MaxLen =
    MaxLen.derive(fields.iterator.map(_.name.value))

  val fieldCount: Int =
    fields.size

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

  val fieldNames: String =
    fields.map(_.name).mkString(", ")

  val fieldNameStrs: String =
    fields.map(_.name.quote).mkString(", ")
}

object Cls {
  implicit def univEq: UnivEq[Cls] = UnivEq.derive
}
