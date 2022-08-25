package japgolly.mrboilerplate.core.data

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import japgolly.mrboilerplate.core.MaxLen
import japgolly.mrboilerplate.core.StringUtils._
import japgolly.mrboilerplate.core.gen.GlobalOptions
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import scala.collection.immutable.SortedSet

sealed trait TypeDef {
  val name      : String
  val typeName  : String
  val typeParams: List[Type]
  val superTypes: List[Type]

  def mapName(f: String => String): TypeDef

  /** the last part of the name as a suffix */
  lazy val tailSuffix =
    name.replaceFirst("^.*\\.", "").withHeadUpper

  /** the whole name as a suffix */
  lazy val wholeSuffix =
    name.filterNot(_ == '.').withHeadUpper

  def instanceNameSuffix(implicit g: GlobalOptions): String =
    if (g.shortInstanceNames)
      ""
    else
      wholeSuffix

  final def valDef(implicit g: GlobalOptions): String =
    if (typeParams.nonEmpty)
      "def"
    else if (g.makeValsLazy)
      "lazy val"
    else
      "val"

  final lazy val typeParamDefs: String =
    if (typeParams.isEmpty)
      ""
    else
      typeParams.mkString("[", ", ", "]")

  protected final lazy val polyField: Field => Boolean =
    Memo(f => typeParams.exists(t => t.isHK && f.typ.contains(t)))

  final lazy val typeParamAp: String =
    if (typeParams.isEmpty)
      ""
    else
      typeParams.map(_.withoutWildcards).mkString("[", ", ", "]")

  def typeParamDefsAndEvTC(tc: String): String

  final def typeNamePoly: String =
    typeName + typeParamAp
}

object TypeDef {
  implicit def univEq: UnivEq[TypeDef] = UnivEq.derive

  sealed trait Concrete extends TypeDef
}

// =====================================================================================================================

final case class SealedBase(name            : String,
                            typeParams      : List[Type],
                            superTypes      : List[Type],
                            directChildren  : List[TypeDef],
                           ) extends TypeDef {

  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.mkString("[", ", ", "]")
    val ex = if (superTypes.isEmpty) "" else superTypes.mkString(" extends ", " with ", "")
    val dc = if (directChildren.isEmpty) "" else s" (children=${directChildren.mkString(", ")})"
    s"sealed $name$tp$ex$dc"
  }

  override val typeName =
    name

  override def mapName(f: String => String): SealedBase =
    copy(
      name = f(name),
      directChildren = directChildren.map(_.mapName(f)))

  override def typeParamDefsAndEvTC(tc: String) =
    typeParamDefs + implicitPolyCaseEvDecl(tc)

  /** `(implicit t1: TC[Case1[A]])` */
  def implicitPolyCaseEvDecl(tc: String): String = {
    val prefix = tc.head.toLower
    val implicits =
      concreteTransitiveChildren.children.iterator.flatMap {
        case c: Cls => Option.when(c.typeParams.nonEmpty)(c.typeNamePoly)
        case _: Obj => None
      }.to(SortedSet).iterator.zipWithIndex.map {
        case ((typ, num)) => s"$prefix${num + 1}: $tc[$typ]"
      }
    if (implicits.isEmpty) "" else implicits.mkString("(implicit ", ", ", ")")
  }

  object concreteTransitiveChildren {

    lazy val children: List[TypeDef.Concrete] =
      MutableArray(
        directChildren
          .iterator
          .flatMap {
            case s: SealedBase => s.concreteTransitiveChildren.children
            case c: Cls        => c :: Nil
            case o: Obj        => o :: Nil
          }
          .toSet
      ).sortBy(_.name).to(List)


    lazy val maxNameLen: MaxLen =
      MaxLen.derive(children.map(_.name))

    lazy val maxTailSuffixLen: MaxLen =
      MaxLen.derive(children.map(_.tailSuffix))

    lazy val maxWholeSuffixLen: MaxLen =
      MaxLen.derive(children.map(_.wholeSuffix))

    lazy val maxTypeNameLen: MaxLen =
      MaxLen.derive(children.map(_.typeName))

    lazy val maxTypeNamePolyLen: MaxLen =
      MaxLen.derive(children.map(_.typeNamePoly))

    lazy val maxCaseTypeLen: MaxLen =
      MaxLen.derive(children.map {
        case c: Cls => c.typeNamePoly
        case o: Obj => o.name
      })
  }
}

object SealedBase {
  implicit def univEq: UnivEq[SealedBase] = UnivEq.derive
}

// =====================================================================================================================

final case class Cls(name      : String,
                     typeParams: List[Type],
                     fields    : List[Field],
                     superTypes: List[Type]) extends TypeDef.Concrete {

  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.mkString("[", ", ", "]")
    val fs = fields.mkString("(", ", ", ")")
    val ex = if (superTypes.isEmpty) "" else superTypes.mkString(" extends ", " with ", "")
    s"class $name$tp$fs$ex"
  }

  override val typeName =
    name

  override def mapName(f: String => String): Cls =
    copy(name = f(name))

  implicit lazy val maxFieldLen: MaxLen =
    MaxLen.derive(fields.iterator.map(_.name.value))

  val fieldCount: Int =
    fields.size

  private val typeUsedInMonoField: Type => Boolean =
    Memo(t => fields.exists(f => !polyField(f) && f.typ.contains(t)))

  lazy val apply =
    s"$name.apply$typeParamAp"

  lazy val unapply =
    fields match {
      case f :: Nil => "_." + f.name
      case fs       => fs.map("a." + _.name).mkString("a => (", ", ", ")")
    }

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

  def implicitHkEv(tc: String): List[Field] = {
    val prefix = tc.head.toLower.toString
    fields
      .iterator
      .filter(f => typeParams.exists(t => t.isHK && f.typ.contains(t)))
      .zipWithIndex
      .map { case (f, i) => Field(FieldName(prefix + (i + 1)), Type(s"$tc[${f.typ}]")) }
      .toList
  }

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

// =====================================================================================================================

final case class Obj(name      : String,
                     superTypes: List[Type]) extends TypeDef.Concrete {

  override val typeName = name + ".type"

  override def mapName(f: String => String): Obj =
    copy(name = f(name))

  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.mkString("[", ", ", "]")
    val ex = if (superTypes.isEmpty) "" else superTypes.mkString(" extends ", " with ", "")
    s"object $name$tp$ex"
  }

  override val typeParams =
    Nil

  override def typeParamDefsAndEvTC(tc: String) =
    typeParamDefs
}

object Obj {
  implicit def univEq: UnivEq[Obj] = UnivEq.derive
}
