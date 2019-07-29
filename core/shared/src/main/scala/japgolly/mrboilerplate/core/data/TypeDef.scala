package japgolly.mrboilerplate.core.data

import japgolly.microlibs.utils.Memo
import japgolly.mrboilerplate.core.StringUtils._
import japgolly.mrboilerplate.core.gen.GlobalOptions

trait TypeDef {
  val name      : String
  val typeParams: List[Type]
  val superTypes: List[Type]

  def termSuffix(g: GlobalOptions): String =
    if (g.shortInstanceNames)
      ""
    else
      name.withHeadUpper

  final def valDef: String =
    if (typeParams.isEmpty)
      "val"
    else
      "def"

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

  final def nameWithTypesApplied: String =
    name + typeParamAp
}
