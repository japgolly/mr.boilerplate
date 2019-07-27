package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.Cls
import japgolly.mrboilerplate.core.StringUtils._
import monocle.macros.Lenses

object UnivEqGen extends Generator {

  override val title = "UnivEq"

  @Lenses
  final case class Options(oneLine: Boolean)

  override def generate(cls: Cls, opt: Options, glopt: GlobalOptions): List[String] = {
    import cls._

    val suffix =
      if (glopt.shortInstanceNames)
        ""
      else
        name.withHeadUpper

    val sep = if (opt.oneLine) " " else "\n  "

    val decl = s"implicit $valDef univEq$suffix$typeParamDefs: UnivEq[$nameWithTypesApplied] =${sep}UnivEq.derive"

    decl :: Nil

  }
}