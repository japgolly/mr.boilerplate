package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import monocle.macros.Lenses

object UnivEqGen extends Generator {

  override val title = "UnivEq"

  @Lenses
  final case class Options(oneLine: Boolean)

  override def genCls(cls: Cls, opt: Options, glopt: GlobalOptions): List[String] =
    gen(cls, opt, glopt)

  override def genSB(sb: SealedBase, opt: Options, glopt: GlobalOptions): List[String] =
    gen(sb, opt, glopt)

  private def gen(td: TypeDef, opt: Options, glopt: GlobalOptions): List[String] = {
    import td._

    val suffix = termSuffix(glopt)

    val sep = if (opt.oneLine) " " else "\n  "

    val defn = s"implicit def univEq$suffix${typeParamDefsAndEvTC("UnivEq")}: UnivEq[$nameWithTypesApplied]"

    val decl = s"$defn =${sep}UnivEq.derive"

    decl :: Nil
  }
}