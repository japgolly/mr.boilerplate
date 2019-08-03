package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import monocle.macros.Lenses

object UnivEqGen extends Generator {

  override val title = "UnivEq"

  @Lenses
  final case class Options(oneLine: Boolean)

  override def gen(opt: Options)(implicit glopt: GlobalOptions): TypeDef => List[String] = td => {
    import td._

    val defn = s"implicit def univEq$suffix${typeParamDefsAndEvTC("UnivEq")}: UnivEq[$typeNamePoly]"

    val sep = if (opt.oneLine) " " else "\n  "

    val decl = s"$defn =${sep}UnivEq.derive"

    decl :: Nil
  }

  override def initStatements(data: Traversable[TypeDef], opt: Options)(implicit glopt: GlobalOptions): List[String] =
    "import japgolly.univeq.UnivEq" :: Nil
}