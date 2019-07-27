package japgolly.mrboilerplate.core.gen

import japgolly.univeq.UnivEq
import monocle.macros.Lenses

@Lenses
final case class GlobalOptions(shortInstanceNames: Boolean)

object GlobalOptions {
  val default = apply(
    shortInstanceNames = false,
  )

  implicit def univEq: UnivEq[GlobalOptions] = UnivEq.derive
}
