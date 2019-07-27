package japgolly.mrboilerplate.core.gen

import japgolly.univeq.UnivEq
import monocle.macros.Lenses

@Lenses
final case class GlobalOptions(shortInstanceNames: Boolean,
                               generateCompanions: Boolean)

object GlobalOptions {
  implicit def univEq: UnivEq[GlobalOptions] = UnivEq.derive
}
