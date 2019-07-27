package japgolly.mrboilerplate.webapp

import monocle.Lens
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.mrboilerplate.core.gen.{Circe, Generator}
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq

sealed trait GenDef {
  val gen: Generator
  val enabledByDefault: Boolean
  def renderOptions(s: StateSnapshot[gen.Options]): VdomElement

  final def title = gen.title
}

object GenDef {

  implicit def univEq: UnivEq[GenDef] = UnivEq.derive

  val values = AdtMacros.adtValues[GenDef].sortBy(_.title)

  // ===================================================================================================================

  case object CirceDef extends GenDef {
    override val gen = Circe

    override val enabledByDefault = true

    override def renderOptions(s: StateSnapshot[Circe.Options]) =
      <.div(
        checkbox(s)(Circe.Options.shortInstanceNames, "Use short instance names"),
        checkbox(s)(Circe.Options.singlesAsObjects, "Encode single-field as single-key objects"),
        checkbox(s)(Circe.Options.monadicObjects, "Encode/decode objects monadically"),
      )
  }

  // ===================================================================================================================

  private def checkbox[O](s: StateSnapshot[O])(l: Lens[O, Boolean], txt: String) =
    <.div(
      <.label(
        <.input.checkbox(
          Styles.checkbox,
          ^.checked := l.get(s.value),
          ^.onChange --> s.modState(l.modify(!_))),
        txt))
}
