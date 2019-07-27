package japgolly.mrboilerplate.webapp

import monocle.Lens
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.mrboilerplate.core.gen.{Circe, Generator, GlobalOptions}
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq

sealed trait GenDef {
  val gen: Generator
  val enabledByDefault: Boolean
  def renderOptions(s: StateSnapshot[gen.Options]): VdomElement

  final def title = gen.title

  val foldOptions: (=> Circe.Options) => gen.Options
}

object GenDef {

  implicit def univEq: UnivEq[GenDef] = UnivEq.derive

  val values = AdtMacros.adtValues[GenDef].sortBy(_.title)

  // ===================================================================================================================

  case object CirceDef extends GenDef {
    override val gen = Circe

    override val enabledByDefault = true

    override val foldOptions = o => o

    override def renderOptions(s: StateSnapshot[Circe.Options]) =
      <.div(
        checkbox(s)(Circe.Options.singlesAsObjects, "Encode single-field as single-key objects"),
        checkbox(s)(Circe.Options.monadicObjects  , "Monadic object codecs"),
        checkbox(s)(Circe.Options.keyConstants    , "Constants for object keys"),
      )
  }

  // ===================================================================================================================

  def renderGlobalOptions(s: StateSnapshot[GlobalOptions]) =
    <.div(
      checkbox(s)(GlobalOptions.shortInstanceNames, "Use short instance names"),
      checkbox(s)(GlobalOptions.generateCompanions, "Generate companion objects"),
    )

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
