package japgolly.mrboilerplate.webapp

import monocle.Lens
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.mrboilerplate.core.gen._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq

sealed trait GeneratorDef {
  val gen: Generator
  val enabledByDefault: Boolean
  def renderOptions(s: StateSnapshot[gen.Options]): VdomElement

  final def title = gen.title

  val foldOptions: ((Circe.Options, UnivEqGen.Options)) => gen.Options
}

object GeneratorDef {

  implicit def univEq: UnivEq[GeneratorDef] = UnivEq.derive

  val values = AdtMacros.adtValues[GeneratorDef].sortBy(_.title)

  // ===================================================================================================================

  case object CirceDef extends GeneratorDef {
    override val gen = Circe

    override val enabledByDefault = true

    override val foldOptions = _._1

    override def renderOptions(s: StateSnapshot[Circe.Options]) =
      <.div(
        checkbox(s)(Circe.Options.singlesAsObjects, "Encode single-field as single-key objects"),
        checkbox(s)(Circe.Options.monadicObjects  , "Monadic object codecs"),
        checkbox(s)(Circe.Options.keyConstants    , "Constants for object keys"),
      )
  }

  case object UnivEqDef extends GeneratorDef {
    override val gen = UnivEqGen

    override val enabledByDefault = false

    override val foldOptions = _._2

    override def renderOptions(s: StateSnapshot[UnivEqGen.Options]) =
      <.div(
        checkbox(s)(UnivEqGen.Options.oneLine, "One-liners"),
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
      ^.marginTop := "0.1em",
      <.label(
        <.input.checkbox(
          Styles.checkbox,
          ^.checked := l.get(s.value),
          ^.onChange --> s.modState(l.modify(!_))),
        txt))
}
