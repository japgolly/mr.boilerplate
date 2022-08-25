package japgolly.mrboilerplate.webapp

import monocle.Lens
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.mrboilerplate.core.gen._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html

sealed trait GeneratorDef {
  val gen: Generator
  val enabledByDefault: Boolean
  def renderOptions(s: StateSnapshot[gen.Options]): VdomElement

  final def title = gen.title

  val foldOptions: ((Circe.Options, UnivEqGen.Options, BooPickle.Options, JsonCodec.Options)) => gen.Options
}

object GeneratorDef {

  implicit def univEq: UnivEq[GeneratorDef] = UnivEq.derive

  val values = AdtMacros.adtValues[GeneratorDef].sortBy(_.title)

  // ===================================================================================================================

  case object BooPickleDef extends GeneratorDef {
    override val gen = BooPickle

    override val enabledByDefault = false

    override val foldOptions = _._3

    override def renderOptions(s: StateSnapshot[BooPickle.Options]) =
      <.div(
        checkbox(s)(BooPickle.Options.conciseSingleFields, "Concise single-field codecs"),
        checkbox(s)(BooPickle.Options.objectCodecs       , "Generate codecs for Scala objects"),
        checkbox(s)(BooPickle.Options.keyConstants       , "Constants for sum-type tags"),
      )
  }

  case object CirceDef extends GeneratorDef {
    override val gen = Circe

    override val enabledByDefault = true

    override val foldOptions = _._1

    override def renderOptions(s: StateSnapshot[Circe.Options]) =
      <.div(
        checkbox(s)(Circe.Options.keyConstants    , "Constants for object keys"),
        checkbox(s)(Circe.Options.singlesAsObjects, "Encode single-field as single-key objects"),
        checkbox(s)(Circe.Options.objectCodecs    , "Generate codecs for Scala objects"),
        checkbox(s)(Circe.Options.monadicObjects  , "Monadic object codecs"),
        select(s)(Circe.Options.sumTypes          , "Sum-type format", Circe.Options.SumTypeFormat.values) {
          case Circe.Options.SumTypeFormat.TypeToValue   => """{"<type>":"<value>"}"""
          case Circe.Options.SumTypeFormat.UntaggedUnion => """<value₁> | <value₂> | …"""
        },
      )
  }

  case object JsonCodecDef extends GeneratorDef {
    override val gen = JsonCodec

    override val enabledByDefault = false

    override val foldOptions = _._4

    override def renderOptions(s: StateSnapshot[JsonCodec.Options]) =
      <.div(
        checkbox(s)(JsonCodec.Options.keyConstants    , "Constants for object keys"),
        checkbox(s)(JsonCodec.Options.singlesAsObjects, "Encode single-field as single-key objects"),
        checkbox(s)(JsonCodec.Options.objectCodecs    , "Generate codecs for Scala objects"),
        checkbox(s)(JsonCodec.Options.monadicObjects  , "Monadic object codecs"),
        select(s)(JsonCodec.Options.sumTypes          , "Sum-type format", JsonCodec.Options.SumTypeFormat.values) {
          case JsonCodec.Options.SumTypeFormat.TypeToValue   => """{"<type>":"<value>"}"""
          case JsonCodec.Options.SumTypeFormat.UntaggedUnion => """<value₁> | <value₂> | …"""
        },
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
      checkbox(s)(GlobalOptions.generateCompanions, "Generate companion objects"),
      checkbox(s)(GlobalOptions.makeValsLazy      , "Lazy vals"),
      checkbox(s)(GlobalOptions.shortInstanceNames, "Use short instance names"),
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

  private val checkboxOn = <.input.checkbox(Styles.checkbox, ^.readOnly := true, ^.disabled := true, ^.checked := true)

  private def select[O, A](s: StateSnapshot[O])(l: Lens[O, A], label: String, options: NonEmptyVector[A])
                          (optionLabel: A => String) = {

    def onChange(e: ReactEventFrom[html.Select]): Callback =
      for {
        v <- CallbackTo(e.target.value).toCBO
        a <- CallbackOption.option(options.whole.find(optionLabel(_) ==* v))
        _ <- s.modState(l.set(a)).toCBO
      } yield ()

    <.div(
      ^.marginTop := "0.1em",
      <.label(
        checkboxOn,
        label + ": ",
        <.select(
          ^.fontFamily := "monospace",
          ^.value := optionLabel(l.get(s.value)),
          ^.onChange ==> onChange,
          options.whole.iterator.map(optionLabel).toTagMod(s =>
            <.option(
              ^.value := s,
              s + " ")))))
  }
}
