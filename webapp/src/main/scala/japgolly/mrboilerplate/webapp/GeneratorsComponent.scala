package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.gen._
import japgolly.mrboilerplate.webapp.DataReusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

object GeneratorsComponent {

  final case class Props(state: StateSnapshot[State]) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  @Lenses
  final case class State(enabled: Set[GeneratorDef],
                         options: List[Generator.AndOptions],
                         glopt: GlobalOptions) {

    def toggle(gd: GeneratorDef): State =
      copy(if (enabled.contains(gd)) enabled - gd else enabled + gd)

    def optionsFor(gd: GeneratorDef): gd.gen.Options =
      options.iterator.map(_.unify(gd.gen)).filterDefined.nextOption().fold(Default.options(gd))(_.options)

    def setOption(o: Generator.AndOptions): State =
      copy(options = o :: options.filterNot(_.gen eq o.gen))
  }

  object State {
    def init: State =
      apply(
        enabled = GeneratorDef.values.iterator.filter(_.enabledByDefault).toSet,
        options = Nil,
        glopt = Default.globalOptions)

    implicit def reusability: Reusability[State] =
      Reusability.byRef
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private def renderGen(gd: GeneratorDef, s: StateSnapshot[State]): VdomElement = {
      import gd.gen
      val enabled = s.value.enabled.contains(gd)
      val header =
        <.label(
          <.input.checkbox(
            Styles.checkbox,
            ^.checked := enabled,
            ^.onChange --> s.modState(_.toggle(gd))),
          gen.title)

      def body: VdomElement = {
        val optionState: StateSnapshot[gen.Options] =
          s.zoomState(_.optionsFor(gd))(o => _.setOption(gen.andOptions(o)))
        <.div(
          Styles.genBody,
          gd.renderOptions(optionState))
      }
      <.div(
        header,
        Option.when(enabled)(body))
    }

    private def renderGlobalOptions(s: StateSnapshot[State]) =
      <.div(
        <.label("Output Options"),
        <.div(
          Styles.genBody,
          GeneratorDef.renderGlobalOptions(s.zoomStateL(State.glopt))))

    def render(p: Props): VdomElement =
      <.section(
        Styles.genOuter,
        <.div(GeneratorDef.values.whole.map(renderGen(_, p.state)): _*))(
        renderGlobalOptions(p.state))
  }

  val Component = ScalaComponent.builder[Props]("GeneratorsComponent")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

}