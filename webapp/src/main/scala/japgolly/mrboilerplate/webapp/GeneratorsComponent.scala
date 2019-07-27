package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.gen._
import japgolly.mrboilerplate.webapp.DataReusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._

object GeneratorsComponent {

  final case class Props(state: StateSnapshot[State]) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final case class State(enabled: Set[GenDef],
                         options: List[Generator.AndOptions]) {

    def toggle(gd: GenDef): State =
      copy(if (enabled.contains(gd)) enabled - gd else enabled + gd)

    def optionsFor(g: Generator): g.Options =
      options.iterator.map(_.unify(g)).filterDefined.nextOption().fold(g.defaultOptions)(_.options)

    def setOption(o: Generator.AndOptions): State =
      copy(options = o :: options.filterNot(_.gen eq o.gen))
  }

  object State {
    def init: State =
      apply(GenDef.values.iterator.filter(_.enabledByDefault).toSet, Nil)

    implicit def reusability: Reusability[State] =
      Reusability.byRef
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private def renderGen(gd: GenDef, s: StateSnapshot[State]): VdomElement = {
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
          s.zoomState(_.optionsFor(gen))(o => _.setOption(gen.andOptions(o)))
        <.div(
          Styles.genBody,
          gd.renderOptions(optionState))
      }
      <.div(
        header,
        Option.when(enabled)(body))
    }

    def render(p: Props): VdomElement =
      <.section(
        Styles.genOuter,
        <.div(GenDef.values.whole.map(renderGen(_, p.state)): _*))
  }

  val Component = ScalaComponent.builder[Props]("GeneratorsComponent")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

}