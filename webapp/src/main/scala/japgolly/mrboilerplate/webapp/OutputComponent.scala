package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object OutputComponent {

  final case class Props(output: String) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomElement =
    <.section(
      Styles.outputOuter,
      <.header(Styles.header, "Output"),
      Highlight(p.output))

  val Component = ScalaComponent.builder[Props]("OutputComponent")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}