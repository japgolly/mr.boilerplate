package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.InputParser.Element.Unrecognised
import japgolly.mrboilerplate.webapp.DataReusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object OutputComponent {

  final case class Props(unrecognised: List[Unrecognised], output: String) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomElement = {

    def failures =
      Option.when(p.unrecognised.nonEmpty) {
        <.div(
          <.div("Failed to recognise:"),
          <.pre(
            ^.color.red,
            ^.marginTop := "1em",
            ^.marginLeft := "4ex",
            p.unrecognised.iterator.map(_.text).mkString("\n\n")),
          <.div(
            ^.marginBottom := "1em",
            "Ignoring the above, the rest of the input results in:"),
        )
      }

    <.section(
      Styles.outputOuter,
      ^.display.flex,
      ^.flexDirection.column,
      <.header(Styles.header, "Output"),
      failures,
      <.div(
        ^.overflow.auto,
        ^.flexGrow := "1",
        ^.minHeight := "16em",
        Highlight(p.output)))
  }

  val Component = ScalaComponent.builder[Props]("OutputComponent")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}