package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._

object InputComponent {

  final case class Props(text: StateSnapshot[String]) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {
    private val onChange = ^.onChange ==> ((e: ReactEventFromTextArea) => $.props.flatMap(_.text.setState(e.target.value)))
    def render(p: Props): VdomElement =
      <.section(
        Styles.inputOuter,
        ^.cls := "form-group",
        <.header(Styles.header, "Input"),
        <.textarea(
          ^.cls := "form-control",
          ^.width := "100%",
          ^.height := "calc(100% - 2em)",
          ^.fontFamily := "monospace",
          ^.backgroundColor := "#eee",
          ^.color := "#000",
          ^.overflow.auto,
          ^.autoFocus := true,
          ^.value := p.text.value,
          onChange))
  }

  val Component = ScalaComponent.builder[Props]("InputComponent")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
