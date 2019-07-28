package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.InputParser.Element.Unrecognised
import japgolly.mrboilerplate.webapp.DataReusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{html, document}

object OutputComponent {

  final case class Props(unrecognised: List[Unrecognised], output: String) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {

    private val hiddenTextAreaRef = Ref[html.TextArea]

    private val copyToClipboard: Callback =
      for {
        textArea <- hiddenTextAreaRef.get
      } yield {
        textArea.select()
        document.execCommand("copy")
        ()
      }

    def render(p: Props): VdomElement = {

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

      def copyButton = TagMod(
        ^.position.relative,
        <.textarea.withRef(hiddenTextAreaRef)(
          ^.position.absolute,
          ^.tabIndex := -1,
          ^.zIndex := "-1",
          ^.opacity := "0",
          ^.readOnly := true,
          ^.value := p.output),
        <.button(
          ^.position.absolute,
          ^.top := "0",
          ^.right := "0",
          ^.cls := "btn btn-primary",
          ^.role := "button",
          ^.onClick --> copyToClipboard,
          "Copy to clipboard"))

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
          copyButton,
          Highlight(p.output)))
    }
  }

  val Component = ScalaComponent.builder[Props]("OutputComponent")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
