package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.InputParser.Element.{Failure, Unrecognised, AbstractClass}
import japgolly.mrboilerplate.webapp.DataReusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{html, document}

object OutputComponent {

  final case class Props(failures: List[Failure], output: String) {
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

    private val redPre = <.pre(^.color.red)

    def render(p: Props): VdomElement = {

      def failures =
        Option.when(p.failures.nonEmpty) {
          var abs = List.empty[AbstractClass]
          var unr = List.empty[Unrecognised]
          p.failures.foreach {
            case a: AbstractClass => abs ::= a
            case u: Unrecognised  => unr ::= u
          }

          val renderAbstractClasses = TagMod.when(abs.nonEmpty)(TagMod(
            <.div("The following types are ignored because they're abstract and unsealed:"),
            <.ul(
              ^.marginTop := "1em",
              abs.reverseIterator.toTagMod(c => <.li(redPre(c.value.name))))))

          val renderUnrecognised = TagMod.when(unr.nonEmpty)(TagMod(
            <.div("Failed to recognise:"),
            <.pre(
              ^.color.red,
              ^.marginTop := "1em",
              ^.marginLeft := "4ex",
              unr.reverseIterator.map(_.text).mkString("\n\n")),
          ))

          <.div(
            renderAbstractClasses,
            renderUnrecognised,
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
          ^.aria.hidden := true,
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
