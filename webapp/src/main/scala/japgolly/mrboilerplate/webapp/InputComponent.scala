package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
import monocle.macros.Lenses

object InputComponent {

  final case class Props(state: StateSnapshot[State]) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  @Lenses
  final case class State(typePrefix: String,
                         mainText: String) {
    val mainText2 = mainText.replaceFirst("[\\s{]+$", "")
    val typePrefix2 = typePrefix.filterNot(_.isWhitespace)
  }

  object State {
    def init: State =
      State(
        typePrefix = "",
        mainText = Default.input,
      )

    implicit val reusability: Reusability[State] =
      Reusability.derive
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val onChangeTypePrefix =
      ^.onChange ==> ((e: ReactEventFromInput) =>
        $.props.flatMap(_.state.zoomStateL(State.typePrefix).setState(e.target.value)))

    private val onChangeMainText =
      ^.onChange ==> ((e: ReactEventFromTextArea) =>
        $.props.flatMap(_.state.zoomStateL(State.mainText).setState(e.target.value)))

    def render(p: Props): VdomElement = {

      val typePrefix =
        <.div(
          ^.display.flex,
          <.div(
            ^.paddingTop := "0.375rem",
            ^.marginRight := "1ex",
            "Type Prefix:"),
          <.div(
            ^.flexGrow := "1",
            <.input.text(
              ^.cls := "form-control",
              ^.fontFamily := "monospace",
              ^.value := p.state.value.typePrefix,
              onChangeTypePrefix)))

      val mainText =
        <.textarea(
          ^.cls := "form-control",
          ^.width := "100%",
          ^.height := "100%",
          ^.minHeight := "12em",
          ^.fontFamily := "monospace",
          ^.backgroundColor := "#eee",
          ^.color := "#000",
          ^.overflow.auto,
          ^.autoFocus := true,
          ^.value := p.state.value.mainText,
          onChangeMainText)

      <.section(
        Styles.inputOuter,
        ^.display.flex,
        ^.flexDirection.column,
        ^.cls := "form-group",
        ^.height := "100%", ^.minHeight := "0", // chrome bug: https://bugs.chromium.org/p/chromium/issues/detail?id=927066
        <.header(Styles.header, "Input"),
        <.div(
          ^.flexGrow := "1",
          mainText),
        <.div(
          ^.marginTop  := "1em",
          typePrefix),
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("InputComponent")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
