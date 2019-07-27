package japgolly.mrboilerplate.webapp.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._

object MainComponent {

  final case class Props() {
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div(^.color.red, "NIM nim nim nim nim nim nim nim nim NIM NIM !!!")
  }

  val Component = ScalaComponent.builder[Props]("MainComponent")
    .renderBackend[Backend]
    .build
}