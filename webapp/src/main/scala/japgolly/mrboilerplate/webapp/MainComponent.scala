package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.InputParser
import japgolly.mrboilerplate.core.gen.Generator
import japgolly.mrboilerplate.webapp.DataReusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
import monocle.macros.Lenses
import scala.collection.immutable.ListSet

object MainComponent {

  final case class Props() {
    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(input: InputComponent.State,
                         gen: GeneratorsComponent.State)

  object State {

    def init = apply(
      input = InputComponent.State.init,
      gen   = GeneratorsComponent.State.init,
    )
  }

  final class Backend($: BackendScope[Props, State]) {

    private val inputSS = StateSnapshot.withReuse.zoomL(State.input).prepareVia($)
    private val genSS   = StateSnapshot.withReuse.zoomL(State.gen).prepareVia($)

    private val pxInputText  = Px.state($).map(_.input.mainText).withReuse.autoRefresh
    private val pxGen        = Px.state($).map(_.gen).withReuse.autoRefresh
    private val pxParsed     = pxInputText.map(InputParser.parse).withReuse
    private val pxParsedKO   = pxParsed.map(_.iterator.map(_.failure).filterDefined.to[List]).withReuse
    private val pxParsedOK1  = pxParsed.map(_.iterator.map(_.success).filterDefined.map(_.value).to[ListSet]).withReuse
    private val pxTypePrefix = Px.state($).map(_.input.typePrefix2).withReuse.autoRefresh

    private val pxParsedOK =
      for {
        defs   <- pxParsedOK1
        prefix <- pxTypePrefix
      } yield if (prefix.isEmpty) defs else defs.map(_.mapName(prefix + _))

    private val pxOutput =
      for {
        gen  <- pxGen
        data <- pxParsedOK
      } yield Generator(gen.enabledWithOptions, data)(gen.glopt)

    def render(p: Props, s: State): VdomElement = {
      <.div(
        Styles.mainOuter,
        InputComponent.Props(inputSS(s)).render,
        GeneratorsComponent.Props(genSS(s)).render,
        OutputComponent.Props(pxParsedKO.value(), pxOutput.value()).render,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("MainComponent")
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}