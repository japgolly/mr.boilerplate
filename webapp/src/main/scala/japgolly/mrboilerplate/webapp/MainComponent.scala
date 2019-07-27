package japgolly.mrboilerplate.webapp

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.mrboilerplate.core.InputParser
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
  final case class State(input: String,
                         gen: GeneratorsComponent.State)

  object State {

    def init = apply(
      input = initialInput,
      gen   = GeneratorsComponent.State.init,
    )
  }

  private def initialInput =
    s"""
       |case class Person(name   : PersonName,
       |                  address: Address,
       |                  phone  : Option[PhoneNumber])
       |
       |final case class Roles[F[_], +A <: AnyRef](roles: F[A])
     """.stripMargin.trim

  final class Backend($: BackendScope[Props, State]) {

    private val inputSS = StateSnapshot.withReuse.zoomL(State.input).prepareVia($)
    private val genSS   = StateSnapshot.withReuse.zoomL(State.gen).prepareVia($)

    private val pxInput    = Px.state($).map(_.input).withReuse.autoRefresh
    private val pxGen      = Px.state($).map(_.gen).withReuse.autoRefresh
    private val pxParsed   = pxInput.map(InputParser.parse).withReuse
    private val pxParsedOk = pxParsed.map(_.iterator.map(_.success).filterDefined.to[ListSet]).withReuse

    private val pxOutput =
      for {
        classes <- pxParsedOk
        gen     <- pxGen
      } yield {
        // TODO temp
        gen.enabled.iterator.flatMap { gd =>
          val opt = gen.optionsFor(gd.gen)
          classes.iterator.map { cls =>
            gd.gen.generate(cls, opt, gen.glopt)
          }
        }.toList
          .flatten.mkString("\n\n")
      }


    def render(p: Props, s: State): VdomElement = {
      <.div(
        Styles.mainOuter,
        InputComponent.Props(inputSS(s)).render,
        GeneratorsComponent.Props(genSS(s)).render,
        OutputComponent.Props(pxOutput.value()).render,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("MainComponent")
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}