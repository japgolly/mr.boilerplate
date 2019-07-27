package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js

object Highlight {

  def apply(input: String): VdomElement =
    Component(input)

  private def render(input: String) = {
    val html = js.Dynamic.global.hljs.highlight("scala", input, true).value.asInstanceOf[String]
    <.pre(
      ^.cls := "hljs scala",
      Styles.highlight,
      <.code(
        ^.dangerouslySetInnerHtml := html))
  }

  val Component = ScalaComponent.builder[String]("Highlight")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}