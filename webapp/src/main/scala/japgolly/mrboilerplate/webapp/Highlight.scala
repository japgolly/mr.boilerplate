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
      ^.height := "100%",
      ^.margin := "0",
      ^.borderRadius := "4px",
      <.code(
        ^.dangerouslySetInnerHtml := html))
  }

  val Component = ScalaComponent.builder[String]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}