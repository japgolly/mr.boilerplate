package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react.vdom.html_<^._

object Styles {

  private final val gridInput = "i"
  private final val gridGen = "g"
  private final val gridOutput = "o"

  val mainOuter = TagMod(
    ^.display.grid,
    ^.gridTemplateAreas(
      s"$gridInput $gridOutput",
      s"$gridGen   $gridOutput",
    ),
    ^.gridTemplateColumns := "1fr 1fr",
    ^.gridTemplateRows := "1fr auto",
    ^.padding := "1em",
    ^.gridGap := "1em",
    ^.minHeight := "100vh",
    ^.maxHeight := "100vh",
    ^.height := "100vh",
  )

  val inputOuter = TagMod(
    ^.gridArea := gridInput,
    ^.cls := "form-group",
  )

  val inputTextarea = TagMod(
    ^.width := "100%",
    ^.height := "100%",
    ^.cls := "form-control",
    ^.fontFamily := "monospace",
    ^.backgroundColor := "#eee",
    ^.color := "#000",
    ^.overflow.auto,
  )

  val genOuter = TagMod(
    ^.gridArea := gridGen,
  )

  val genBody = TagMod(
    ^.paddingLeft := "4ex",
  )

  val outputOuter = TagMod(
    ^.gridArea := gridOutput,
    ^.overflow.auto,
  )

  val highlight = TagMod(
    ^.borderRadius := "4px",
    ^.minHeight := "100%",
  )

  val checkbox =
    ^.marginRight := "0.8ex"
}
