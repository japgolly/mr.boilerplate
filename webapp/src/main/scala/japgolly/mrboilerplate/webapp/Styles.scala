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
    ^.padding := "0.5em 1em 1em 1em",
    ^.gridColumnGap := "1.5em",
    ^.gridRowGap := "2.2em",
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
    ^.height := "calc(100% - 2em)",
    ^.cls := "form-control",
    ^.fontFamily := "monospace",
    ^.backgroundColor := "#eee",
    ^.color := "#000",
    ^.overflow.auto,
  )

  val genOuter = TagMod(
    ^.gridArea := gridGen,
  )

  val genBlock = TagMod(
    ^.marginBottom := "1em",
  )

  val genOptionRow = TagMod(
    ^.marginTop := "0.1em",
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
    ^.height := "calc(100% - 2em - 15px)",
    ^.margin := "0",
  )

  val header = TagMod(
    ^.fontSize := "150%",
    ^.fontWeight.bold,
    ^.color := "#c00",
    ^.marginBottom := "0.3em",
  )

  val checkbox =
    ^.marginRight := "0.8ex"
}
