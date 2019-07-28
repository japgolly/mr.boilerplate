package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react.vdom.html_<^._

/** Not all styles, just shared ones */
object Styles {

  private final val gridInput  = "i"
  private final val gridGen    = "g"
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

  val inputOuter  = ^.gridArea := gridInput
  val genOuter    = ^.gridArea := gridGen
  val outputOuter = ^.gridArea := gridOutput

  val header = TagMod(
    ^.fontSize := "150%",
    ^.fontWeight.bold,
    ^.color := "#c00",
    ^.marginBottom := "0.3em",
  )

  val checkbox =
    ^.marginRight := "0.8ex"
}
