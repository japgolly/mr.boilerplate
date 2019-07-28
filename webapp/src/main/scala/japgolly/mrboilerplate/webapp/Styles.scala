package japgolly.mrboilerplate.webapp

import japgolly.scalajs.react.vdom.html_<^._

/** Not all styles, just shared ones */
object Styles {

  private final val gridInput  = "i"
  private final val gridGen    = "g"
  private final val gridOutput = "o"

  private val hpad = "1em"

  val mainOuter = TagMod(
    ^.display.grid,
    ^.gridTemplateAreas(
      s"$gridInput $gridOutput",
      s"$gridGen   $gridOutput",
    ),
    ^.gridTemplateColumns := s"calc(50% - $hpad) calc(50% - $hpad)",
    ^.gridTemplateRows := "1fr auto",
    ^.padding := s"0.5em $hpad 1em $hpad",
    ^.gridGap := "1.5em",
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
