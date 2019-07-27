package japgolly.mrboilerplate.webapp

import japgolly.mrboilerplate.webapp.components.MainComponent
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.document

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {
    val root = document.getElementById("root")
    val comp = MainComponent.Props().render
    comp.renderIntoDOM(root)
    ()
  }

}
