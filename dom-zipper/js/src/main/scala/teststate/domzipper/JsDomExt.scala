package teststate.domzipper

import org.scalajs.dom
import org.scalajs.dom.html
import teststate.util.CssUtil

trait JsDomExt {

  implicit def testStateExtJsDomDocument(a: dom.Document): JsDomExt.DocumentExt = new JsDomExt.DocumentExt(a)

}

object JsDomExt extends JsDomExt {

  final class DocumentExt(private val self: dom.Document) extends AnyVal {
    def addStyleTag(content: String): Unit = {
      val style = self.createElement("style").asInstanceOf[html.Style]
      style.`type` = "text/css"
      style.innerHTML = content
      self.getElementsByTagName("head")(0).appendChild(style)
      ()
    }

    def disableCssAnimation(disableAnimation  : Boolean = true,
                            disableTransitions: Boolean = true,
                            disableTransforms : Boolean = true): Unit = {
      val css = CssUtil.disableCssAnimation(
        disableAnimation   = disableAnimation,
        disableTransitions = disableTransitions,
        disableTransforms  = disableTransforms)
      addStyleTag(css)
    }
  }

}
