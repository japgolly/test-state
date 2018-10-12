package teststate.domzipper

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js
import teststate.util.CssUtil

trait JsDomExt {

  implicit def testStateExtJsDomDocument(a: dom.Document): JsDomExt.DocumentExt = new JsDomExt.DocumentExt(a)
  implicit def testStateExtJsDomElement (a: dom.Element) : JsDomExt.ElementExt  = new JsDomExt.ElementExt(a)
  implicit def testStateExtJsDomNodeList(a: dom.NodeList): JsDomExt.NodeListExt = new JsDomExt.NodeListExt(a)

}

object JsDomExt extends JsDomExt {

  private val matchesFn: (dom.Element, String) => Boolean =
    if (js.Dynamic.global.Element.prototype.matches.asInstanceOf[js.UndefOr[Any]].isDefined)
      (e, s) => e.asInstanceOf[js.Dynamic].matches(s).asInstanceOf[Boolean]
    else
      (e, s) => e.asInstanceOf[js.Dynamic].msMatchesSelector(s).asInstanceOf[Boolean]

  final class DocumentExt(private val self: dom.Document) extends AnyVal {
    def addStyleTag(content: String): Unit = {
      val style = self.createElement("style").asInstanceOf[html.Style]
      style.`type` = "text/css"
      style.innerHTML = content
      self.getElementsByTagName("head")(0).appendChild(style)
    }

    def disableCssAnimation(disableTransitions: Boolean = true,
                            disableTransforms : Boolean = true,
                            disableAnimation  : Boolean = true): Unit = {
      val css = CssUtil.disableCssAnimation(
        disableTransitions = disableTransitions,
        disableTransforms  = disableTransforms,
        disableAnimation   = disableAnimation)
      addStyleTag(css)
    }
  }

  final class ElementExt(private val self: dom.Element) extends AnyVal {
    def matches(sel: String): Boolean =
      matchesFn(self, sel)
  }

  final class NodeListExt(private val self: dom.NodeList) extends AnyVal {
    def iterator: Iterator[dom.Node] =
      (0 until self.length).iterator.map(self.apply)
  }

}
