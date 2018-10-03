package teststate.domzipper.jsoup

import org.jsoup.nodes.Element
import scala.collection.JavaConverters._

final case class JsoupElement(underlying: Element) {
  override def toString = underlying.toString

  def select(css: String): Vector[JsoupElement] =
    underlying.select(css).asScala.iterator.filterNot(_ == underlying).map(JsoupElement).toVector

  lazy val children: Vector[JsoupElement] =
    underlying.children().asScala.iterator.map(JsoupElement).toVector

  lazy val childrenSet: Set[JsoupElement] =
    children.toSet

  def selectChildren(css: String): Vector[JsoupElement] =
    select(css).filter(childrenSet.contains)

  def parent: Option[JsoupElement] =
    Option(underlying.parent).map(JsoupElement)

  def attr(name: String): Option[String] =
    if (underlying.attributes().hasKeyIgnoreCase(name))
      Some(underlying.attr(name))
    else
      None

  def getTagName: String =
    underlying.tagName

  def innerText: String =
    if (getTagName.equalsIgnoreCase("input"))
      ""
    else
      underlying.text()

  lazy val classNames: Set[String] =
    underlying.classNames.asScala.toSet

  def isSelected: Boolean =
    underlying.attributes().hasKeyIgnoreCase("checked")

  def innerHtml = underlying.html()
  def outerHtml = underlying.outerHtml()

  def root: JsoupElement =
    underlying.root() match {
      case e: Element => JsoupElement(e)
      case r          => JsoupElement(r.ownerDocument())
    }

  def matches(sel: String): Boolean =
    root.underlying.select(sel).contains(this.underlying)
}