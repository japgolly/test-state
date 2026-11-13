package teststate.domzipper.jsoup

import org.jsoup.nodes.Element
import org.jsoup.select.QueryParser
import scala.jdk.CollectionConverters._

final case class JsoupElement(underlying: Element) {
  override def toString = underlying.toString

  def select(css: String): Vector[JsoupElement] =
    underlying.select(css).asScala.iterator.filterNot(_ == underlying).map(JsoupElement.apply).toVector

  lazy val children: Vector[JsoupElement] =
    underlying.children().asScala.iterator.map(JsoupElement.apply).toVector

  def selectChildren(css: String): Vector[JsoupElement] = {
    val eval = QueryParser.parse(css)
    children.filter(c => eval.matches(underlying, c.underlying))
  }

  def parent: Option[JsoupElement] =
    Option(underlying.parent).map(JsoupElement.apply)

  def attr(name: String): Option[String] =
    if (underlying.hasAttr(name))
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
    underlying.hasAttr("checked")

  def innerHtml = underlying.html()
  def outerHtml = underlying.outerHtml()

  def root: JsoupElement =
    JsoupElement(underlying.root())

  def matches(sel: String): Boolean =
    underlying.is(sel)
}