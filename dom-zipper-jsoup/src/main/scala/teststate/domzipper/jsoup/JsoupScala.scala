package teststate.domzipper.jsoup

import org.jsoup.nodes.{Element => JElement}
import scala.collection.JavaConverters._

final case class Element(underlying: JElement) {
  override def toString = underlying.toString

  def select(css: String): Vector[Element] =
    underlying.select(css).asScala.iterator.filterNot(_ == underlying).map(Element).toVector

  lazy val children: Vector[Element] =
    underlying.children().asScala.iterator.map(Element).toVector

  lazy val childrenSet: Set[Element] =
    children.toSet

  def selectChildren(css: String): Vector[Element] =
    select(css).filter(childrenSet.contains)

  def parent: Option[Element] =
    Option(underlying.parent).map(Element)

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

  def root: Element =
    underlying.root() match {
      case e: JElement => Element(e)
      case r => Element(r.ownerDocument())
    }

  def matches(sel: String): Boolean =
    root.underlying.select(sel).contains(this.underlying)
}