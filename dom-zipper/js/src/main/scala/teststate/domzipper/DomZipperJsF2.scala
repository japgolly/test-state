package teststate.domzipper

import org.scalajs.dom
import org.scalajs.dom.html
import scala.reflect.ClassTag
import scala.scalajs.js
import DomZipper2.{CssSelEngine, CssSelResult, DomCollection, Layer}
import ErrorHandler.{ErrorHandlerOptionOps, ErrorHandlerResultOps, Id}
import JsDomExt._

object DomZipperJsF2 {
  type Dom = dom.Element

  type DomCollection[F[_], C[_]] = DomZipper2.DomCollection[DomZipperJsF2, F, C, Dom]

  def safeCastDom[F[_], D <: Dom](dom: Dom)(implicit ct: ClassTag[D], F: ErrorHandler[F]): F[D] =
    ct.unapply(dom) orFail s"${dom.nodeName} is not a ${ct.runtimeClass}."

  def liftNode[F[_]](n: dom.Node)(implicit F: ErrorHandler[F]): F[Dom] =
    n match {
      case e: dom.Element => F pass e
      case x              => F fail s"Not an element: $x"
    }

//  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {
//
//    def root(implicit $: CssSelEngine[Dom, Dom], scrub: HtmlScrub): DomZipperJsF2[F] =
//      apply("window.document.children(0)", dom.window.document.children(0))
//
//    def body(implicit $: CssSelEngine[Dom, Dom], scrub: HtmlScrub): DomZipperJsF2[F] =
//      apply("window.document.body", dom.window.document.body)
//
//    def apply(dom: Dom)(implicit $: CssSelEngine[Dom, Dom], scrub: HtmlScrub): DomZipperJsF2[F] =
//      apply("<provided>", dom)
//
//    def apply(name: String, dom: Dom)(implicit $: CssSelEngine[Dom, Dom], scrub: HtmlScrub): DomZipperJsF2[F] =
//      new DomZipperJsF2(Vector.empty, Layer(name, "", dom))
//  }
}

import DomZipperJsF2.{liftNode, safeCastDom}

final class DomZipperJsF2[F[_], A](override protected val prevLayers: Vector[Layer[DomZipperJsF2.Dom]],
                                   override protected val curLayer: Layer[DomZipperJsF2.Dom],
                                   override protected val peek: ((Vector[Layer[DomZipperJsF2.Dom]], Layer[DomZipperJsF2.Dom])) => A,
                                 )(implicit
                                   override protected val $: CssSelEngine[DomZipperJsF2.Dom, DomZipperJsF2.Dom],
                                   override protected[domzipper] val htmlScrub: HtmlScrub,
                                   override protected val F: ErrorHandler[F]
                                 ) extends DomZipperBase2.WithStore[F, A, DomZipperJsF2] {

  override type Dom = DomZipperJsF2.Dom

  override protected def newStore[B](pos: Pos, peek: Peek[B]): DomZipperJsF2[F, B] =
    new DomZipperJsF2(pos._1, pos._2, peek)

  override def unfocus =
    new DomZipperJsF2(prevLayers, curLayer, _._2.dom)

  override protected def self =
    this

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): DomZipperJsF2[G, A] =
    new DomZipperJsF2(prevLayers, curLayer, peek)($, h, g)

  override protected def _parent: F[Dom] =
    liftNode(dom.parentNode)

  override protected def _outerHTML: String =
    dynamicString(_.outerHTML)

  override protected def _innerHTML: String =
    dynamicString(_.innerHTML)

  private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, A] =
    DomCollection[DomZipperJsF2, F, C, Dom, A](desc, result, C)(addLayer)

  protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, A] =
    newDomCollection(sel, runCssQuery(sel), C)

  private def childIterator: Iterator[Dom] =
    dom.childNodes.iterator.collect {
      case e: org.scalajs.dom.Element => e
    }

  protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, A] =
    newDomCollection(desc, childIterator.toVector, C)

  protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, A] = {
    val all = runCssQuery(sel).toSet
    newDomCollection(desc, childIterator.filter(all.contains).toVector, C)
  }

  override def matches(css: String): F[Boolean] =
    F pass dom.matches(css)

  override def getAttribute(name: String): Option[String] =
    Option(dom.attributes.getNamedItem(name)).map(_.value)

  override def tagName: String =
    dom.tagName

  override def innerText: String =
    dom.textContent.trim

  override def checked: F[Boolean] =
    dynamicMethod[Boolean](_.checked) orFail s".checked failed on $dom."

  override def classes: Set[String] =
    dom match {
      case h: html.Element =>
        val c = h.classList
        (0 until c.length).map(c.apply)(collection.breakOut)
      case _ =>
        Set.empty
    }

  override def value: F[String] =
    dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."

  /** Cast DOM to [[js.Dynamic]] and invoke a method expected to return `A` if successful. */
  def dynamicMethod[A](f: js.Dynamic => Any): Option[A] =
    f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[A]].toOption

  /** Cast DOM to [[js.Dynamic]], invoke a method, return the result as a `String`. */
  def dynamicString(f: js.Dynamic => Any): String =
    dynamicMethod[Any](f).fold("undefined")(_.toString)

  def domAs[D <: Dom](implicit ct: ClassTag[D]): F[D] =
    safeCastDom[F, D](dom)

  def domAsHtml: F[html.Element] =
    domAs[html.Element]

  def forceDomAs[D <: Dom]: D =
    dom.asInstanceOf[D]

  /** The currently selected option in a &lt;select&gt; dropdown. */
  def selectedOption: F[Option[html.Option]] =
    domAs[html.Select].map(s =>
      if (s.selectedIndex >= 0)
        Some(s.options(s.selectedIndex))
      else
        None
    )

  /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
  def selectedOptionText: F[Option[String]] =
    selectedOption.map(_.map(_.text))
}
