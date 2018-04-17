package teststate.domzipper

import org.scalajs.dom
import org.scalajs.dom.{Node, html}
import scala.reflect.ClassTag
import scala.scalajs.js
import ErrorHandler._

object DomZipperJS extends DomZipperModule {
  import AbstractCollector._

  override type Base = dom.Node
  override type NextBase = dom.Element

  type Root = html.Document

  private def rootLayer = Layer("window.document", "", dom.window.document)

  class Constructors[Next <: NextBase, Out[_]](implicit h: ErrorHandler[Out]) {
    def root(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipper[Root, Next, Out] =
      new DomZipper(Vector.empty, rootLayer, scrub)($, h)

    def apply[D <: Base](dom: D)(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipper[D, Next, Out] =
      apply("<provided>", dom)($, scrub)

    def apply[D <: Base](name: String, dom: D)(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipper[D, Next, Out] =
      new DomZipper(Vector.empty, Layer(name, "", dom), scrub)($, h)
  }

  /** DOM Zipper.
    *
    * @param $     The CSS selector engine. Usually either jQuery or Sizzle.
    * @param h     The error handler.
    * @tparam Cur  The type of the current DOM focus.
    * @tparam Next The type of all DOM children.
    * @tparam Out  The shape of all output that can potentially fail.
    */
  final class DomZipper[+Cur <: Base, Next <: NextBase, Out[_]] private[domzipper](prevLayers: Vector[Layer[Base]],
                                                                                 curLayer  : Layer[Cur],
                                                                                 htmlScrub : HtmlScrub)
                                                                                (implicit $: CssSelEngine,
                                                                                 h: ErrorHandler[Out])
      extends AbstractDomZipper[Cur, Next, Out](prevLayers, curLayer, htmlScrub) {

    override protected def setScrubHtml(f: HtmlScrub): DomZipper[Cur, Next, Out] =
      new DomZipper(prevLayers, curLayer, f)

    override def failBy[Result[_]](errorHandler: ErrorHandler[Result]): DomZipper[Cur, Next, Result] =
      new DomZipper(prevLayers, curLayer, htmlScrub)($, errorHandler)

    override protected[domzipper] def addLayer[D2 <: Node](nextLayer: DomZipperJS.Layer[D2]): DomZipper[D2, Next, Out] =
      new DomZipper(prevLayers :+ curLayer, nextLayer, htmlScrub)

    def dom: Cur =
      curLayer.dom

    def as[D2 <: Base](implicit ct: ClassTag[D2]): Out[DomZipper[D2, Next, Out]] =
      domAs[D2].map(d =>
        new DomZipper(prevLayers, curLayer.copy(dom = d), htmlScrub))

    def asHtml: Out[DomZipper[html.Element, html.Element, Out]] =
      as[html.Element].map(_.withHtmlChildren)

    def forceAs[D2 <: Base]: Out[DomZipper[D2, Next, Out]] =
      this.asInstanceOf[Out[DomZipper[D2, Next, Out]]]

    def forceChildren[A <: NextBase]: DomZipper[Cur, A, Out] =
      new DomZipper(prevLayers, curLayer, htmlScrub)

    def widenChildren[A >: Next <: NextBase]: DomZipper[Cur, A, Out] =
      forceChildren

    def withHtmlChildren: DomZipper[Cur, html.Element, Out] =
      forceChildren

    def domAs[D2 <: Base](implicit ct: ClassTag[D2]): Out[D2] =
      ct.unapply(dom) orFail s"${dom.nodeName} is not a ${ct.runtimeClass}."

    def domAsHtml: Out[html.Element] =
      domAs[html.Element]

    def forceDomAs[D2 <: Base]: D2 =
      dom.asInstanceOf[D2]

    override def collect[C[_]](sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, sel, c)

    /** Cast DOM to [[js.Dynamic]] and invoke a method expected to return `A` if successful. */
    def dynamicMethod[A](f: js.Dynamic => Any): Option[A] =
      f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[A]].toOption

    /** Cast DOM to [[js.Dynamic]], invoke a method, return the result as a `String`. */
    def dynamicString(f: js.Dynamic => Any): String =
      dynamicMethod[Any](f).fold("undefined")(_.toString)

    protected override def _outerHTML = htmlScrub run dynamicString(_.outerHTML)
    protected override def _innerHTML = htmlScrub run dynamicString(_.innerHTML)

    override def innerText: String =
      dom.textContent

    override def value: Out[String] =
      dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."

    override def checked: Out[Boolean] =
      dynamicMethod[Boolean](_.checked) orFail s".checked failed on $dom."

    /** The currently selected option in a &lt;select&gt; dropdown. */
    def selectedOption: Out[Option[html.Option]] =
      domAs[html.Select].map(s =>
        if (s.selectedIndex >= 0)
          Some(s.options(s.selectedIndex))
        else
          None
      )

    /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
    def selectedOptionText: Out[Option[String]] =
      selectedOption.map(_.map(_.text))

    def findSelfOrChildWithAttribute[DD >: Cur <: Base](attr: String)(implicit ev: DomZipper[Next, Next, Out] <:< DomZipper[DD, Next, Out]): Out[Option[DomZipper[DD, Next, Out]]] =
      dom.attributes.getNamedItem(attr) match {
        case null => collect01(s"*[$attr]").zippers.map(_.map(ev))
        case _ => Some(this)
      }
  }

  // ===================================================================================================================

  final class Collector[C[_], D <: Next, Next <: NextBase, Out[_]](from: DomZipper[_, Next, Out],
                                                                   sel: String,
                                                                   cont: Container[C, Out])
                                                                  (implicit h: ErrorHandler[Out])
      extends AbstractCollector[C, D, Next, Out](from, sel, cont) {

    def as[DD <: D]: Collector[C, DD, Next, Out] =
      this.asInstanceOf[Collector[C, DD, Next, Out]]

    def asHtml(implicit ev: html.Element <:< D): Collector[C, html.Element, html.Element, Out] =
      this.asInstanceOf[Collector[C, html.Element, html.Element, Out]]
  }
}
