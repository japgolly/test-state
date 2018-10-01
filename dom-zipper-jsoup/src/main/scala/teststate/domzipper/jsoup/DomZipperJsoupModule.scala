package teststate.domzipper.jsoup

import teststate.domzipper._
import org.jsoup.nodes.Document
import ErrorHandler._

object DomZipperJsoupModule extends DomZipperModule {
  import AbstractCollector._

  override type Base = Element
  override type NextBase = Element

  final class Constructors[Out[_]](implicit h: ErrorHandler[Out]) {

    def apply(name: String, e: Element)(implicit scrub: HtmlScrub): DomZipper[Element, Element, Out] =
      new DomZipper(Vector.empty, Layer(name, "", e), scrub)(h)

    def apply(name: String, e: org.jsoup.nodes.Element)(implicit scrub: HtmlScrub): DomZipper[Element, Element, Out] =
      apply(name, Element(e))

    def apply(doc: Document)(implicit scrub: HtmlScrub): DomZipper[Element, Element, Out] =
      apply("root", doc)

    def body(doc: Document)(implicit scrub: HtmlScrub): DomZipper[Element, Element, Out] =
      apply("body", doc.body())

  }

  private val cssSelJsoup: CssSelEngine =
    CssSelEngine((css, parent) => parent.select(css))

  // ===================================================================================================================

  /** DOM Zipper through Jsoup.
    *
    * @param h The error handler.
    * @tparam Out The shape of all output that can potentially fail.
    */
  final class DomZipper[+Cur <: Base, Next <: NextBase, Out[_]] private[jsoup](prevLayers: Vector[Layer[Base]],
                                                                               curLayer: Layer[Cur],
                                                                               htmlScrub: HtmlScrub)
                                                                              (implicit h: ErrorHandler[Out])
    extends AbstractDomZipper[Cur, Next, Out](prevLayers, curLayer, htmlScrub)(cssSelJsoup, h) {

    override protected def setScrubHtml(f: HtmlScrub): DomZipper[Cur, Next, Out] =
      new DomZipper(prevLayers, curLayer, f)

    override def failBy[Result[_]](errorHandler: ErrorHandler[Result]): DomZipper[Cur, Next, Result] =
      new DomZipper(prevLayers, curLayer, htmlScrub)(errorHandler)

    override protected[domzipper] def addLayer[NewCur <: Element](nextLayer: Layer[NewCur]): DomZipper[NewCur, Next, Out] =
      new DomZipper(prevLayers :+ curLayer, nextLayer, htmlScrub)

    override protected def collect[C[_]](sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, sel, runCssQuery(sel), c, None)

    override protected def collectChildren[C[_]](desc: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, desc, dom.children, c, None)

    override protected def collectChildren[C[_]](desc: String, sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, desc, dom.selectChildren(sel), c, None)

    override protected def _parent: Out[Element] =
      h.option(dom.parent, s"$dom doesn't have a parent.")

    def dom: Element =
      curLayer.dom

    override def matches(sel: String): Out[Boolean] =
      dom.matches(sel)

    override def getAttribute(name: String): Option[String] =
      dom.attr(name)

    override def needAttribute(name: String): Out[String] =
      h.option(getAttribute(name), s"${dom.getTagName} doesn't have attribute $name")

    protected override def _outerHTML =
      htmlScrub.run(dom.outerHtml)

    protected override def _innerHTML =
      htmlScrub.run(dom.innerHtml)

    override def innerText: String =
      dom.innerText

    override def value: Out[String] =
      Option(dom.underlying.`val`()) orFail s".value failed on <${dom.getTagName}>."

    override def checked: Out[Boolean] =
      dom.isSelected

    override def classes: Set[String] =
      dom.classNames

    // TODO ↓ These methods are copied from the Selenium zipper. Move into base.

    /** The currently selected option in a &lt;select&gt; dropdown. */
    def selectedOption: Out[Collector[Option, Next, Next, Out]] =
      dom.getTagName.toUpperCase match {
        case "SELECT" => collect01("option[selected]")
        case x        => h.fail(s"<$x> is not a <SELECT>")
      }

    /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
    def selectedOptionText: Out[Option[String]] =
      selectedOption.flatMap(_.mapDoms(_.innerText))

    def findSelfOrChildWithAttribute[DD >: Cur <: Base](attr: String)(implicit ev: DomZipper[Next, Next, Out] <:< DomZipper[DD, Next, Out]): Out[Option[DomZipper[DD, Next, Out]]] =
      getAttribute(attr) match {
        case None => collect01(s"*[$attr]").zippers.map(_.map(ev))
        case Some(_) => Some(this)
      }
  }

  // ===================================================================================================================

  final class Collector[C[_], D <: Next, Next <: NextBase, Out[_]](from: DomZipper[_, Next, Out],
                                                                   desc: String,
                                                                   rawResult: CssSelResult,
                                                                   cont: Container[C, Out],
                                                                   colFilter: Option[NextBase => Boolean])
                                                                  (implicit h: ErrorHandler[Out])
      extends AbstractCollector[C, D, Next, Out](from, desc, rawResult, cont, colFilter) {

    override protected def withFilter(colFilter: Option[NextBase => Boolean]): Collector[C, D, Next, Out] =
      new Collector(from, desc, rawResult, cont, colFilter)
  }
}
