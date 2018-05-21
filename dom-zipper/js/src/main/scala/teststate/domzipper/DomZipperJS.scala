package teststate.domzipper

import org.scalajs.dom
import org.scalajs.dom.html
import scala.reflect.ClassTag
import scala.scalajs.js
import ErrorHandler._
import JsDomExt._

object DomZipperJS extends DomZipperModule {
  import AbstractCollector._

  override type Base = dom.Node
  override type NextBase = dom.Element

  type Root = html.Document

  private def rootLayer = Layer("window.document", "", dom.window.document)

  final class Constructors[Next <: NextBase, Out[_]](implicit h: ErrorHandler[Out]) {
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

    override protected[domzipper] def addLayer[NewCur <: Base](nextLayer: DomZipperJS.Layer[NewCur]): DomZipper[NewCur, Next, Out] =
      new DomZipper(prevLayers :+ curLayer, nextLayer, htmlScrub)

    def dom: Cur =
      curLayer.dom

    def as[NewCur <: Base](implicit ct: ClassTag[NewCur]): Out[DomZipper[NewCur, Next, Out]] =
      domAs[NewCur].map(d =>
        new DomZipper(prevLayers, curLayer.copy(dom = d), htmlScrub))

    def asHtml: Out[DomZipper[html.Element, html.Element, Out]] =
      as[html.Element].map(_.withHtmlChildren)

    def forceAs[NewCur <: Base]: Out[DomZipper[NewCur, Next, Out]] =
      this.asInstanceOf[Out[DomZipper[NewCur, Next, Out]]]

    def forceChildren[A <: NextBase]: DomZipper[Cur, A, Out] =
      new DomZipper(prevLayers, curLayer, htmlScrub)

    def widenChildren[A >: Next <: NextBase]: DomZipper[Cur, A, Out] =
      forceChildren

    def withHtmlChildren: DomZipper[Cur, html.Element, Out] =
      forceChildren

    def domAs[NewCur <: Base](implicit ct: ClassTag[NewCur]): Out[NewCur] =
      ct.unapply(dom) orFail s"${dom.nodeName} is not a ${ct.runtimeClass}."

    def domAsHtml: Out[html.Element] =
      domAs[html.Element]

    def forceDomAs[NewCur <: Base]: NewCur =
      dom.asInstanceOf[NewCur]

    override def matches(sel: String): Out[Boolean] =
      dom match {
        case e: org.scalajs.dom.Element => e.matches(sel)
        case x => h fail s"Not an element: " + x
      }

    override def collect[C[_]](sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, sel, runCssQuery(sel), c, None)

    private def childIterator: Iterator[Next] =
      dom.childNodes.iterator.collect {
        case e: org.scalajs.dom.Element => e.asInstanceOf[Next] // asInstanceOf! oh god...
      }

    override protected def collectChildren[C[_]](desc: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, desc, childIterator.toVector, c, None)

    override protected def collectChildren[C[_]](desc: String, sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out] = {
      val all = runCssQuery(sel).toSet
      new Collector(this, desc, childIterator.filter(all.contains).toVector, c, None)
    }

    /** Cast DOM to [[js.Dynamic]] and invoke a method expected to return `A` if successful. */
    def dynamicMethod[A](f: js.Dynamic => Any): Option[A] =
      f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[A]].toOption

    /** Cast DOM to [[js.Dynamic]], invoke a method, return the result as a `String`. */
    def dynamicString(f: js.Dynamic => Any): String =
      dynamicMethod[Any](f).fold("undefined")(_.toString)

    protected override def _outerHTML = htmlScrub run dynamicString(_.outerHTML)
    protected override def _innerHTML = htmlScrub run dynamicString(_.innerHTML)

    override def innerText: String =
      dom.textContent.trim

    override def value: Out[String] =
      dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."

    override def checked: Out[Boolean] =
      dynamicMethod[Boolean](_.checked) orFail s".checked failed on $dom."

    override def classes: Set[String] =
      dom match {
        case h: html.Element =>
          val c = h.classList
          (0 until c.length).map(c.apply)(collection.breakOut)
        case _ =>
          Set.empty
      }

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

    def findSelfOrChildWithAttribute(attr: String): Out[Option[DomZipper[Base, Next, Out]]] =
      dom.attributes.getNamedItem(attr) match {
        case null => collect01(s"*[$attr]").zippers.map(z => z)
        case _ => Some(this)
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

    def as[DD <: D]: Collector[C, DD, Next, Out] =
      this.asInstanceOf[Collector[C, DD, Next, Out]]

    def asHtml(implicit ev: html.Element <:< D): Collector[C, html.Element, html.Element, Out] =
      this.asInstanceOf[Collector[C, html.Element, html.Element, Out]]
  }
}
