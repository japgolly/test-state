package teststate.domzipper.jsoup

import teststate.domzipper._
import org.jsoup.nodes.Document
import DomZipper.{CssSelEngine, CssSelResult, DomCollection, Layer}
import ErrorHandler.{ErrorHandlerOptionOps, ErrorHandlerResultOps}

object DomZipperJsoupF {

  type Dom = JsoupElement

  type DomCollection[F[_], C[_]] = DomZipper.DomCollection[DomZipperJsoupF, F, C, Dom]

  type CssSelEngine = DomZipper.CssSelEngine[Dom, Dom]

  private implicit val cssSelJsoup: CssSelEngine =
    CssSelEngine((css, parent) => parent.select(css))

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {
    import org.jsoup.nodes.Element

    def apply(name: String, e: Element)(implicit scrub: HtmlScrub): DomZipperJsoupF[F] =
      new DomZipperJsoupF(Vector.empty, Layer(name, "", JsoupElement(e)))

    def apply(doc: Document)(implicit scrub: HtmlScrub): DomZipperJsoupF[F] =
      apply("root", doc)

    def body(doc: Document)(implicit scrub: HtmlScrub): DomZipperJsoupF[F] =
      apply("body", doc.body())
  }
}

import DomZipperJsoupF.Dom

final class DomZipperJsoupF[F[_]](override protected val prevLayers: Vector[Layer[Dom]],
                                  override protected val curLayer: Layer[Dom]
                                 )(implicit
                                   override protected val $: CssSelEngine[Dom, Dom],
                                   override protected val htmlScrub: HtmlScrub,
                                   override protected val F: ErrorHandler[F]
                                 ) extends DomZipperBase[F, Dom, DomZipperJsoupF] {

  override protected def self = this

  override protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]) =
    new DomZipperJsoupF(prevLayers, curLayer)($, h, g)

  override protected[domzipper] def addLayer(nextLayer: Layer[Dom]) =
    new DomZipperJsoupF(prevLayers :+ curLayer, nextLayer)

  private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]): DomCollection[DomZipperJsoupF, F, C, Dom] =
    new DomCollection[DomZipperJsoupF, F, C, Dom](this, _.addLayer(_), desc, result, None, C)

  override protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsoupF, F, C, Dom] =
    newDomCollection(sel, runCssQuery(sel), C)

  override protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsoupF, F, C, Dom] =
    newDomCollection(desc, dom.children, C)

  override protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsoupF, F, C, Dom] =
    newDomCollection(desc, dom.selectChildren(sel), C)

  override protected def _parent: F[Dom] =
    F.option(dom.parent, s"$dom doesn't have a parent.")

  override def matches(sel: String): F[Boolean] =
    F pass dom.matches(sel)

  override def getAttribute(name: String): Option[String] =
    dom.attr(name)

  protected override def _outerHTML =
    htmlScrub.run(dom.outerHtml)

  protected override def _innerHTML =
    htmlScrub.run(dom.innerHtml)

  override def innerText: String =
    dom.innerText

  override def value: F[String] =
    Option(dom.underlying.`val`()) orFail s".value failed on <${dom.getTagName}>."

  override def checked: F[Boolean] =
    F pass dom.isSelected

  override def classes: Set[String] =
    dom.classNames

  override def tagName =
    dom.getTagName

  /** The currently selected option in a &lt;select&gt; dropdown. */
  def selectedOption: F[DomCollection[DomZipperJsoupF, F, Option, Dom]] =
    dom.getTagName.toUpperCase match {
      case "SELECT" => F pass collect01("option[selected]")
      case x        => F.fail(s"<$x> is not a <SELECT>")
    }

  /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
  def selectedOptionText: F[Option[String]] =
    selectedOption.flatMap(_.mapDoms(_.innerText))
}