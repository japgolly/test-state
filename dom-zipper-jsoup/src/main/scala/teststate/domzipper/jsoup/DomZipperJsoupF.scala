package teststate.domzipper.jsoup

import teststate.domzipper._
import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import DomZipper.{CssSelResult, DomCollection, Layer}
import ErrorHandler.{ErrorHandlerOptionOps, ErrorHandlerResultOps}

object DomZipperJsoupF {

  type Dom = JsoupElement

  type DomCollection[F[_], C[_], A] = DomZipper.DomCollection[DomZipperJsoupF, F, C, Dom, A]

  type CssSelEngine = DomZipper.CssSelEngine[Dom, Dom]

  private implicit val cssSelJsoup: CssSelEngine =
    DomZipper.CssSelEngine((css, parent) => parent.select(css))

  private val rootDomFn: DomZipperBase.Layers[Dom] => Dom =
    _.latest.dom

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {
    import org.jsoup.nodes.Element

    def apply(name: String, e: Element)(implicit scrub: HtmlScrub): DomZipperJsoupF[F, Dom] =
      new DomZipperJsoupF(DomZipperBase.Layers init Layer(name, "", JsoupElement(e)), rootDomFn)

    def apply(doc: Document)(implicit scrub: HtmlScrub): DomZipperJsoupF[F, Dom] =
      apply("root", doc)

    def body(body: Document)(implicit scrub: HtmlScrub): DomZipperJsoupF[F, Dom] =
      apply("body", body)

    def parseHtml(html: String)(implicit scrub: HtmlScrub): DomZipperJsoupF[F, Dom] =
      apply(Jsoup.parse(html))

    def parseBody(bodyHtml: String)(implicit scrub: HtmlScrub): DomZipperJsoupF[F, Dom] = {
      // Jsoup is weird
      val j = Jsoup.parse(s"<html>$bodyHtml</html>").selectFirst("body")
      apply("body", j)
    }
  }
}

import DomZipperJsoupF.{CssSelEngine, Dom}

final class DomZipperJsoupF[F[_], A](override protected val layers: DomZipperBase.Layers[Dom],
                                     override protected val peek: DomZipperBase.Layers[Dom] => A
                                   )(implicit
                                     override protected val $: CssSelEngine,
                                     override protected[domzipper] val htmlScrub: HtmlScrub,
                                     override protected val F: ErrorHandler[F]
                                   ) extends DomZipperBase.WithStore[F, Dom, A, DomZipperJsoupF] {

  override protected def newStore[B](pos: Pos, peek: Peek[B]): DomZipperJsoupF[F, B] =
    new DomZipperJsoupF(pos, peek)

  override def isCapable(c: DomZipper.Capability) = c match {
    case DomZipper.Capability.RadioButtonChecked => false
  }

  override def unmap =
    new DomZipperJsoupF(layers, DomZipperJsoupF.rootDomFn)

  override protected def self = this

  override protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]) =
    new DomZipperJsoupF(layers, peek)($, h, g)

  private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]) =
    DomCollection[DomZipperJsoupF, F, C, Dom, Dom, A](desc, enrichErr, result, C)(addLayer)

  override protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]) =
    newDomCollection(sel, runCssQuery(sel), C)

  override protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]) =
    newDomCollection(desc, dom.children, C)

  override protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]) =
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
  def selectedOption: F[DomCollection[DomZipperJsoupF, F, Option, Dom, A]] =
    dom.getTagName.toUpperCase match {
      case "SELECT" => F pass collect01("option[selected]")
      case x        => F.fail(s"<$x> is not a <SELECT>")
    }

  /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
  def selectedOptionText: F[Option[String]] =
    selectedOption.flatMap(_.mapDoms(_.innerText))
}