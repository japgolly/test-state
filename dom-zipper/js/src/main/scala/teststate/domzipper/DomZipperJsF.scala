package teststate.domzipper

import org.scalajs.dom
import org.scalajs.dom.html
import scala.reflect.ClassTag
import scala.scalajs.js
import teststate.domzipper.DomZipper.{CssSelResult, DomCollection, Layer}
import teststate.domzipper.ErrorHandler.ErrorHandlerOptionOps

object DomZipperJsF {
  type Dom = dom.Element

  type DomCollection[F[_], C[_], A] = DomZipper.DomCollection[DomZipperJsF, F, C, Dom, A]

  type CssSelEngine = DomZipper.CssSelEngine[Dom, Dom]

  def safeCastDom[F[_], D <: Dom](dom: Dom)(implicit ct: ClassTag[D], F: ErrorHandler[F]): F[D] =
    ct.unapply(dom) orFail s"${dom.nodeName} is not a ${ct.runtimeClass}."

  def liftNode[F[_]](n: dom.Node)(implicit F: ErrorHandler[F]): F[Dom] =
    n match {
      case e: dom.Element => F pass e
      case x              => F fail s"Not an element: $x"
    }

  private val rootDomFn: DomZipperBase.Layers[Dom] => Dom =
    _.latest.dom

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {

    def root(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJsF[F, Dom] =
      apply("window.document.children(0)", dom.window.document.children(0))

    def body(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJsF[F, Dom] =
      apply("window.document.body", dom.window.document.body)

    def apply(dom: Dom)(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJsF[F, Dom] =
      apply("<provided>", dom)

    def apply(name: String, dom: Dom)(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJsF[F, Dom] =
      new DomZipperJsF(DomZipperBase.Layers init Layer(name, "", dom), rootDomFn)

    def failBy[G[_]: ErrorHandler]: Constructors[G]   = new Constructors
    def failToOption: Constructors[Option           ] = failBy(ErrorHandler.ReturnOption)
    def failToEither: Constructors[Either[String, *]] = failBy(ErrorHandler.ReturnEither)

    type DomCollection [      C[_], A] = DomZipperJsF.DomCollection[F, C, A]
    type DomCollectionF[G[_], C[_], A] = DomZipperJsF.DomCollection[G, C, A]
  }
}

import DomZipperJsF.{CssSelEngine, Dom, liftNode, rootDomFn, safeCastDom}

final class DomZipperJsF[F[_], A](override protected val layers: DomZipperBase.Layers[Dom],
                                  override protected val peek: DomZipperBase.Layers[Dom] => A
                                )(implicit
                                  override protected val $: CssSelEngine,
                                  override protected[domzipper] val htmlScrub: HtmlScrub,
                                  override protected val F: ErrorHandler[F]
                                ) extends DomZipperBase.WithStore[F, Dom, A, DomZipperJsF] {

  override protected def newStore[B](pos: Pos, peek: Peek[B]): DomZipperJsF[F, B] =
    new DomZipperJsF(pos, peek)

  override def isCapable(c: DomZipper.Capability) = c match {
    case DomZipper.Capability.RadioButtonChecked => true
  }

  override def unmap =
    new DomZipperJsF(layers, rootDomFn)

  override protected def self = this

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): DomZipperJsF[G, A] =
    new DomZipperJsF(layers, peek)($, h, g)

  override protected def _parent: F[Dom] =
    liftNode(dom.parentNode)

  override protected def _outerHTML: String =
    dynamicString(_.outerHTML)

  override protected def _innerHTML: String =
    dynamicString(_.innerHTML)

  private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]) =
    DomCollection[DomZipperJsF, F, C, Dom, Dom, A](desc, enrichErr, result, C)(addLayer)

  protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]) =
    newDomCollection(sel, runCssQuery(sel), C)

  private def childIterator: Iterator[Dom] =
    dom.childNodes.iterator.collect {
      case e: org.scalajs.dom.Element => e
    }

  protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]) =
    newDomCollection(desc, childIterator.toVector, C)

  protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]) = {
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
        (0 until c.length).iterator.map(c.apply).toSet
      case _ =>
        Set.empty
    }

  override def value: F[String] =
    dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."

  /** Cast DOM to [[js.Dynamic]] and invoke a method expected to return `A` if successful. */
  def dynamicMethod[B](f: js.Dynamic => Any): Option[B] =
    f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[B]].toOption

  /** Cast DOM to [[js.Dynamic]], invoke a method, return the result as a `String`. */
  def dynamicString(f: js.Dynamic => Any): String =
    dynamicMethod[Any](f).fold("undefined")(_.toString)

  def domAs[D <: Dom](implicit ct: ClassTag[D]): F[D] =
    safeCastDom[F, D](dom)

  def domAsHtml: F[html.Element] =
    domAs[html.Element]

  def domAsButton: F[html.Button] =
    domAs[html.Button]

  def domAsInput: F[html.Input] =
    domAs[html.Input]

  def domAsTextArea: F[html.TextArea] =
    domAs[html.TextArea]

  def forceDomAs[D <: Dom]: D =
    dom.asInstanceOf[D]
}
