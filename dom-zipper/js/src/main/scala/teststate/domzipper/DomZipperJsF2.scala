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

import DomZipperJsF2.{Dom, liftNode, safeCastDom}

final class DomZipperJsF2[F[_], A](protected val prevLayers: Vector[Layer[Dom]],
                                   protected val curLayer: Layer[Dom],
                                   A: Dom => A,
                                 )(implicit
                                   protected val $: CssSelEngine[Dom, Dom],
                                   override protected[domzipper] val htmlScrub: HtmlScrub,
                                   override protected val F: ErrorHandler[F]
                                 ) extends DomZipper2[F, A, DomZipperJsF2] {
  private val cssCondStart = "(^|, *)".r
  private def cssPrepend_>(a: String) = cssCondStart.replaceAllIn(a, "$1> ")

  type Self[G[_], B] = DomZipperJsF2[G, B]

  override def map[B](f: A => B): DomZipperJsF2[F, B] =
    ???

  override def extend[B](f: DomZipperJsF2[F, A] => B): DomZipperJsF2[F, B] =
    ???

  override def duplicate: DomZipperJsF2[F, DomZipperJsF2[F, A]] =
    ???



  private def allLayers =
    prevLayers :+ curLayer

  override def describe: String =
    s"DESC: ${allLayers.iterator.map(_.display) mkString " -> "}\nHTML: $outerHTML"

  override protected def self =
    this

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): Self[G, A] =
    new DomZipperJsF2(prevLayers, curLayer, A)($, h, g)

  override def scrubHtml(f: HtmlScrub): Self[F, A] =
    copySelf(htmlScrub >> f, F)

  def failBy[G[_]](g: ErrorHandler[G]): Self[G, A] =
    copySelf(htmlScrub, g)

  def failToOption: Self[Option           , A] = failBy(ErrorHandler.ReturnOption)
  def failToEither: Self[Either[String, ?], A] = failBy(ErrorHandler.ReturnEither)
  def throwErrors : Self[Id               , A] = failBy(ErrorHandler.Throw)

  protected[domzipper] def addLayer(nextLayer: Layer[Dom]) =
    ??? ///new DomZipperJsF2(prevLayers :+ curLayer, nextLayer)

  override def dom: A =
    A(curLayer.dom)

  override def collect01(sel: String): DomCollection[Self, F, Option, A] = ??? //collect(sel, F.XC01)
  override def collect0n(sel: String): DomCollection[Self, F, Vector, A] = ??? //collect(sel, F.XC0N)
  override def collect1n(sel: String): DomCollection[Self, F, Vector, A] = ??? //collect(sel, F.XC1N)

  override def children01: DomCollection[Self, F, Option, A] = ??? //collectChildren(">*", F.XC01)
  override def children0n: DomCollection[Self, F, Vector, A] = ??? //collectChildren(">*", F.XC0N)
  override def children1n: DomCollection[Self, F, Vector, A] = ??? //collectChildren(">*", F.XC1N)

  override def children01(sel: String): DomCollection[Self, F, Option, A] = ??? //collectChildren(cssPrepend_>(sel), sel, F.XC01)
  override def children0n(sel: String): DomCollection[Self, F, Vector, A] = ??? //collectChildren(cssPrepend_>(sel), sel, F.XC0N)
  override def children1n(sel: String): DomCollection[Self, F, Vector, A] = ??? //collectChildren(cssPrepend_>(sel), sel, F.XC1N)

  lazy val parent: F[Self[F, A]] =
    F.map(_parent)(a => addLayer(Layer("parent", ":parent", a)))

  def runCssQuery(sel: String): CssSelResult[A] =
    ??? //$.run(sel, curLayer.dom)

  override def apply(name: String, sel: String, which: MofN): F[Self[F, A]] = {
//    val results = runCssQuery(sel)
//    if (results.length != which.n)
//      F fail {
//        val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
//        failMsg(s"${q}uery failed: [$sel]. Expected ${which.n} results, not ${results.length}.")
//      }
//    else
//      F pass {
//        val nextDom = results(which.m - 1)
//        val nextLayer = Layer(name, sel, nextDom)
//        addLayer(nextLayer)
//      }
    ???
  }

  override def child(name: String, sel: String, which: MofN): F[Self[F, A]] = {
//    val results = if (sel.isEmpty) children1n else children1n(sel)
//    F.flatMap(results.zippers) { zippers =>
//      if (zippers.length != which.n)
//        F fail {
//          val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
//          failMsg(s"${q}uery failed: [${results.desc}]. Expected ${which.n} results, not ${zippers.length}.")
//        }
//      else
//        F pass zippers(which.m - 1)
//    }
    ???
  }

  private def failMsg(msg: String): String =
    msg + "\n" + describe

  protected def _parent: F[Dom] =
    ??? //liftNode(dom.parentNode)

  override protected def _outerHTML: String =
    dynamicString(_.outerHTML)

  override protected def _innerHTML: String =
    dynamicString(_.innerHTML)

  private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, Dom] =
    DomCollection[DomZipperJsF2, F, C, Dom](desc, result, C)(addLayer)

  protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, Dom] =
    ??? //newDomCollection(sel, runCssQuery(sel), C)

  private def childIterator: Iterator[Dom] = ???
//    dom.childNodes.iterator.collect {
//      case e: org.scalajs.dom.Element => e
//    }

  protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, Dom] =
    newDomCollection(desc, childIterator.toVector, C)

  protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperJsF2, F, C, Dom] = {
//    val all = runCssQuery(sel).toSet
//    newDomCollection(desc, childIterator.filter(all.contains).toVector, C)
    ???
  }

  override def matches(css: String): F[Boolean] =
    ??? //F pass dom.matches(css)

  override def getAttribute(name: String): Option[String] =
    ??? //Option(dom.attributes.getNamedItem(name)).map(_.value)

  override def tagName: String =
    ??? //dom.tagName

  override def innerText: String =
    ??? //dom.textContent.trim

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

//  def domAs[D <: Dom](implicit ct: ClassTag[D]): F[D] =
//    safeCastDom[F, D](dom)
//
//  def domAsHtml: F[html.Element] =
//    domAs[html.Element]
//
//  def forceDomAs[D <: Dom]: D =
//    dom.asInstanceOf[D]
//
//  /** The currently selected option in a &lt;select&gt; dropdown. */
//  def selectedOption: F[Option[html.Option]] =
//    domAs[html.Select].map(s =>
//      if (s.selectedIndex >= 0)
//        Some(s.options(s.selectedIndex))
//      else
//        None
//    )
//
//  /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
//  def selectedOptionText: F[Option[String]] =
//    selectedOption.map(_.map(_.text))
}
