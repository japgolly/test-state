package teststate.domzipper

import org.scalajs.dom.{Element, Node, html, window}
import scala.reflect.ClassTag
import scala.scalajs.js
import DomZipper._
import Collector._
import ErrorHandler._

object DomZipper {

  type Base = Node
  type NextBase = Element

  case class CssSelEngine(run: (String, Node) => CssSelResult) extends AnyVal

  type CssSelResult = js.Array[NextBase]

  def displayNameAndSel(name: String, sel: String): String =
    (Option(name).filter(_.nonEmpty), Option(sel).filter(_.nonEmpty)) match {
      case (Some(n), Some(s)) => s"$n [$s]"
      case (None   , Some(s)) => s
      case (Some(n), None   ) => n
      case (None   , None   ) => "?"
    }

  case class Layer[+D <: Base](name: String, sel: String, dom: D) {
    def display: String =
      displayNameAndSel(name, sel)
  }

  /** A CSS selector that finds any currently editable DOM. */
  val EditableSel: String =
    List("input", "textarea", "select")
      .map(_ + ":not(:disabled)") // :not(:read-only) | Firefox errors out when :read-only is used
      .mkString(",")

  private val idS = (s: String) => s
  private val rootLayer = Layer("window.document", "", window.document)

  class Constructors[Next <: NextBase, Out[_]](implicit h: ErrorHandler[Out]) {
    def root(implicit $: CssSelEngine): DomZipper[html.Document, Next, Out] =
      new DomZipper(Vector.empty, rootLayer, idS)($, h)
  }
}

/** DOM Zipper.
  *
  * @param $         The CSS selector engine. Usually either jQuery or Sizzle.
  * @param h         The error handler.
  * @param htmlScrub Arbitrary preprocessor applied before returning any HTML text.
  * @tparam D        The type of the current DOM focus.
  * @tparam Next     The type of all DOM children.
  * @tparam Out      The shape of all output that can potentially fail.
  */
final class DomZipper[+D <: Base, Next <: NextBase, Out[_]] private[domzipper](prevLayers: Vector[Layer[Base]],
                                                                               curLayer  : Layer[D],
                                                                               htmlScrub : String => String)
                                                                              (implicit $: CssSelEngine,
                                                                                        h: ErrorHandler[Out]) {

  private implicit def autoPass[A](a: A): Out[A] =
    h pass a

  // ==================
  // Self configuration
  // ==================

  def scrubHtml(f: String => String): DomZipper[D, Next, Out] =
    new DomZipper(prevLayers, curLayer, f compose htmlScrub)

  def failBy[Result[_]](errorHandler: ErrorHandler[Result]): DomZipper[D, Next, Result] =
    new DomZipper(prevLayers, curLayer, htmlScrub)($, errorHandler)

  def failToOption: DomZipper[D, Next, Option]   = failBy(ReturnOption)
  def failToEither: DomZipper[D, Next, ErrMsgOr] = failBy(ReturnEither)
  def throwErrors : DomZipper[D, Next, Id]       = failBy(Throw)

  def as[D2 <: Base](implicit ct: ClassTag[D2]): Out[DomZipper[D2, Next, Out]] =
    domAs[D2].map(d =>
      new DomZipper(prevLayers, curLayer.copy(dom = d), htmlScrub))

  def asHtml: Out[DomZipper[html.Element, Next, Out]] =
    as[html.Element]

  def forceAs[D2 <: Base]: Out[DomZipper[D2, Next, Out]] =
    this.asInstanceOf[Out[DomZipper[D2, Next, Out]]]

  // =======
  // Descent
  // =======

  def directSelect(sel: String): CssSelResult =
    $.run(sel, dom)

  def apply(sel: String): Out[DomZipper[Next, Next, Out]] =
    apply("", sel)

  def apply(sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] =
    apply("", sel, which)

  def apply(name: String, sel: String): Out[DomZipper[Next, Next, Out]] =
    apply(name, sel, MofN.Sole)

  def apply(name: String, sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] = {
    val results = directSelect(sel)
    if (results.length != which.n)
      h fail {
        val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
        failMsg(s"${q}uery failed: [$sel]. Expected ${which.n} results, not ${results.length}.")
      }
    else {
      // More correct would be to ClassTag[Next].unapply here but all but guaranteed to pass.
      // Given how often this is executed let's avoid the overhead.
      val nextDom = results(which.m - 1).asInstanceOf[Next]
      val nextLayer = Layer(name, sel, nextDom)
      addLayer(nextLayer)
    }
  }

  private[domzipper] def addLayer[D2 <: Base](nextLayer: Layer[D2]): DomZipper[D2, Next, Out] =
    new DomZipper(prevLayers :+ curLayer, nextLayer, htmlScrub)

  private def failMsg(msg: String): String =
    msg + "\n" + describeLoc

  private def allLayers =
    prevLayers :+ curLayer

  def describeLoc: String =
    s"DESC: ${allLayers.iterator.map(_.display) mkString " → "}\nHTML: $outerHTML"

  // ====================
  // DOM & DOM inspection
  // ====================

  def dom: D =
    curLayer.dom

  def domAs[D2 <: Base](implicit ct: ClassTag[D2]): Out[D2] =
    ct.unapply(dom) orFail s"${dom.nodeName} is not a ${ct.runtimeClass}."

  def domAsHtml: Out[html.Element] =
    domAs[html.Element]

  def forceDomAs[D2 <: Base]: D2 =
    dom.asInstanceOf[D2]

  /** Cast DOM to [[js.Dynamic]] and invoke a method expected to return `A` if successful. */
  def dynamicMethod[A](f: js.Dynamic => Any): Option[A] =
    f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[A]].toOption

  /** Cast DOM to [[js.Dynamic]], invoke a method, return the result as a `String`. */
  def dynamicString(f: js.Dynamic => Any): String =
    dynamicMethod[Any](f).fold("undefined")(_.toString)

  def outerHTML: String = htmlScrub(dynamicString(_.outerHTML))
  def innerHTML: String = htmlScrub(dynamicString(_.innerHTML))
  def innerText: String = dom.textContent

  def value: Out[String] =
    dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."

  def checked: Out[Boolean] =
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

  private def collect[C[_]](sel: String, c: Container[C, Out]) =
    new Collector[C, Next, Out](this, sel, c)

  def collect01(sel: String): Collector[Option, Next, Out] = collect(sel, new Container01)
  def collect0n(sel: String): Collector[Vector, Next, Out] = collect(sel, new Container0N)
  def collect1n(sel: String): Collector[Vector, Next, Out] = collect(sel, new Container1N)

  def exists(sel: String): Boolean =
    collect0n(sel).nonEmpty

  def findSelfOrChildWithAttribute[DD >: D <: Base](attr: String)(implicit ev: DomZipper[Next, Next, Out] <:< DomZipper[DD, Next, Out]): Out[Option[DomZipper[DD, Next, Out]]] =
    dom.attributes.getNamedItem(attr) match {
      case null => collect01(s"*[$attr]").zippers.map(_.map(ev))
      case _    => Some(this)
    }

  def editables01 = collect01(EditableSel)
  def editables0n = collect0n(EditableSel)
  def editables1n = collect1n(EditableSel)
}