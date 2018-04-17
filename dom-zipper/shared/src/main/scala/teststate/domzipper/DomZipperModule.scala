package teststate.domzipper

import ErrorHandler._

trait DomZipperModule {
  type Base
  type NextBase <: Base

  final type CssSelEngine =
    DomZipperModule.CssSelEngine[Base, NextBase]

  final def CssSelEngine(run: (String, Base) => CssSelResult): CssSelEngine =
    DomZipperModule.CssSelEngine(run)

  final type CssSelResult =
    DomZipperModule.CssSelResult[NextBase]

  final protected type Layer[+Dom <: Base] =
    DomZipperModule.Layer[Base, Dom]

  final protected def Layer[Dom <: Base](name: String, sel: String, dom: Dom): Layer[Dom] =
    DomZipperModule.Layer(name, sel, dom)

  import AbstractCollector._

  // ===================================================================================================================

  type DomZipper[+D <: Base, Next <: NextBase, Out[_]] <: AbstractDomZipper[D, Next, Out]

  protected def newDomZipper[D <: Base, Next <: NextBase, Out[_]](prevLayers: Vector[Layer[Base]],
                                                                  curLayer: Layer[D],
                                                                  htmlScrub: HtmlScrub)
                                                                 (implicit $: CssSelEngine,
                                                                  h: ErrorHandler[Out]): DomZipper[D, Next, Out]

  /** DOM Zipper.
    *
    * @tparam D        The type of the current DOM focus.
    * @tparam Next     The type of all DOM children.
    * @tparam Out      The shape of all output that can potentially fail.
    */
  abstract class AbstractDomZipper[+D <: Base, Next <: NextBase, Out[_]] protected (
      prevLayers: Vector[Layer[Base]],
      curLayer: Layer[D],
      htmlScrub: HtmlScrub)
     (implicit $: CssSelEngine,
      h: ErrorHandler[Out]) { self: DomZipper[D, Next, Out] =>

    protected final implicit def autoPass[A](a: A): Out[A] =
      h pass a

    // ==================
    // Self configuration
    // ==================

    final def scrubHtml(f: HtmlScrub): DomZipper[D, Next, Out] =
      newDomZipper(prevLayers, curLayer, htmlScrub >> f)

    final def scrubHtml(f: String => String): DomZipper[D, Next, Out] =
      scrubHtml(HtmlScrub(f))

    final def failBy[Result[_]](errorHandler: ErrorHandler[Result]): DomZipper[D, Next, Result] =
      newDomZipper(prevLayers, curLayer, htmlScrub)($, errorHandler)

    final def failToOption: DomZipper[D, Next, Option]   = failBy(ReturnOption)
    final def failToEither: DomZipper[D, Next, ErrMsgOr] = failBy(ReturnEither)
    final def throwErrors : DomZipper[D, Next, Id]       = failBy(Throw)

    // =======
    // Descent
    // =======

    final def directSelect(sel: String): CssSelResult =
      $.run(sel, dom)

    final def apply(sel: String): Out[DomZipper[Next, Next, Out]] =
      apply("", sel)

    final def apply(sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] =
      apply("", sel, which)

    final def apply(name: String, sel: String): Out[DomZipper[Next, Next, Out]] =
      apply(name, sel, MofN.Sole)

    final def apply(name: String, sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] = {
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

    private[domzipper] final def addLayer[D2 <: Base](nextLayer: Layer[D2]): DomZipper[D2, Next, Out] =
      newDomZipper(prevLayers :+ curLayer, nextLayer, htmlScrub)

    private final def failMsg(msg: String): String =
      msg + "\n" + describeLoc

    private final def allLayers =
      prevLayers :+ curLayer

    final def describeLoc: String =
      s"DESC: ${allLayers.iterator.map(_.display) mkString " → "}\nHTML: $outerHTML"

    // ====================
    // DOM & DOM inspection
    // ====================

    final def dom: D =
      curLayer.dom

    protected def _outerHTML: String
    protected def _innerHTML: String
    final def outerHTML: String = htmlScrub run _outerHTML
    final def innerHTML: String = htmlScrub run _innerHTML

    def innerText: String

    def value: Out[String]

    def checked: Out[Boolean]

    private final def collect[C[_]](sel: String, c: Container[C, Out]) =
      newCollector[C, Next, Next, Out](this, sel, c)

    final def collect01(sel: String): Collector[Option, Next, Next, Out] = collect(sel, new Container01)
    final def collect0n(sel: String): Collector[Vector, Next, Next, Out] = collect(sel, new Container0N)
    final def collect1n(sel: String): Collector[Vector, Next, Next, Out] = collect(sel, new Container1N)

    final def exists(sel: String): Boolean =
      collect0n(sel).nonEmpty

    final def editables01 = collect01(DomZipperModule.EditableSel)
    final def editables0n = collect0n(DomZipperModule.EditableSel)
    final def editables1n = collect1n(DomZipperModule.EditableSel)
  }

  // ===================================================================================================================

  type Collector[C[_], D <: Next, Next <: NextBase, Out[_]] <: AbstractCollector[C, D, Next, Out]

  protected def newCollector[C[_], D <: Next, Next <: NextBase, Out[_]](from: DomZipper[_, Next, Out],
                                                                        sel: String,
                                                                        cont: Container[C, Out])
                                                                       (implicit h: ErrorHandler[Out]): Collector[C, D, Next, Out]

  abstract class AbstractCollector[C[_], D <: Next, Next <: NextBase, Out[_]] protected (
      from: DomZipper[_, Next, Out],
      sel: String,
      cont: Container[C, Out])
     (implicit h: ErrorHandler[Out]) {

    // Eager! DOM could change!
    private final val result = from.directSelect(sel)

    final def isEmpty: Boolean =
      result.isEmpty

    final def nonEmpty: Boolean =
      !isEmpty

    final def size: Int =
      result.length

    final def doms: Out[C[D]] = {
      val e1: Out[C[NextBase]] = cont(sel, result)
      val e2: Out[C[D]]        = e1.asInstanceOf[Out[C[D]]]
      e2
    }

    private final def addLayer(d: D): DomZipper[D, Next, Out] =
      from.addLayer(Layer("collect", sel, d))

    final def zippers: Out[C[DomZipper[D, Next, Out]]] =
      doms.map(cont.map(_)(addLayer))

    final def mapDoms[A](f: D => A): Out[C[A]] =
      doms.map(cont.map(_)(f))

    final def mapZippers[A](f: DomZipper[D, Next, Out] => A): Out[C[A]] =
      mapDoms(d => f(addLayer(d)))

    final def outerHTMLs[A]: Out[C[String]] = mapZippers(_.outerHTML)
    final def innerHTMLs[A]: Out[C[String]] = mapZippers(_.innerHTML)
    final def innerTexts[A]: Out[C[String]] = mapZippers(_.innerText)
  }

  object AbstractCollector {
    trait Container[C[_], Out[_]] {
      def apply(sel: String, es: CssSelResult): Out[C[NextBase]]
      def map[A, B](c: C[A])(f: A => B): C[B]
    }

    final class Container01[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Option, Out] {
      override def apply(sel: String, es: CssSelResult) =
        es.length match {
          case 0 => h pass None
          case 1 => h pass Some(es.head)
          case n => h fail s"$n matches found for: $sel"
        }
      override def map[A, B](c: Option[A])(f: A => B) =
        c map f
    }

    final class Container0N[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Vector, Out] {
      override def apply(sel: String, es: CssSelResult) =
        h pass es
      override def map[A, B](c: Vector[A])(f: A => B) =
        c map f
    }

    final class Container1N[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Vector, Out] {
      override def apply(sel: String, es: CssSelResult) =
        if (es.isEmpty)
          h fail s"No matches found for: $sel"
        else
          h pass es
      override def map[A, B](c: Vector[A])(f: A => B) =
        c map f
    }
  }
}

// =====================================================================================================================

object DomZipperModule {

  final case class CssSelEngine[I, O](run: (String, I) => CssSelResult[O]) extends AnyVal

  type CssSelResult[O] = Vector[O]

  def displayNameAndSel(name: String, sel: String): String =
    (Option(name).filter(_.nonEmpty), Option(sel).filter(_.nonEmpty)) match {
      case (Some(n), Some(s)) => s"$n [$s]"
      case (None, Some(s)) => s
      case (Some(n), None) => n
      case (None, None) => "?"
    }

  final case class Layer[Base, +Dom <: Base](name: String, sel: String, dom: Dom) {
    def display: String =
      displayNameAndSel(name, sel)
  }

  /** A CSS selector that finds any currently editable DOM. */
  val EditableSel: String =
    List("input", "textarea", "select")
      .map(_ + ":not(:disabled)") // :not(:read-only) | Firefox errors out when :read-only is used
      .mkString(",")

}