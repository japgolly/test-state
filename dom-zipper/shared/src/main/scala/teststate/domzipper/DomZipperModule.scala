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

  type DomZipper[+Cur <: Base, Next <: NextBase, Out[_]] <: AbstractDomZipper[Cur, Next, Out]

  /** DOM Zipper.
    *
    * @tparam Cur  The type of the current DOM focus.
    * @tparam Next The type of all DOM children.
    * @tparam Out  The shape of all output that can potentially fail.
    */
  abstract class AbstractDomZipper[+Cur <: Base, Next <: NextBase, Out[_]] protected (
      prevLayers: Vector[Layer[Base]],
      curLayer: Layer[Cur],
      htmlScrub: HtmlScrub)
     (implicit $: CssSelEngine,
      h: ErrorHandler[Out]) { self: DomZipper[Cur, Next, Out] =>

    protected final implicit def autoPass[A](a: A): Out[A] =
      h pass a

    // ==================
    // Self configuration
    // ==================

    protected def setScrubHtml(f: HtmlScrub): DomZipper[Cur, Next, Out]

    final def scrubHtml(f: HtmlScrub): DomZipper[Cur, Next, Out] =
      setScrubHtml(htmlScrub >> f)

    final def scrubHtml(f: String => String): DomZipper[Cur, Next, Out] =
      scrubHtml(HtmlScrub(f))

    def failBy[Result[_]](errorHandler: ErrorHandler[Result]): DomZipper[Cur, Next, Result]

    final def failToOption: DomZipper[Cur, Next, Option]   = failBy(ReturnOption)
    final def failToEither: DomZipper[Cur, Next, ErrMsgOr] = failBy(ReturnEither)
    final def throwErrors : DomZipper[Cur, Next, Id]       = failBy(Throw)

    // ====================
    // DOM & DOM inspection
    // ====================

    def getAttribute(name: String): Option[String]
    def needAttribute(name: String): Out[String]

    protected def _outerHTML: String
    protected def _innerHTML: String
    final def outerHTML: String = htmlScrub run _outerHTML
    final def innerHTML: String = htmlScrub run _innerHTML

    def innerText: String

    def value: Out[String]

    def checked: Out[Boolean]

    def classes: Set[String]

    final def exists(sel: String): Boolean =
      collect0n(sel).nonEmpty

    final def exists(sel: String, suchThat: DomZipper[Next, Next, Out] => Boolean): Boolean =
      collect0n(sel).filter(suchThat).nonEmpty

    def matches(sel: String): Out[Boolean]

    protected def collect[C[_]](sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out]
    final def collect01(sel: String): Collector[Option, Next, Next, Out] = collect(sel, new Container01)
    final def collect0n(sel: String): Collector[Vector, Next, Next, Out] = collect(sel, new Container0N)
    final def collect1n(sel: String): Collector[Vector, Next, Next, Out] = collect(sel, new Container1N)

    protected def collectChildren[C[_]](desc: String, c: Container[C, Out]): Collector[C, Next, Next, Out]
    protected def collectChildren[C[_]](desc: String, sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out]

    final def children01: Collector[Option, Next, Next, Out] = collectChildren(">*", new Container01)
    final def children0n: Collector[Vector, Next, Next, Out] = collectChildren(">*", new Container0N)
    final def children1n: Collector[Vector, Next, Next, Out] = collectChildren(">*", new Container1N)
    final def children01(sel: String): Collector[Option, Next, Next, Out] = collectChildren(cssPrepend_>(sel), sel, new Container01)
    final def children0n(sel: String): Collector[Vector, Next, Next, Out] = collectChildren(cssPrepend_>(sel), sel, new Container0N)
    final def children1n(sel: String): Collector[Vector, Next, Next, Out] = collectChildren(cssPrepend_>(sel), sel, new Container1N)

    final def editables01 = collect01(DomZipperModule.EditableSel)
    final def editables0n = collect0n(DomZipperModule.EditableSel)
    final def editables1n = collect1n(DomZipperModule.EditableSel)

    // =======
    // Descent
    // =======

    final def runCssQuery(sel: String): CssSelResult =
      $.run(sel, curLayer.dom)

    final def apply(sel: String): Out[DomZipper[Next, Next, Out]] =
      apply("", sel)

    final def apply(sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] =
      apply("", sel, which)

    final def apply(name: String, sel: String): Out[DomZipper[Next, Next, Out]] =
      apply(name, sel, MofN.Sole)

    final def apply(name: String, sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] = {
      val results = runCssQuery(sel)
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

    final def child(sel: String): Out[DomZipper[Next, Next, Out]] =
      child("", sel)

    final def child(which: MofN): Out[DomZipper[Next, Next, Out]] =
      child("", which)

    final def child(sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] =
      child("", sel, which)

    final def child(name: String, sel: String): Out[DomZipper[Next, Next, Out]] =
      child(name, sel, MofN.Sole)

    final def child(name: String, sel: String, which: MofN): Out[DomZipper[Next, Next, Out]] = {
      val results = if (sel.isEmpty) children1n else children1n(sel)
      h.flatMap(results.zippers) { zippers =>
        if (zippers.length != which.n)
          h fail {
            val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
            failMsg(s"${q}uery failed: [${results.desc}]. Expected ${which.n} results, not ${zippers.length}.")
          }
        else
          zippers(which.m - 1)
      }
    }

    protected[domzipper] def addLayer[NewCur <: Base](nextLayer: Layer[NewCur]): DomZipper[NewCur, Next, Out]

    private final def failMsg(msg: String): String =
      msg + "\n" + describeLoc

    private final def allLayers =
      prevLayers :+ curLayer

    final def describeLoc: String =
      s"DESC: ${allLayers.iterator.map(_.display) mkString " → "}\nHTML: $outerHTML"
  }

  private val cssCondStart = "(^|, *)".r
  private def cssPrepend_>(a: String) = cssCondStart.replaceAllIn(a, "$1> ")

  // ===================================================================================================================

  type Collector[C[_], D <: Next, Next <: NextBase, Out[_]] <: AbstractCollector[C, D, Next, Out]

  // Note: Herein, NextBase is manually cast to D.
  // This is for JS convenience and a typical example is NextBase=HTMLElement, D=HTMLButton
  abstract class AbstractCollector[C[_], D <: Next, Next <: NextBase, Out[_]] protected (
      from: DomZipper[_, Next, Out],
      private[teststate] final val desc: String,
      rawResult: CssSelResult,
      cont: Container[C, Out],
      colFilter: Option[NextBase => Boolean])
     (implicit h: ErrorHandler[Out]) {

    protected def withFilter(colFilter: Option[NextBase => Boolean]): Collector[C, D, Next, Out]

    private final val result: Vector[NextBase] =
      colFilter.fold(rawResult)(rawResult.filter)

    final def isEmpty: Boolean =
      result.isEmpty

    final def nonEmpty: Boolean =
      !isEmpty

    final def size: Int =
      result.length

    final def doms: Out[C[D]] = {
      val e1: Out[C[NextBase]] = cont(desc, result)
      val e2: Out[C[D]]        = e1.asInstanceOf[Out[C[D]]]
      e2
    }

    private final def addLayer(d: D): DomZipper[D, Next, Out] =
      from.addLayer(Layer("collect", desc, d))

    final def zippers: Out[C[DomZipper[D, Next, Out]]] =
      doms.map(cont.map(_)(addLayer))

    final def mapDoms[A](f: D => A): Out[C[A]] =
      doms.map(cont.map(_)(f))

    @deprecated("Use .map", "2.2.0")
    final def mapZippers[A](f: DomZipper[D, Next, Out] => A): Out[C[A]] =
      map(f)

    final def map[A](f: DomZipper[D, Next, Out] => A): Out[C[A]] =
      mapDoms(d => f(addLayer(d)))

    final def filter(f: DomZipper[D, Next, Out] => Boolean): Collector[C, D, Next, Out] = {
      val f2: NextBase => Boolean = nb => f(addLayer(nb.asInstanceOf[D]))
      val f3: NextBase => Boolean = colFilter.fold(f2)(f0 => d => f0(d) && f2(d))
      withFilter(Some(f3))
    }

    final def outerHTMLs[A]: Out[C[String]] = map(_.outerHTML)
    final def innerHTMLs[A]: Out[C[String]] = map(_.innerHTML)
    final def innerTexts[A]: Out[C[String]] = map(_.innerText)
  }

  object AbstractCollector {
    trait Container[C[_], Out[_]] {
      def apply(desc: String, es: CssSelResult): Out[C[NextBase]]
      def map[A, B](c: C[A])(f: A => B): C[B]
    }

    final class Container01[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Option, Out] {
      override def apply(desc: String, es: CssSelResult) =
        es.length match {
          case 0 => h pass None
          case 1 => h pass Some(es.head)
          case n => h fail s"$n matches found for: $desc"
        }
      override def map[A, B](c: Option[A])(f: A => B) =
        c map f
    }

    final class Container0N[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Vector, Out] {
      override def apply(desc: String, es: CssSelResult) =
        h pass es
      override def map[A, B](c: Vector[A])(f: A => B) =
        c map f
    }

    final class Container1N[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Vector, Out] {
      override def apply(desc: String, es: CssSelResult) =
        if (es.isEmpty)
          h fail s"No matches found for: $desc"
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