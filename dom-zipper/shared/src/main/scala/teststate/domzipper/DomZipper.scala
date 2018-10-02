package teststate.domzipper

import DomZipper._
import ErrorHandler.Id

trait DomZipper[F[_], A] {
  import DomCollection.Container

  final type Collection[C[_]] = DomCollection[Self, F, C, A]

  private final def allLayers =
    prevLayers :+ curLayer

  final def describeLoc: String =
    s"DESC: ${allLayers.iterator.map(_.display) mkString " -> "}\nHTML: $outerHTML"

  // ==================
  // Self configuration
  // ==================

  type Self[G[_]] <: DomZipper[G, A]

  protected def self: Self[F]

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): Self[G]

  protected implicit val $: CssSelEngine[A, A]
  protected implicit val F: ErrorHandler[F]
  protected val htmlScrub: HtmlScrub
  protected val prevLayers: Vector[Layer[A]]
  protected val curLayer: Layer[A]
  protected[domzipper] def addLayer(nextLayer: Layer[A]): Self[F]

  final def scrubHtml(f: HtmlScrub): Self[F] =
    copySelf(htmlScrub >> f, F)

  final def scrubHtml(f: String => String): Self[F] =
    scrubHtml(HtmlScrub(f))

  final def failBy[G[_]](g: ErrorHandler[G]): Self[G] =
    copySelf(htmlScrub, g)

  final def failToOption: Self[Option           ] = failBy(ErrorHandler.ReturnOption)
  final def failToEither: Self[Either[String, ?]] = failBy(ErrorHandler.ReturnEither)
  final def throwErrors : Self[Id               ] = failBy(ErrorHandler.Throw)

  // ====================
  // DOM & DOM inspection
  // ====================

  protected def _outerHTML: String
  protected def _innerHTML: String
  protected def collect[C[_]](sel: String, C: Container[F, C]): Collection[C]
  protected def collectChildren[C[_]](desc: String, C: Container[F, C]): Collection[C]
  protected def collectChildren[C[_]](desc: String, sel: String, C: Container[F, C]): Collection[C]

  def matches(css: String): F[Boolean]

  def getAttribute(name: String): Option[String]

  def tagName: String

  def innerText: String

  def checked: F[Boolean]

  def classes: Set[String]

  def value: F[String]

  final def dom: A =
    curLayer.dom

  final def needAttribute(name: String): F[String] =
    F.option(getAttribute(name), s"$tagName doesn't have attribute $name")

  final def outerHTML: String = htmlScrub run _outerHTML
  final def innerHTML: String = htmlScrub run _innerHTML

  final def collect01(sel: String): Collection[Option] = collect(sel, new DomCollection.Container01)
  final def collect0n(sel: String): Collection[Vector] = collect(sel, new DomCollection.Container0N)
  final def collect1n(sel: String): Collection[Vector] = collect(sel, new DomCollection.Container1N)

  final def children01: Collection[Option] = collectChildren(">*", new DomCollection.Container01)
  final def children0n: Collection[Vector] = collectChildren(">*", new DomCollection.Container0N)
  final def children1n: Collection[Vector] = collectChildren(">*", new DomCollection.Container1N)

  final def children01(sel: String): Collection[Option] = collectChildren(cssPrepend_>(sel), sel, new DomCollection.Container01)
  final def children0n(sel: String): Collection[Vector] = collectChildren(cssPrepend_>(sel), sel, new DomCollection.Container0N)
  final def children1n(sel: String): Collection[Vector] = collectChildren(cssPrepend_>(sel), sel, new DomCollection.Container1N)

  final def editables01 = collect01(EditableSel)
  final def editables0n = collect0n(EditableSel)
  final def editables1n = collect1n(EditableSel)

  final def exists(sel: String): Boolean =
    collect0n(sel).nonEmpty

  final def exists(sel: String, suchThat: Self[F] => Boolean): Boolean =
    collect0n(sel).filter(suchThat).nonEmpty

  final def findSelfOrChildWithAttribute(attr: String): F[Option[Self[F]]] =
    getAttribute(attr) match {
      case None    => collect01(s"*[$attr]").zippers
      case Some(_) => F pass Some(self)
    }

  // =======
  // Descent
  // =======

  protected def _parent: F[A]

  final def runCssQuery(sel: String): CssSelResult[A] =
    $.run(sel, curLayer.dom)

  final def apply(sel: String): F[Self[F]] =
    apply("", sel)

  final def apply(sel: String, which: MofN): F[Self[F]] =
    apply("", sel, which)

  final def apply(name: String, sel: String): F[Self[F]] =
    apply(name, sel, MofN.Sole)

  final def apply(name: String, sel: String, which: MofN): F[Self[F]] = {
    val results = runCssQuery(sel)
    if (results.length != which.n)
      F fail {
        val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
        failMsg(s"${q}uery failed: [$sel]. Expected ${which.n} results, not ${results.length}.")
      }
    else
      F pass {
        val nextDom = results(which.m - 1)
        val nextLayer = Layer(name, sel, nextDom)
        addLayer(nextLayer)
      }
  }

  final def child(sel: String): F[Self[F]] =
    child("", sel)

  final def child(which: MofN = MofN.Sole): F[Self[F]] =
    child("", which)

  final def child(sel: String, which: MofN): F[Self[F]] =
    child("", sel, which)

  final def child(name: String, sel: String): F[Self[F]] =
    child(name, sel, MofN.Sole)

  final def child(name: String, sel: String, which: MofN): F[Self[F]] = {
    val results = if (sel.isEmpty) children1n else children1n(sel)
    F.flatMap(results.zippers) { zippers =>
      if (zippers.length != which.n)
        F fail {
          val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
          failMsg(s"${q}uery failed: [${results.desc}]. Expected ${which.n} results, not ${zippers.length}.")
        }
      else
        F pass zippers(which.m - 1)
    }
  }

  final lazy val parent: F[Self[F]] =
    F.map(_parent)(a => addLayer(Layer("parent", ":parent", a)))

  private final def failMsg(msg: String): String =
    msg + "\n" + describeLoc

}

// =====================================================================================================================

object DomZipper {

  final case class CssSelEngine[I, O](run: (String, I) => CssSelResult[O]) extends AnyVal

  type CssSelResult[O] = Vector[O]

  def displayNameAndSel(name: String, sel: String): String =
    (Option(name).filter(_.nonEmpty), Option(sel).filter(_.nonEmpty)) match {
      case (Some(n), Some(s)) => s"$n [$s]"
      case (None, Some(s)) => s
      case (Some(n), None) => n
      case (None, None) => "?"
    }

  // TODO Stop using sel, use something free
  final case class Layer[A](name: String, sel: String, dom: A) {
    def display: String =
      displayNameAndSel(name, sel)
  }

  private val cssCondStart = "(^|, *)".r
  private def cssPrepend_>(a: String) = cssCondStart.replaceAllIn(a, "$1> ")

  /** A CSS selector that finds any currently editable DOM. */
  val EditableSel: String =
    List("input", "textarea", "select")
      .map(_ + ":not(:disabled)") // :not(:read-only) | Firefox errors out when :read-only is used
      .mkString(",")

  // ===================================================================================================================

  final class DomCollection[Z[f[_]] <: DomZipper[f, A], F[_], C[_], A](from      : Z[F],
                                                                       addLayerFn: (Z[F], Layer[A]) => Z[F],
                                                                       val desc  : String,
                                                                       rawResult : CssSelResult[A],
                                                                       filterFn  : Option[A => Boolean],
                                                                       C         : DomCollection.Container[F, C])
                                                                      (implicit val F: ErrorHandler[F]) {

    private val result: Vector[A] =
      filterFn.fold(rawResult)(rawResult.filter)

    def isEmpty: Boolean =
      result.isEmpty

    def nonEmpty: Boolean =
      !isEmpty

    def size: Int =
      result.length

    def doms: F[C[A]] =
      C(desc, result)

    def traverse[B](f: A => F[B]): F[C[B]] =
      F.flatMap(doms)(C.traverse(_)(f))

    private def addLayer(a: A): Z[F] =
      addLayerFn(from, Layer("collect", desc, a))

    def zippers: F[C[Z[F]]] =
      mapDoms(addLayer)

    def mapDoms[B](f: A => B): F[C[B]] =
      F.map(doms)(C.map(_)(f))

    def map[B](f: Z[F] => B): F[C[B]] =
      mapDoms(d => f(addLayer(d)))

    @deprecated("Use .map", "2.2.0")
    def mapZippers[B](f: Z[F] => B): F[C[B]] =
      map(f)

    def filter(f: Z[F] => Boolean): DomCollection[Z, F, C, A] = {
      val f2: A => Boolean = a => f(addLayer(a))
      val f3: A => Boolean = filterFn.fold(f2)(f0 => d => f0(d) && f2(d))
      new DomCollection(from, addLayerFn, desc, rawResult, Some(f3), C)
    }

    def outerHTMLs: F[C[String]] = map(_.outerHTML)
    def innerHTMLs: F[C[String]] = map(_.innerHTML)
    def innerTexts: F[C[String]] = map(_.innerText)
  }

  // ===================================================================================================================

  object DomCollection {
    trait Container[F[_], C[_]] {
      def apply[A](desc: String, es: CssSelResult[A]): F[C[A]]
      def map[A, B](c: C[A])(f: A => B): C[B]
      def traverse[A, B](c: C[A])(f: A => F[B]): F[C[B]]
    }

    final class Container01[F[_]](implicit F: ErrorHandler[F]) extends Container[F, Option] {
      override def apply[A](desc: String, es: CssSelResult[A]) =
        es.length match {
          case 0 => F pass None
          case 1 => F pass Some(es.head)
          case n => F fail s"$n matches found for: $desc"
        }
      override def map[A, B](c: Option[A])(f: A => B) =
        c map f
      override def traverse[A, B](c: Option[A])(f: A => F[B]): F[Option[B]] =
        c match {
          case Some(a) => F.map(f(a))(Some(_))
          case None    => F pass None
        }
    }

    final class Container0N[F[_]](implicit F: ErrorHandler[F]) extends Container[F, Vector] {
      override def apply[A](desc: String, es: CssSelResult[A]) =
        F pass es
      override def map[A, B](c: Vector[A])(f: A => B) =
        c map f
      override def traverse[A, B](c: Vector[A])(f: A => F[B]): F[Vector[B]] =
        vectorTraverse(c)(f)
    }

    final class Container1N[F[_]](implicit F: ErrorHandler[F]) extends Container[F, Vector] {
      override def apply[A](desc: String, es: CssSelResult[A]) =
        if (es.isEmpty)
          F fail s"No matches found for: $desc"
        else
          F pass es
      override def map[A, B](c: Vector[A])(f: A => B) =
        c map f
      override def traverse[A, B](c: Vector[A])(f: A => F[B]): F[Vector[B]] =
        vectorTraverse(c)(f)
    }

    private def vectorTraverse[F[_], A, B](c: Vector[A])(f: A => F[B])(implicit F: ErrorHandler[F]): F[Vector[B]] = {
      def go(i: Int, v: Vector[B]): F[Vector[B]] =
        if (i == c.length)
          F pass v
        else {
          val a = c(i)
          F.flatMap(f(a))(b => go(i + 1, v :+ b))
        }
      go(0, Vector.empty)
    }

  }
}
