package teststate.domzipper

import DomZipper._
import ErrorHandler.Id

trait DomZipper[F[_], A, Self[G[_]] <: DomZipper[G, A, Self]] {

  def describe: String

  @deprecated("Use .describe", "2.3.0")
  final def describeLoc: String = describe

  // ==================
  // Self configuration
  // ==================

  protected def self: Self[F]

  protected implicit val F: ErrorHandler[F]
  protected val htmlScrub: HtmlScrub

  def scrubHtml(f: HtmlScrub): Self[F]

  final def scrubHtml(f: String => String): Self[F] =
    scrubHtml(HtmlScrub(f))

  def failBy[G[_]](g: ErrorHandler[G]): Self[G]

  final def failToOption: Self[Option           ] = failBy(ErrorHandler.ReturnOption)
  final def failToEither: Self[Either[String, ?]] = failBy(ErrorHandler.ReturnEither)
  final def throwErrors : Self[Id               ] = failBy(ErrorHandler.Throw)

  // ====================
  // DOM & DOM inspection
  // ====================

  protected def _outerHTML: String
  protected def _innerHTML: String

  def matches(css: String): F[Boolean]

  def getAttribute(name: String): Option[String]

  def tagName: String

  def innerText: String

  def checked: F[Boolean]

  def classes: Set[String]

  def value: F[String]

  def dom: A

  final def needAttribute(name: String): F[String] =
    F.option(getAttribute(name), s"$tagName doesn't have attribute $name")

  final def outerHTML: String = htmlScrub run _outerHTML
  final def innerHTML: String = htmlScrub run _innerHTML

  def collect01(sel: String): DomCollection[Self, F, Option, A]
  def collect0n(sel: String): DomCollection[Self, F, Vector, A]
  def collect1n(sel: String): DomCollection[Self, F, Vector, A]

  def children01: DomCollection[Self, F, Option, A]
  def children0n: DomCollection[Self, F, Vector, A]
  def children1n: DomCollection[Self, F, Vector, A]

  def children01(sel: String): DomCollection[Self, F, Option, A]
  def children0n(sel: String): DomCollection[Self, F, Vector, A]
  def children1n(sel: String): DomCollection[Self, F, Vector, A]

  final def editables01 = collect01(EditableSel)
  final def editables0n = collect0n(EditableSel)
  final def editables1n = collect1n(EditableSel)

  def exists(sel: String): Boolean =
    collect0n(sel).nonEmpty

  def exists(sel: String, suchThat: Self[F] => Boolean): Boolean =
    collect0n(sel).filter(suchThat).nonEmpty

  def findSelfOrChildWithAttribute(attr: String): F[Option[Self[F]]] =
    getAttribute(attr) match {
      case None    => collect01(s"*[$attr]").zippers
      case Some(_) => F pass Some(self)
    }

  // =======
  // Descent
  // =======

  def parent: F[Self[F]]
  def apply(name: String, sel: String, which: MofN): F[Self[F]]
  def child(name: String, sel: String, which: MofN): F[Self[F]]

  final def apply(sel: String)              : F[Self[F]] = apply("", sel)
  final def apply(sel: String, which: MofN) : F[Self[F]] = apply("", sel, which)
  final def apply(name: String, sel: String): F[Self[F]] = apply(name, sel, MofN.Sole)

  final def child(sel: String)              : F[Self[F]] = child("", sel)
  final def child(which: MofN = MofN.Sole)  : F[Self[F]] = child("", which)
  final def child(sel: String, which: MofN) : F[Self[F]] = child("", sel, which)
  final def child(name: String, sel: String): F[Self[F]] = child(name, sel, MofN.Sole)
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

  /** A CSS selector that finds any currently editable DOM. */
  val EditableSel: String =
    List("input", "textarea", "select")
      .map(_ + ":not(:disabled)") // :not(:read-only) | Firefox errors out when :read-only is used
      .mkString(",")

  // ===================================================================================================================

  final class DomCollection[Z[f[_]] <: DomZipper[f, A, Z], F[_], C[_], A](
      private[domzipper] val desc      : String,
      private[domzipper] val rawResults: Vector[Z[F]],
                             filterFn  : Option[Z[F] => Boolean],
                             C         : DomCollection.Container[F, C])
      (implicit val F: ErrorHandler[F]) {

    private val result: Vector[Z[F]] =
      filterFn match {
        case None    => rawResults
        case Some(f) => rawResults.filter(f)
      }

    def isEmpty: Boolean =
      result.isEmpty

    def nonEmpty: Boolean =
      !isEmpty

    def size: Int =
      result.length

    def doms: F[C[A]] =
      map(_.dom)

    def mapDoms[B](f: A => B): F[C[B]] =
      F.map(doms)(C.map(_)(f))

    def traverse[B](f: A => F[B]): F[C[B]] =
      F.flatMap(doms)(C.traverse(_)(f))

    def zippers: F[C[Z[F]]] =
      C(desc, result)

    def map[B](f: Z[F] => B): F[C[B]] =
      C(desc, result.map(f))

    @deprecated("Use .map", "2.2.0")
    def mapZippers[B](f: Z[F] => B): F[C[B]] =
      map(f)

    def filter(f: Z[F] => Boolean): DomCollection[Z, F, C, A] = {
      val f2: Z[F] => Boolean = filterFn.fold(f)(f0 => z => f0(z) && f(z))
      new DomCollection(desc, rawResults, Some(f2), C)
    }

    def outerHTMLs: F[C[String]] = map(_.outerHTML)
    def innerHTMLs: F[C[String]] = map(_.innerHTML)
    def innerTexts: F[C[String]] = map(_.innerText)
  }

  // ===================================================================================================================

  object DomCollection {

    def apply[Z[f[_]] <: DomZipper[f, A, Z], F[_], C[_], A](desc: String,
                                                            rawResults: Vector[A],
                                                            C: Container[F, C])
                                                           (addLayer: Layer[A] => Z[F])
                                                           (implicit F: ErrorHandler[F]): DomCollection[Z, F, C, A] =
      new DomCollection[Z, F, C, A](
        desc,
        rawResults.map(a => addLayer(Layer("collect", desc, a))),
        None,
        C)

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
