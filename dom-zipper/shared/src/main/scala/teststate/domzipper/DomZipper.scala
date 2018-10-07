package teststate.domzipper

import DomZipper._
import ErrorHandler.ErrorHandlerResultOps

trait DomZipper[F[_], A, Self[G[_], B] <: DomZipper[G, B, Self]] {
  
  def map[B](f: A => B): Self[F, B]

  def extend[B](f: Self[F, A] => B): Self[F, B]

  def duplicate: Self[F, Self[F, A]]

  // or .focus? or .value?
  def extract: A

  type Dom
  def dom: Dom
  def unfocus:  Self[F, Dom]

  def describe: String

  // ==================
  // Self configuration
  // ==================

  protected def self: Self[F, A]

  protected implicit def F: ErrorHandler[F]
  protected[domzipper] def htmlScrub: HtmlScrub

  def scrubHtml(f: HtmlScrub): Self[F, A]

  final def scrubHtml(f: String => String): Self[F, A] =
    scrubHtml(HtmlScrub(f))

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

  def exists(sel: String, suchThat: Self[F, A] => Boolean): Boolean =
    collect0n(sel).filter(suchThat).nonEmpty

  def findSelfOrChildWithAttribute(attr: String): F[Option[Self[F, A]]] =
    getAttribute(attr) match {
      case None    => collect01(s"*[$attr]").zippers
      case Some(_) => F pass Some(self)
    }

  // =======
  // Descent
  // =======

  def parent: F[Self[F, A]]
  def apply(name: String, sel: String, which: MofN): F[Self[F, A]]
  def child(name: String, sel: String, which: MofN): F[Self[F, A]]

  final def apply(sel: String)              : F[Self[F, A]] = apply("", sel)
  final def apply(sel: String, which: MofN) : F[Self[F, A]] = apply("", sel, which)
  final def apply(name: String, sel: String): F[Self[F, A]] = apply(name, sel, MofN.Sole)

  final def child(sel: String)              : F[Self[F, A]] = child("", sel)
  final def child(which: MofN = MofN.Sole)  : F[Self[F, A]] = child("", which)
  final def child(sel: String, which: MofN) : F[Self[F, A]] = child("", sel, which)
  final def child(name: String, sel: String): F[Self[F, A]] = child(name, sel, MofN.Sole)
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

  final class DomCollection[Z[f[_], a] <: DomZipper[f, a, Z], F[_], C[_], A](
      private[domzipper] val desc      : String,
      private[domzipper] val rawResults: Vector[Z[F, A]],
                             filterFn  : Option[Z[F, A] => Boolean],
      private[domzipper] val C         : DomCollection.Container[F, C])
      (implicit val F: ErrorHandler[F]) {

    private val result: Vector[Z[F, A]] =
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

    def extracts: F[C[A]] =
      map(_.extract)

    def mapExtract[B](f: A => B): F[C[B]] =
      F.map(extracts)(C.map(_)(f))

    def traverseExtracts[B](f: A => F[B]): F[C[B]] =
      F.flatMap(extracts)(C.traverse(_)(f))

    type Dom = Z[F, A]#Dom

    def doms: F[C[Dom]] =
      map(_.dom)

    def mapDom[B](f: Dom => B): F[C[B]] =
      F.map(doms)(C.map(_)(f))

    def traverseDoms[B](f: Dom => F[B]): F[C[B]] =
      F.flatMap(doms)(C.traverse(_)(f))

    def zippers: F[C[Z[F, A]]] =
      C(desc, result)

    def map[B](f: Z[F, A] => B): F[C[B]] =
      C(desc, result.map(f))

    @deprecated("Use .map", "2.2.0")
    def mapZippers[B](f: Z[F, A] => B): F[C[B]] =
      map(f)

    def filter(f: Z[F, A] => Boolean): DomCollection[Z, F, C, A] = {
      val f2: Z[F, A] => Boolean = filterFn.fold(f)(f0 => z => f0(z) && f(z))
      new DomCollection(desc, rawResults, Some(f2), C)
    }

    def outerHTMLs: F[C[String]] = map(_.outerHTML)
    def innerHTMLs: F[C[String]] = map(_.innerHTML)
    def innerTexts: F[C[String]] = map(_.innerText)
  }

  // ===================================================================================================================

  object DomCollection {

    def apply[Z[f[_], a] <: DomZipper[f, a, Z], F[_], C[_], A, B](desc: String,
                                                                   rawResults: Vector[A],
                                                                   C: Container[F, C])
                                                                   (addLayer: Layer[A] => Z[F, B])
                                                                   (implicit F: ErrorHandler[F]): DomCollection[Z, F, C, B] =
      new DomCollection[Z, F, C, B](
        desc,
        rawResults.map(a => addLayer(Layer("collect", desc, a))),
        None,
        C)

    trait Container[F[_], C[_]] {
      def apply[A](desc: String, es: CssSelResult[A]): F[C[A]]
      def map[A, B](c: C[A])(f: A => B): C[B]
      def traverse[A, B](c: C[A])(f: A => F[B]): F[C[B]]
      def get[A](as: C[A], idx: Int): A
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
      override def get[A](as: Option[A], idx: Int): A =
        as.get
    }

    final class Container0N[F[_]](implicit F: ErrorHandler[F]) extends Container[F, Vector] {
      override def apply[A](desc: String, es: CssSelResult[A]) =
        F pass es
      override def map[A, B](c: Vector[A])(f: A => B) =
        c map f
      override def traverse[A, B](c: Vector[A])(f: A => F[B]): F[Vector[B]] =
        vectorTraverse(c)(f)
      override def get[A](as: Vector[A], idx: Int): A =
        as(idx)
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
      override def get[A](as: Vector[A], idx: Int): A =
        as(idx)
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
