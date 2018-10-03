package teststate.domzipper

import ErrorHandler._

object DomZipperPair {
  def apply[
    F[_],
    A,
    Fast[f[_]] <: DomZipper[f, _, Fast],
    Slow[f[_]] <: DomZipper[f, A, Slow]
  ](
     fast: Fast[F],
     slow: Slow[F]
   )(
    implicit F: ErrorHandler[F]
   ): DomZipperPair[F, () => F[A]] =
    full[F, () => F[A], Fast, Slow](fast, Store(slow)(F.pass), g => () => g().map(_.dom))

  def full[
    F[_],
    A,
    _FastF[f[_]] <: DomZipper[f, _, _FastF],
    _SlowF[f[_]] <: DomZipper[f, _, _SlowF]
  ](
    _fast: _FastF[F],
    _slow: Store[_SlowF[F], F[_SlowF[F]]],
    _domFn: (() => F[_SlowF[F]]) => A
   )(
    implicit _F: ErrorHandler[F]
   ): DomZipperPair[F, A] =
    new DomZipperPair[F, A] {
      override protected type FastF[f[_]] = _FastF[f]
      override protected type SlowF[f[_]] = _SlowF[f]
      override protected val fast = _fast
      override protected val slow = _slow
      override protected val domFn = _domFn
      override protected implicit val F = _F
    }
}

trait DomZipperPair[F[_], A] extends DomZipper[F, A, λ[G[_] => DomZipperPair[G, A]]] {
  import DomZipper.DomCollection

  protected type FastF[f[_]] <: DomZipper[f, _, FastF]
  protected type SlowF[f[_]] <: DomZipper[f, _, SlowF]
  protected final type Fast = FastF[F]
  protected final type Slow = SlowF[F]

  protected val fast: Fast
  protected val slow: Store[Slow, F[Slow]]
  protected val domFn: (() => F[Slow]) => A
  protected implicit val F: ErrorHandler[F]

  private def flatMapSlow(f: Slow => F[Slow]): Store[Slow, F[Slow]] =
    slow.map(_.flatMap(f))

  private def zmap(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    f(fast).map(DomZipperPair.full[F, A, FastF, SlowF](_, flatMapSlow(s), domFn))

  /*
  def getAttribute(name: String): Option[String] =
    fast.getAttribute(name)

  def apply(css: String): F[DomZipperPair[F, A]] =
    zmap(_(css), _(css))

  def parent: F[DomZipperPair[F, A]] =
    zmap(_.parent, _.parent)
  */

  override def describe = fast.describe

  // ==================
  // Self configuration
  // ==================

  protected override def self = this

  protected val htmlScrub = fast.htmlScrub

  override def scrubHtml(f: HtmlScrub): DomZipperPair[F, A]

  override def failBy[G[_]](g: ErrorHandler[G]): DomZipperPair[G, A]

  // ====================
  // DOM & DOM inspection
  // ====================

  protected override def _outerHTML = fast.outerHTML
  protected override def _innerHTML = fast.innerHTML

  override def matches     (css: String)  = fast.matches(css)
  override def getAttribute(name: String) = fast.getAttribute(name)

  override def tagName   = fast.tagName
  override def innerText = fast.innerText
  override def checked   = fast.checked
  override def classes   = fast.classes
  override def value     = fast.value

  override def dom: A =
    domFn(() => slow.extract)

  override def collect01(sel: String) = ??? //: DomCollection[Self, F, Option, A]
  override def collect0n(sel: String) = ??? //: DomCollection[Self, F, Vector, A]
  override def collect1n(sel: String) = ??? //: DomCollection[Self, F, Vector, A]

  override def children01 = ??? //: DomCollection[Self, F, Option, A]
  override def children0n = ??? //: DomCollection[Self, F, Vector, A]
  override def children1n = ??? //: DomCollection[Self, F, Vector, A]

  override def children01(sel: String) = ??? //: DomCollection[Self, F, Option, A]
  override def children0n(sel: String) = ??? //: DomCollection[Self, F, Vector, A]
  //override def children1n(sel: String) = //: DomCollection[Self, F, Vector, A]

  // TODO Dom type here should be fast dom
  //      - Add to DomZipperPair types
  //      - Split dom & collection dom types in DomZipper
  // TODO Collection zipper type mismatch
  //      - Either: Map from FastZipper => PairZipper
  //      -     or: add newDomCollection that starts with PairZipper and delegates to Fast internally
  override def children1n(sel: String): DomCollection[λ[G[_] => DomZipperPair[G, A]], F, Vector, A] =
    fast.children1n(sel)

  // =======
  // Descent
  // =======

  override def parent: F[DomZipperPair[F, A]] =
    zmap(_.parent, _.parent)

  override def apply(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
    zmap(_(name, sel, which), _(name, sel, which))

  override def child(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
  zmap(_.child(name, sel, which), _.child(name, sel, which))
}
