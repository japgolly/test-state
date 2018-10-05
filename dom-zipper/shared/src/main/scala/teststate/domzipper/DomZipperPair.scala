package teststate.domzipper

import ErrorHandler._

object DomZipperPair {
  def apply[
    F[_],
    S,
    Fast[f[_]] <: DomZipper[f, _, Fast],
    Slow[f[_]] <: DomZipper[f, S, Slow]
  ](
     fast: Fast[F],
     slow: Slow[F]
   )(
    implicit F: ErrorHandler[F]
   ): DomZipperPair[F, () => F[S]] =
    full[F, () => F[S], Fast, Slow, S](fast, Store.fromUnit(F pass slow), identity)

  def full[
    F[_],
    A,
    _FastF[f[_]] <: DomZipper[f, _, _FastF],
    _SlowF[f[_]] <: DomZipper[f, _S, _SlowF],
    _S
  ](
    _fast: _FastF[F],
    _slow: Store[Unit, F[_SlowF[F]]],
    _domFn: (() => F[_S]) => A
   )(
    implicit _F: ErrorHandler[F]
   ): DomZipperPair[F, A] =
    new DomZipperPair[F, A] {
      override protected type FastF[f[_]] = _FastF[f]
      override protected type SlowF[f[_]] = _SlowF[f]
      override protected type S = _S
      override protected val fast = _fast
      override protected val slow = _slow
      override protected val domFn = _domFn
      override protected implicit val F = _F
    }
}

trait DomZipperPair[F[_], A] extends DomZipper[F, A, λ[G[_] => DomZipperPair[G, A]]] {
  import DomZipper.DomCollection

  protected type FD
  protected type S
  protected type FastF[f[_]] <: DomZipper[f, FD, FastF]
  protected type SlowF[f[_]] <: DomZipper[f, S, SlowF]
  protected final type Fast = FastF[F]
  protected final type Slow = SlowF[F]

  protected val fast: Fast
  protected val slow: Store[Unit, F[Slow]]
  protected val domFn: (() => F[S]) => A
  protected implicit val F: ErrorHandler[F]

  private def flatMapSlow(f: Slow => F[Slow]): Store[Unit, F[Slow]] =
    slow.map(_.flatMap(f))

  private def zmap(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    f(fast).map(DomZipperPair.full[F, A, FastF, SlowF, S](_, flatMapSlow(s), domFn))

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
    domFn(() => slow.extract.map(_.dom))

  override def collect01(sel: String) = ??? //: DomCollection[Self, F, Option, A]
  override def collect0n(sel: String) = ??? //: DomCollection[Self, F, Vector, A]
  override def collect1n(sel: String) = ??? //: DomCollection[Self, F, Vector, A]

  override def children01 = ??? //: DomCollection[Self, F, Option, A]
  override def children0n = ??? //: DomCollection[Self, F, Vector, A]
  override def children1n = ??? //: DomCollection[Self, F, Vector, A]

  override def children01(sel: String) = ??? //: DomCollection[Self, F, Option, A]
  override def children0n(sel: String) = ??? //: DomCollection[Self, F, Vector, A]
  //override def children1n(sel: String) = //: DomCollection[Self, F, Vector, A]

  type FfsIntellij[G[_]] = DomZipperPair[G, A]
//  override def children1n(sel: String): DomCollection[λ[G[_] => DomZipperPair[G, A]], F, Vector, A] =
  override def children1n(sel: String): DomCollection[FfsIntellij, F, Vector, A] = {
    val fastC = fast.children1n(sel)
    val size = fastC.size
    lazy val slowC = slow.map(_.map(_.children1n(sel))).extract
    val rawResults = Vector.tabulate(size) { i =>
      val f = fastC.rawResults(i)
      val s = Store.fromUnit(slowC.flatMap(_.zippers).map(_(i)))
      DomZipperPair.full[F, A, FastF, SlowF, S](f, s, domFn)
    }

    new DomCollection[FfsIntellij, F, Vector, A](
      desc       = fastC.desc,
      rawResults = rawResults,
      filterFn   = None,
      C          = new DomCollection.Container1N[F]
    )
  }


  // =======
  // Descent
  // =======

  override def parent: F[DomZipperPair[F, A]] =
    zmap(
      _.parent,
      _.parent)

  override def apply(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
    zmap(
      _(name, sel, which),
      _(name, sel, which))

  override def child(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
    zmap(
      _.child(name, sel, which),
      _.child(name, sel, which))
}
