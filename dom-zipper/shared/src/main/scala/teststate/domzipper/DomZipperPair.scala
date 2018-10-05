package teststate.domzipper

import ErrorHandler._

object DomZipperPair {
  def apply[F[_],
            Fast[f[_]] <: DomZipper[f, FD, Fast], FD,
            Slow[f[_]] <: DomZipper[f, SD, Slow], SD]
            (fast: Fast[F], slow: Slow[F])
            (implicit F: ErrorHandler[F]): DomZipperPair[F, () => F[SD]] =
    full[F, Fast, FD, Slow, SD, () => F[SD]](fast, () => F.pass(slow), identity)

  def full[
    F[_],
    _FastF[f[_]] <: DomZipper[f, _FD, _FastF], _FD,
    _SlowF[f[_]] <: DomZipper[f, _SD, _SlowF], _SD,
    A
  ](
    _fast: _FastF[F],
    _slow: () => F[_SlowF[F]],
    _domFn: (() => F[_SD]) => A
   )(
    implicit _F: ErrorHandler[F]
   ): DomZipperPair[F, A] =
    new DomZipperPair[F, A] {
      override protected type FastF[f[_]] = _FastF[f]
      override protected type SlowF[f[_]] = _SlowF[f]
      override protected type FD = _FD
      override protected type SD = _SD
      override protected val fast = _fast
      override protected val slow = _slow
      override protected val domFn = _domFn
      override protected implicit val F = _F
    }
}

trait DomZipperPair[F[_], A] extends DomZipper[F, A, λ[G[_] => DomZipperPair[G, A]]] {
  import DomZipper.DomCollection

  protected type FD
  protected type SD
  protected type FastF[f[_]] <: DomZipper[f, FD, FastF]
  protected type SlowF[f[_]] <: DomZipper[f, SD, SlowF]
  protected final type Fast = FastF[F]
  protected final type Slow = SlowF[F]

  protected val fast: Fast
  protected val slow: () => F[Slow]
  protected val domFn: (() => F[SD]) => A
  protected implicit val F: ErrorHandler[F]

  private def zmap(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    f(fast).map(DomZipperPair.full[F, FastF, FD, SlowF, SD, A](_, () => slow().flatMap(s), domFn))

  private def cmap[C[_]](runF: Fast => DomCollection[FastF, F, C, FD],
                         runS: Slow => DomCollection[SlowF, F, C, SD]): DomCollection[λ[G[_] => DomZipperPair[G, A]], F, C, A] = {
    val colF = runF(fast)
    lazy val colS = slow().map(runS)
    val C = colF.C
    val rawResults = Vector.tabulate(colF.size) { i =>
      val f = colF.rawResults(i)
      lazy val s = colS.flatMap(_.zippers).map(C.get(_, i))
      DomZipperPair.full[F, FastF, FD, SlowF, SD, A](f, () => s, domFn)
    }

    new DomCollection[λ[G[_] => DomZipperPair[G, A]], F, C, A](
      desc       = colF.desc,
      rawResults = rawResults,
      filterFn   = None,
      C          = C
    )
  }

  // ===================================================================================================================

  override def describe = fast.describe

  protected override def self = this

  protected[domzipper] val htmlScrub = fast.htmlScrub

  override def scrubHtml(f: HtmlScrub): DomZipperPair[F, A] = {
    lazy val s = slow().map(_.scrubHtml(f))
    DomZipperPair.full[F, FastF, FD, SlowF, SD, A](fast.scrubHtml(f), () => s, domFn)
  }

  protected override def _outerHTML = fast.outerHTML
  protected override def _innerHTML = fast.innerHTML

  override def matches     (css: String)  = fast.matches(css)
  override def getAttribute(name: String) = fast.getAttribute(name)

  override def tagName   = fast.tagName
  override def innerText = fast.innerText
  override def checked   = fast.checked
  override def classes   = fast.classes
  override def value     = fast.value

  override def dom = domFn(() => slow().map(_.dom))

  override def parent: F[DomZipperPair[F, A]] =
    zmap(_.parent, _.parent)

  override def apply(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
    zmap(_(name, sel, which), _(name, sel, which))

  override def child(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
    zmap(_.child(name, sel, which), _.child(name, sel, which))

  override def collect01(sel: String) = cmap(_.collect01(sel), _.collect01(sel))
  override def collect0n(sel: String) = cmap(_.collect0n(sel), _.collect0n(sel))
  override def collect1n(sel: String) = cmap(_.collect1n(sel), _.collect1n(sel))

  override def children01(sel: String) = cmap(_.children01(sel), _.children01(sel))
  override def children0n(sel: String) = cmap(_.children0n(sel), _.children0n(sel))
  override def children1n(sel: String) = cmap(_.children1n(sel), _.children1n(sel))

  override def children01 = cmap(_.children01, _.children01)
  override def children0n = cmap(_.children0n, _.children0n)
  override def children1n = cmap(_.children1n, _.children1n)
}
