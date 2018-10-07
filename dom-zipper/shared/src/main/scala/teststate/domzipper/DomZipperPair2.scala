package teststate.domzipper

import ErrorHandler._

/** Fusion of two DomZippers over identical content.
  *
  * One zipper ("fast") is used for all of the inspection,
  * the other zipper ("slow") is used when real DOM is needed.
  *
  * @since 2.3.0
  */
trait DomZipperPair2[F[_], A] extends DomZipper2[F, A, DomZipperPair2] {
  import DomZipper2.DomCollection
  import DomZipperPair2.FastAndSlow

  override protected implicit def F: ErrorHandler[F] = fastAndSlow.F

  protected type FD
  protected type SD
  protected type FastF[f[_], a] <: DomZipper2[f, a, FastF]
  protected type SlowF[f[_], a] <: DomZipper2[f, a, SlowF]
  protected val fastAndSlow: FastAndSlow[F, FastF, FD, SlowF, SD]
  import fastAndSlow.{fast, slow, Fast, Slow}

  protected val domFn: FastAndSlow[F, FastF, FD, SlowF, SD] => A

  private def bimap(f: Fast => Fast, s: Slow => Slow): DomZipperPair2[F, A] =
    fastAndSlow.bimap(f, s).toDomZipperPair2(domFn)

  private def bimapF(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair2[F, A]] =
    fastAndSlow.bimapF(f, s).map(_.toDomZipperPair2(domFn))

  private def cmap[C[_]](runF: Fast => DomCollection[FastF, F, C, FD],
                         runS: Slow => DomCollection[SlowF, F, C, SD]): DomCollection[DomZipperPair2, F, C, A] = {
    val colF = runF(fast)
    lazy val colS = slow().map(runS)
    val C = colF.C
    val rawResults = Vector.tabulate(colF.size) { i =>
      val f = colF.rawResults(i)
      lazy val s = colS.flatMap(_.zippers).map(C.get(_, i))
      FastAndSlow(f, () => s).toDomZipperPair2(domFn)
    }

    new DomCollection[DomZipperPair2, F, C, A](
      desc       = colF.desc,
      rawResults = rawResults,
      filterFn   = None,
      C          = C
    )
  }

  // ===================================================================================================================

  override def map[B](f: A => B): DomZipperPair2[F, B] =
    fastAndSlow.toDomZipperPair2(f compose domFn)

  override def duplicate: DomZipperPair2[F, DomZipperPair2[F, A]] =
    fastAndSlow.toDomZipperPair2(_.toDomZipperPair2(domFn))

  override def extend[B](f: DomZipperPair2[F, A] => B): DomZipperPair2[F, B] =
    duplicate.map(f)

  override def extract: A =
    domFn(fastAndSlow)

  override type Dom_ = () => F[SD]
  override def dom = fastAndSlow.rootRomFn(fastAndSlow)
  override def unfocus = fastAndSlow.toDomZipperPair2Root

  // ===================================================================================================================

  override def describe = fast.describe

  protected override def self = this

  protected[domzipper] def htmlScrub = fast.htmlScrub

  override def scrubHtml(f: HtmlScrub): DomZipperPair2[F, A] =
    bimap(_.scrubHtml(f), _.scrubHtml(f))

  protected override def _outerHTML = fast.outerHTML
  protected override def _innerHTML = fast.innerHTML

  override def matches     (css: String)  = fast.matches(css)
  override def getAttribute(name: String) = fast.getAttribute(name)

  override def tagName   = fast.tagName
  override def innerText = fast.innerText
  override def checked   = fast.checked
  override def classes   = fast.classes
  override def value     = fast.value

  // override def dom = domFn(() => slow().map(_.dom))

  override def parent: F[DomZipperPair2[F, A]] =
    bimapF(_.parent, _.parent)

  override def apply(name: String, sel: String, which: MofN): F[DomZipperPair2[F, A]] =
    bimapF(_(name, sel, which), _(name, sel, which))

  override def child(name: String, sel: String, which: MofN): F[DomZipperPair2[F, A]] =
    bimapF(_.child(name, sel, which), _.child(name, sel, which))

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

// =====================================================================================================================

object DomZipperPair2 {
  def apply[F[_],
            Fast[f[_], a] <: DomZipper2[f, a, Fast], FD,
            Slow[f[_], a] <: DomZipper2[f, a, Slow], SD]
            (fast: Fast[F, FD], slow: Slow[F, SD])
            (implicit F: ErrorHandler[F]): DomZipperPair2[F, () => F[SD]] =
    FastAndSlow(fast, () => F.pass(slow)).toDomZipperPair2Root

  final case class FastAndSlow[
    F[_],
    FastF[f[_], a] <: DomZipper2[f, a, FastF], FD,
    SlowF[f[_], a] <: DomZipper2[f, a, SlowF], SD,
  ](fast: FastF[F, FD], slow: () => F[SlowF[F, SD]])
   (implicit val F: ErrorHandler[F]) {

    type Fast = FastF[F, FD]
    type Slow = SlowF[F, SD]

    def bimap(f: Fast => Fast, s: Slow => Slow): FastAndSlow[F, FastF, FD, SlowF, SD] = {
      lazy val ss = slow().map(s)
      FastAndSlow[F, FastF, FD, SlowF, SD](f(fast), () => ss)
    }

    def bimapF(f: Fast => F[Fast], s: Slow => F[Slow]): F[FastAndSlow[F, FastF, FD, SlowF, SD]] = {
      lazy val ss = slow().flatMap(s)
      f(fast).map(FastAndSlow[F, FastF, FD, SlowF, SD](_, () => ss))
    }

    val rootRomFn: FastAndSlow[F, FastF, FD, SlowF, SD] => () => F[SD] =
      f => () => f.slow().map(_.extract)

    def toDomZipperPair2[A](f: FastAndSlow[F, FastF, FD, SlowF, SD] => A): DomZipperPair2[F, A] = {
      type _FastF[f[_], a] = FastF[f, a]
      type _SlowF[f[_], a] = SlowF[f, a]
      type _FD = FD
      type _SD = SD
      new DomZipperPair2[F, A] {
        override protected type FastF[f[_], a] = _FastF[f, a]
        override protected type SlowF[f[_], a] = _SlowF[f, a]
        override protected type FD = _FD
        override protected type SD = _SD
        override protected val fastAndSlow = FastAndSlow.this
        override protected val domFn = f
      }
    }

    def toDomZipperPair2Root = toDomZipperPair2(rootRomFn)
  }

}
