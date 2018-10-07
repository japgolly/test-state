package teststate.domzipper

import ErrorHandler._

/** Fusion of two DomZippers over identical content.
  *
  * One zipper ("fast") is used for all of the inspection,
  * the other zipper ("slow") is used when real DOM is needed.
  *
  * @since 2.3.0
  */
sealed trait DomZipperPair[F[_], A] extends DomZipper[F, A, DomZipperPair] with DomZipperBase.Store[F, A, DomZipperPair] {
  import DomZipper.DomCollection
  import DomZipperPair.FastAndSlow

  override protected implicit def F: ErrorHandler[F] = pos.F

  override protected final type Pos = FastAndSlow[F, FastF, FD, SlowF, SD]
  protected type FD
  protected type SD
  protected type FastF[f[_], a] <: DomZipper[f, a, FastF]
  protected type SlowF[f[_], a] <: DomZipper[f, a, SlowF]
  protected val pos: Pos
  import pos.{fast, slow, Fast, Slow}

  protected val peek: Peek[A]

  private def bimap(f: Fast => Fast, s: Slow => Slow): DomZipperPair[F, A] =
    pos.bimap(f, s).toDomZipperPair(peek)

  private def bimapF(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    pos.bimapF(f, s).map(_.toDomZipperPair(peek))

  private def cmap[C[_]](runF: Fast => DomCollection[FastF, F, C, FD],
                         runS: Slow => DomCollection[SlowF, F, C, SD]): DomCollection[DomZipperPair, F, C, A] = {
    val colF = runF(fast)
    lazy val colS = slow().map(runS)
    val C = colF.C
    val rawResults = Vector.tabulate(colF.size) { i =>
      val f = colF.rawResults(i)
      lazy val s = colS.flatMap(_.zippers).map(C.get(_, i))
      FastAndSlow(f, () => s).toDomZipperPair(peek)
    }

    new DomCollection[DomZipperPair, F, C, A](
      desc       = colF.desc,
      rawResults = rawResults,
      filterFn   = None,
      C          = C
    )
  }

  // ===================================================================================================================

  override final protected def newStore[B](pos: FastAndSlow[F, FastF, FD, SlowF, SD], peek: Peek[B]): DomZipperPair[F, B] =
    pos.toDomZipperPair(peek)

  override final type Dom = () => F[SD]
  override final def dom = pos.rootRomFn(pos)
  override final def unmap = pos.toDomZipperPairRoot

  // ===================================================================================================================

  override def describe = fast.describe

  protected override def self = this

  protected[domzipper] def htmlScrub = fast.htmlScrub

  override def scrubHtml(f: HtmlScrub): DomZipperPair[F, A] =
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

  override def parent: F[DomZipperPair[F, A]] =
    bimapF(_.parent, _.parent)

  override def apply(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
    bimapF(_(name, sel, which), _(name, sel, which))

  override def child(name: String, sel: String, which: MofN): F[DomZipperPair[F, A]] =
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

object DomZipperPair {
  def apply[F[_],
            Fast[f[_], a] <: DomZipper[f, a, Fast], FD,
            Slow[f[_], a] <: DomZipper[f, a, Slow], SD]
            (fast: Fast[F, FD], slow: Slow[F, SD])
            (implicit F: ErrorHandler[F]): DomZipperPair[F, () => F[SD]] =
    FastAndSlow(fast, () => F.pass(slow)).toDomZipperPairRoot

  final case class FastAndSlow[
    F[_],
    FastF[f[_], a] <: DomZipper[f, a, FastF], FD,
    SlowF[f[_], a] <: DomZipper[f, a, SlowF], SD,
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

    def toDomZipperPair[A](f: FastAndSlow[F, FastF, FD, SlowF, SD] => A): DomZipperPair[F, A] = {
      type _FastF[f[_], a] = FastF[f, a]
      type _SlowF[f[_], a] = SlowF[f, a]
      type _FD = FD
      type _SD = SD
      new DomZipperPair[F, A] {
        override protected type FastF[f[_], a] = _FastF[f, a]
        override protected type SlowF[f[_], a] = _SlowF[f, a]
        override protected type FD = _FD
        override protected type SD = _SD
        override protected val pos = FastAndSlow.this
        override protected val peek = f
      }
    }

    def toDomZipperPairRoot = toDomZipperPair(rootRomFn)
  }

}
