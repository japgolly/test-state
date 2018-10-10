package teststate.domzipper

import ErrorHandler._

/** Fusion of two DomZippers over identical content.
  *
  * One zipper ("fast") is used for all of the inspection,
  * the other zipper ("slow") is used when real DOM is needed.
  *
  * @since 2.3.0
  */
sealed trait DomZipperPair[F[_], Dom, A] extends DomZipperBase.Store[F, Dom, A, DomZipperPair[?[_], Dom, ?]] {
  import DomZipper.DomCollection
  import DomZipperPair.FastAndSlow

  override protected implicit def F: ErrorHandler[F] = pos.F

  override protected final type Pos = FastAndSlow[F, FastF, FD, SlowF, SD]
  protected type FD
  protected type SD
  protected type FastF[f[_], a] <: DomZipper[f, _, a, FastF]
  protected type SlowF[f[_], a] <: DomZipper[f, _, a, SlowF]
  protected val pos: Pos
  import pos.{fast, slow, Fast, Slow}

  protected val peek: Peek[A]

  // This is embarrassing. Scala won't let me specify SD alone in place of Dom in the class definition.
  protected implicit def ffs0(z: () => F[SD]): Dom
  protected implicit def ffs1(z: DomZipperPair[F, () => F[SD], () => F[SD]]): DomZipperPair[F, Dom, Dom]
  protected implicit def ffs2[B](z: DomZipperPair[F, () => F[SD], B]): DomZipperPair[F, Dom, B]
  protected implicit def ffs3[B](z: Vector[DomZipperPair[F, () => F[SD], B]]): Vector[DomZipperPair[F, Dom, B]]

  private def bimap(f: Fast => Fast, s: Slow => Slow): DomZipperPair[F, Dom, A] =
    pos.bimap(f, s).toDomZipperPair(peek)

  private def bimapF(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, Dom, A]] =
    pos.bimapF(f, s).map(_.toDomZipperPair(peek))

  private def cmap[C[_]](runF: Fast => DomCollection[FastF, F, C, _, FD],
                         runS: Slow => DomCollection[SlowF, F, C, _, SD]): DomCollection[DomZipperPair[?[_], Dom, ?], F, C, Dom, A] = {
    val colF = runF(fast)
    lazy val colS = slow().map(runS)
    val C = colF.C
    val rawResults = Vector.tabulate(colF.size) { i =>
      val f = colF.rawResults(i)
      lazy val s = colS.flatMap(_.zippers).map(C.get(_, i))
      FastAndSlow(f, () => s).toDomZipperPair(peek)
    }
    new DomCollection[DomZipperPair[?[_], Dom, ?], F, C, Dom, A](
      desc       = colF.desc,
      rawResults = rawResults,
      filterFn   = None,
      C          = C
    )
  }

  override final protected def newStore[B](pos: FastAndSlow[F, FastF, FD, SlowF, SD], peek: Peek[B]): DomZipperPair[F, Dom, B] =
    pos.toDomZipperPair(peek)

  override final def dom = pos.rootRomFn(pos)

  override final def unmap = pos.toDomZipperPairRoot

  override def describe = fast.describe

  protected override def self = this

  protected[domzipper] def htmlScrub = fast.htmlScrub

  override def scrubHtml(f: HtmlScrub) =
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

  override def parent: F[DomZipperPair[F, Dom, A]] =
    bimapF(_.parent, _.parent)

  override def apply(name: String, sel: String, which: MofN): F[DomZipperPair[F, Dom, A]] =
    bimapF(_(name, sel, which), _(name, sel, which))

  override def child(name: String, sel: String, which: MofN): F[DomZipperPair[F, Dom, A]] =
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

  type Home[F[_], A] = DomZipperPair[F, () => F[A], () => F[A]]

  type DomCollection[F[_], C[_], Dom, A] = DomZipper.DomCollection[DomZipperPair[?[_], () => F[Dom], ?], F, C, () => F[Dom], A]

  def apply[F[_],
            Fast[f[_], a] <: DomZipper[f, _, a, Fast], FD,
            Slow[f[_], a] <: DomZipper[f, _, a, Slow], SD]
            (fast: Fast[F, FD], slow: Slow[F, SD])
            (implicit F: ErrorHandler[F]): Home[F, SD] =
    FastAndSlow(fast, () => F.pass(slow)).toDomZipperPairRoot

  final case class FastAndSlow[F[_],
                               FastF[f[_], a] <: DomZipper[f, _, a, FastF], FD,
                               SlowF[f[_], a] <: DomZipper[f, _, a, SlowF], SD
                             ](fast: FastF[F, FD], slow: () => F[SlowF[F, SD]])
                              (implicit val F: ErrorHandler[F]) {

    type Fast = FastF[F, FD]
    type Slow = SlowF[F, SD]
    type Dom = () => F[SD]

    def bimap(f: Fast => Fast, s: Slow => Slow): FastAndSlow[F, FastF, FD, SlowF, SD] = {
      lazy val ss = slow().map(s)
      FastAndSlow[F, FastF, FD, SlowF, SD](f(fast), () => ss)
    }

    def bimapF(f: Fast => F[Fast], s: Slow => F[Slow]): F[FastAndSlow[F, FastF, FD, SlowF, SD]] = {
      lazy val ss = slow().flatMap(s)
      f(fast).map(FastAndSlow[F, FastF, FD, SlowF, SD](_, () => ss))
    }

    val rootRomFn: FastAndSlow[F, FastF, FD, SlowF, SD] => Dom =
      f => () => f.slow().map(_.extract)

    def toDomZipperPair[A](f: FastAndSlow[F, FastF, FD, SlowF, SD] => A): DomZipperPair[F, Dom, A] = {
      type _FastF[f[_], a] = FastF[f, a]
      type _SlowF[f[_], a] = SlowF[f, a]
      type _FD = FD
      type _SD = SD
      new DomZipperPair[F, Dom, A] {
        override protected type FastF[f[_], a] = _FastF[f, a]
        override protected type SlowF[f[_], a] = _SlowF[f, a]
        override protected type FD = _FD
        override protected type SD = _SD
        override protected val pos = FastAndSlow.this
        override protected val peek = f
        override protected implicit def ffs0(z: () => F[SD]) = z
        override protected implicit def ffs1(z: DomZipperPair[F, () => F[SD], () => F[SD]]) = z
        override protected implicit def ffs2[B](z: DomZipperPair[F, () => F[SD], B]) = z
        override protected implicit def ffs3[B](z: Vector[DomZipperPair[F, () => F[SD], B]]) = z
      }
    }

    def toDomZipperPairRoot = toDomZipperPair(rootRomFn)
  }

}
