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
     slow: Slow[F],
     evalStrategy: Eval.Strategy
   )(
    implicit F: ErrorHandler[F]
   ): DomZipperPair[F, () => F[A]] =
    full[F, () => F[A], Fast, Slow](
      fast,
      Store(slow)(s => Eval.value(F pass s)),
      g => () => g().map(_.dom),
      evalStrategy)

  def full[
    F[_],
    A,
    _FastF[f[_]] <: DomZipper[f, _, _FastF],
    _SlowF[f[_]] <: DomZipper[f, _, _SlowF]
  ](
    _fast: _FastF[F],
    _slow: Store[_SlowF[F], Eval[F[_SlowF[F]]]],
    _domFn: (() => F[_SlowF[F]]) => A,
    _evalStrategy: Eval.Strategy
   )(
    implicit _F: ErrorHandler[F]
   ): DomZipperPair[F, A] =
    new DomZipperPair[F, A] {
      override protected type FastF[f[_]] = _FastF[f]
      override protected type SlowF[f[_]] = _SlowF[f]
      override protected val fast = _fast
      override protected val slow = _slow
      override protected val domFn = _domFn
      override protected val evalStrategy = _evalStrategy
      override protected implicit val F = _F
    }
}

trait DomZipper2[F[_], A, Self[G[_]] <: DomZipper2[G, A, Self]] {
  def dom: A
}

trait DomZipperPair[F[_], A] extends DomZipper2[F, A, λ[G[_] => DomZipperPair[G, A]]] {

  protected type FastF[f[_]] <: DomZipper[f, _, FastF]
  protected type SlowF[f[_]] <: DomZipper[f, _, SlowF]
  protected final type Fast = FastF[F]
  protected final type Slow = SlowF[F]

  protected val fast: Fast
  protected val slow: Store[Slow, Eval[F[Slow]]]
  protected val domFn: (() => F[Slow]) => A
  protected val evalStrategy: Eval.Strategy
  protected implicit val F: ErrorHandler[F]

  def getAttribute(name: String): Option[String] =
    fast.getAttribute(name)

  def dom: A =
    domFn(() => slow.extract.eval())

  def apply(css: String): F[DomZipperPair[F, A]] =
    zmap(_(css), _(css))

  def parent: F[DomZipperPair[F, A]] =
    zmap(_.parent, _.parent)

  private def flatMapSlow(f: Slow => F[Slow]): Store[Slow, Eval[F[Slow]]] =
    slow.map(efa => evalStrategy(efa.eval().flatMap(f)))

  private def zmap(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    f(fast).map(DomZipperPair.full[F, A, FastF, SlowF](_, flatMapSlow(s), domFn, evalStrategy))
}
