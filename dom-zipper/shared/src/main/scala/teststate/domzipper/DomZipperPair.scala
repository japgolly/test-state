package teststate.domzipper

import ErrorHandler._

final class Store[S, A](val pos: S, val peek: S => A) {
  override def toString: String =
    s"Store($pos)($peek)"

  def map[B](f: A => B): Store[S, B] =
    Store(pos)(f compose peek)

  def extract: A =
    peek(pos)

  def duplicate: Store[S, Store[S, A]] =
    Store(pos)(Store(_)(peek))

  def extend[B](f: Store[S, A] => B): Store[S, B] =
    duplicate.map(f)
}

object Store {
  def apply[S, A](pos: S)(peek: S => A): Store[S, A] =
    new Store(pos, peek)
}


// =====================================================================================================================

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

trait DomZipper2[F[_], A, Self[G[_]] <: DomZipper2[G, A, Self]] {
  def dom: A
}

trait DomZipperPair[F[_], A] extends DomZipper2[F, A, λ[G[_] => DomZipperPair[G, A]]] {

  protected type FastF[f[_]] <: DomZipper[f, _, FastF]
  protected type SlowF[f[_]] <: DomZipper[f, _, SlowF]
  protected final type Fast = FastF[F]
  protected final type Slow = SlowF[F]

  protected val fast: Fast
  protected val slow: Store[Slow, F[Slow]]
  protected val domFn: (() => F[Slow]) => A
  protected implicit val F: ErrorHandler[F]

  def getAttribute(name: String): Option[String] =
    fast.getAttribute(name)

  def dom: A =
    domFn(() => slow.extract)

  def apply(css: String): F[DomZipperPair[F, A]] =
    zmap(_(css), _(css))

  def parent: F[DomZipperPair[F, A]] =
    zmap(_.parent, _.parent)

  private def zmap(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    f(fast).map(DomZipperPair.full[F, A, FastF, SlowF](_, slow.map(_.flatMap(s)), domFn))
}
