package teststate.domzipper

import ErrorHandler._

final case class Store[S, A](pos: S, peek: S => A) {
  def map[B](f: A => B): Store[S, B] =
    Store(pos, f compose peek)

  def extract: A =
    peek(pos)

  def duplicate: Store[S, Store[S, A]] =
    Store(pos, Store(_, peek))

  def extend[B](f: Store[S, A] => B): Store[S, B] =
    duplicate.map(f)
}

// =====================================================================================================================

object DomZipperPair {
  def apply[
    F[_],
    A,
    _FastF[f[_]] <: DomZipper[f, _, _FastF],
    _SlowF[f[_]] <: DomZipper[f, A, _SlowF]
  ](
    _fast: _FastF[F],
    _slow: Store[_SlowF[F], F[_SlowF[F]]]
   )(
    implicit f: ErrorHandler[F]
   ): DomZipperPair[F, A] =
    new DomZipperPair[F, A] {
      override type FastF[f[_]] = _FastF[f]
      override type SlowF[f[_]] = _SlowF[f]
      override val fast = _fast
      override val slow = _slow
      override implicit val F = f
    }
}

trait DomZipperPair[F[_], A] {
  type FastF[f[_]] <: DomZipper[f, _, FastF]
  type SlowF[f[_]] <: DomZipper[f, A, SlowF]
  final type Fast = FastF[F]
  final type Slow = SlowF[F]
  val fast: Fast
  val slow: Store[Slow, F[Slow]]
  implicit val F: ErrorHandler[F]

  def getAttribute(name: String): Option[String] =
    fast.getAttribute(name)

  val dom: () => F[A] =
    () => slow.extract.map(_.dom)

  def apply(css: String): F[DomZipperPair[F, A]] =
    `i see a pattern: `(_(css), _(css))

  def parent: F[DomZipperPair[F, A]] =
    `i see a pattern: `(_.parent, _.parent)

  private def `i see a pattern: `(f: Fast => F[Fast], s: Slow => F[Slow]): F[DomZipperPair[F, A]] =
    f(fast).map(DomZipperPair[F, A, FastF, SlowF](_, slow.map(_.flatMap(s))))
}
