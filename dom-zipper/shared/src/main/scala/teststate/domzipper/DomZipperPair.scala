package teststate.domzipper

import ErrorHandler._

// trait DomZipper[F[_], A, Self[G[_]] <: DomZipper[G, A, Self]] {

final case class Store[S, A](pos: S, peek: S => A) {
  def map[B](f: A => B): Store[S, B] =
    Store(pos, f compose peek)

  def extract: A =
    peek(pos)

  def duplicate: Store[S, Store[S, A]] =
    Store(pos, Store(_, peek))

  def extend[B](f: Store[S, A] => B): Store[S, B] =
    duplicate.map(f)

  def xmap[T](g: S => T)(f: T => S): Store[T, A] =
    Store(g(pos), peek compose f)
}

object DomZipperPair {
  def apply[F[_], A,
  _FastF[F[_]] <: DomZipper[F, _, _FastF],
  _SlowF[F[_]] <: DomZipper[F, A, _SlowF]](
                                                     _fast: _FastF[F],
                                                     _slow: Store[_SlowF[F], F[_SlowF[F]]]
                                                   ): DomZipperPair[F, A] =
    new DomZipperPair[F, A] {
      override type FastF[F[_]] = _FastF[F]
      override type SlowF[F[_]] = _SlowF[F]
      override val fast = _fast
      override val slow = _slow
    }
}

trait DomZipperPair[F[_], A]
//fast: DomZipper[F, _, _],
//slow: DomZipper[F, A, _]
// extends DomZipper[F, () => F[A], DomZipperPair]
{

  type FastF[F[_]] = DomZipper[F, _, FastF]
  final type Fast = FastF[F]

  type SlowF[F[_]] = DomZipper[F, A, SlowF]
  final type Slow = SlowF[F]

  val fast: Fast
  val slow: Store[Slow, F[Slow]]

  def getAttribute(name: String): Option[String] =
    fast.getAttribute(name)

  def apply(css: String): F[DomZipperPair[F, A]] =
    fast(css).map(f =>
      DomZipperPair[F, A, FastF, SlowF](f, slow.map(_.flatMap(_.apply(css))))
    )

  val dom: () => F[A] =
    () => slow.extract.map(_.dom)

//  final lazy val parent: F[DomZipperPair[F, A]] =
//    F.map(_parent)(a => addLayer(Layer("parent", ":parent", a)))

}
