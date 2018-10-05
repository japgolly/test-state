package teststate.domzipper

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

  def fromUnit[A](a: => A): Store[Unit, A] =
    apply(())(_ => a)
}
