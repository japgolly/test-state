package testate.typeclass

import acyclic.file

trait ~~>[F[_], G[_]] {
  def apply[A](fa: => F[A]): G[A]
}
