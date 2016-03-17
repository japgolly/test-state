package teststate.typeclass

import acyclic.file

trait PolyComposable[A, B, C] {
  def compose(a: A, b: B): C
}

object PolyComposable {
  type Mono[A] = PolyComposable[A, A, A]

  final class AndOps[A](private val a: A) extends AnyVal {
    def &[B, C](b: B)(implicit c: PolyComposable[A, B, C]): C =
      c.compose(a, b)
  }

  sealed trait CanAnd[A]
  @inline def CanAnd[A] = null.asInstanceOf[CanAnd[A]]

  trait ToOps {
    implicit def toPolyComposableAndOps[A: CanAnd](a: A): AndOps[A] =
      new AndOps(a)
  }

  trait Implicits extends ToOps
}
