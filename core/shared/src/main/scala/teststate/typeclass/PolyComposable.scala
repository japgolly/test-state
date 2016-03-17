package teststate.typeclass

import acyclic.file

case class PolyComposable[A, B, C](compose: (A, B) => C) extends AnyVal

object PolyComposable {
  type Mono[A] = PolyComposable[A, A, A]
  type Left[A, B] = PolyComposable[A, B, A]
  type Right[A, B] = PolyComposable[A, B, B]

  final class AndOps[A](private val a: A) extends AnyVal {
    def &[B, C](b: B)(implicit c: PolyComposable[A, B, C]): C =
      c.compose(a, b)
  }

  final class SeqOps[A](private val a: A) extends AnyVal {
    def >>[B, C](b: B)(implicit c: PolyComposable[A, B, C]): C =
      c.compose(a, b)

    def <<[B, C](b: B)(implicit c: PolyComposable[B, A, C]): C =
      c.compose(b, a)

    @inline def andThen[B, C](b: B)(implicit c: PolyComposable[A, B, C]): C =
      >>(b)(c)

    @inline def precededBy[B, C](b: B)(implicit c: PolyComposable[B, A, C]): C =
      <<(b)(c)
  }

  sealed trait CanAnd[A]
  @inline def CanAnd[A] = null.asInstanceOf[CanAnd[A]]

  sealed trait CanSeq[A]
  @inline def CanSeq[A] = null.asInstanceOf[CanSeq[A]]

  trait ToOps {
    implicit def toPolyComposableAndOps[A: CanAnd](a: A): AndOps[A] =
      new AndOps(a)

    implicit def toPolyComposableSeqOps[A: CanSeq](a: A): SeqOps[A] =
      new SeqOps(a)
  }

  trait Implicits extends ToOps
}
