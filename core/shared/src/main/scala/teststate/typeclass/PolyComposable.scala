package teststate.typeclass

import acyclic.file
import scala.annotation.implicitNotFound
import scala.collection.compat._

@implicitNotFound("\n  Can't compose ${A}\n            and ${B}")
case class PolyComposable[Op, A, B, C](compose: (A, B) => C) extends AnyVal

object PolyComposable {
  type Mono [Op, A]    = PolyComposable[Op, A, A, A]
  type Left [Op, A, B] = PolyComposable[Op, A, B, A]
  type Right[Op, A, B] = PolyComposable[Op, A, B, B]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait AndOp

  final class AndOps[A](private val a: A) extends AnyVal {
    def &[B, C](b: B)(implicit c: PolyComposable[AndOp, A, B, C]): C =
      c.compose(a, b)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait SeqOp

  final class SeqOps[A](private val a: A) extends AnyVal {
    def >>[B, C](b: B)(implicit c: PolyComposable[SeqOp, A, B, C]): C =
      c.compose(a, b)

    def <<[B, C](b: B)(implicit c: PolyComposable[SeqOp, B, A, C]): C =
      c.compose(b, a)

    @inline def andThen[B, C](b: B)(implicit c: PolyComposable[SeqOp, A, B, C]): C =
      >>(b)(c)

    @inline def precededBy[B, C](b: B)(implicit c: PolyComposable[SeqOp, B, A, C]): C =
      <<(b)(c)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Higher precedence. Evaluated before other SeqOps */
  sealed trait HPSeqOp

  final class HPSeqOps[A](private val a: A) extends AnyVal {
    def +>[B, C](b: B)(implicit c: PolyComposable[HPSeqOp, A, B, C]): C =
      c.compose(a, b)

    def <+[B, C](b: B)(implicit c: PolyComposable[HPSeqOp, B, A, C]): C =
      c.compose(b, a)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class MonoComposableTraversableOnceOps[C[x] <: IterableOnce[x], Op, A](as: C[A], c: Mono[Op, A]) {
    def combine(implicit empty: Empty[A]): A =
      as.foldLeft(empty.instance)(c.compose)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait Can[Op, A]
  @inline def Can[Op, A] = null.asInstanceOf[Can[Op, A]]

  trait ToOps {
    implicit def toPolyComposableAndOps  [A](a: A)(implicit w: Can[AndOp  , A]): AndOps  [A] = new AndOps  (a)
    implicit def toPolyComposableSeqOps  [A](a: A)(implicit w: Can[SeqOp  , A]): SeqOps  [A] = new SeqOps  (a)
    implicit def toPolyComposableHPSeqOps[A](a: A)(implicit w: Can[HPSeqOp, A]): HPSeqOps[A] = new HPSeqOps(a)

    implicit def toMonoComposableTraversableOnceOpsA[C[x] <: IterableOnce[x], A](as: C[A])(implicit c: Mono[AndOp, A]): MonoComposableTraversableOnceOps[C, AndOp, A] =
      new MonoComposableTraversableOnceOps(as, c)

    implicit def toMonoComposableTraversableOnceOpsS[C[x] <: IterableOnce[x], A](as: C[A])(implicit c: Mono[SeqOp, A]): MonoComposableTraversableOnceOps[C, SeqOp, A] =
      new MonoComposableTraversableOnceOps(as, c)
  }

  trait Implicits extends ToOps
}
