package teststate.core

import acyclic.file
import teststate.data.Sack
import teststate.typeclass.PolyComposable
import PolyComposable.CanAnd
import Types._

object CoreComposition {

  trait P0 {

    implicit def checksPolyComposable[C[-_, _], D[-_, _], A, E](implicit
                                                                  c: ToInvariant[CheckShapeA, C],
                                                                  d: ToInvariant[CheckShapeA, D],
                                                                  i: PolyComposable.Mono[CheckShapeA[Invariant, A, E]])
    : PolyComposable[CheckShapeA[C, A, E], CheckShapeA[D, A, E], CheckShapeA[Invariant, A, E]] = {
      type F[X[-_, _]] = CheckShapeA[X, A, E]
      new PolyComposable[F[C], F[D], F[Invariant]] {
        override def compose(fc: F[C], fd: F[D]) =
          i.compose(c toInvariant fc, d toInvariant fd)
      }
    }
  }

  trait Implicits extends P0 {

    implicit def checksMonoComposable[A, B]: PolyComposable[Sack[A, B], Sack[A, B], Sack[A, B]] =
      new PolyComposable[Sack[A, B], Sack[A, B], Sack[A, B]] {
        override def compose(a: Sack[A, B], b: Sack[A, B]) =
          Sack.append(a, b)
      }

    implicit def checksCanAnd[C[-_, _], A, B]: CanAnd[CheckShapeA[C, A, B]] =
      CanAnd
  }

}