package teststate.core

import teststate.core.Types.CheckShapeA
import teststate.data.Sack
import teststate.typeclass.PolyComposable
import teststate.typeclass.PolyComposable._

/**
  * P = Point
  * @ = Around
  * I = Invariant
  * C = Check
  * A = Action
  *
  * P & P = P
  * @ & @ = @
  * I & I = I
  * C & C = I
  */
object CoreComposition {

  trait P0 {

    implicit def checksPolyComposable[C[-_, _], D[-_, _], A, E](implicit
                                                                c: ToInvariants[CheckShapeA, C],
                                                                d: ToInvariants[CheckShapeA, D],
                                                                i: PolyComposable.Mono[AndOp, CheckShapeA[Invariant, A, E]])
        : PolyComposable[AndOp, CheckShapeA[C, A, E], CheckShapeA[D, A, E], CheckShapeA[Invariant, A, E]] =
      PolyComposable((fc, fd) => i.compose(c toInvariants fc, d toInvariants fd))
  }

  trait Implicits extends P0 {

    implicit def checksMonoComposable[C[-_, _], A, E]: PolyComposable.Mono[AndOp, CheckShapeA[C, A, E]] =
      PolyComposable(Sack.append)

    implicit def checksCanAnd[C[-_, _], A, B]: Can[AndOp, CheckShapeA[C, A, B]] = Can
  }

}