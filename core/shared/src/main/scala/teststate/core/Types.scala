package teststate.core

import teststate.data._
import teststate.typeclass.Profunctor

object Types {

  type SackE[-A, +B, +E] = Sack[A, NamedError[Failure[E]] Or B]

  type CheckShape1[C[-_, _]] = ({  type T[-O, -S, E] = C[OS[O, S], E]  })

  type CheckShapeA[C[-_, _], -A,     E] = SackE[A, C[A, E], E]
  type CheckShape [C[-_, _], -O, -S, E] = CheckShapeA[C, OS[O, S], E]

  type Points    [-O, -S, E] = CheckShape[Point    , O, S, E]
  type Arounds   [-O, -S, E] = CheckShape[Around   , O, S, E]
  type Invariants[-O, -S, E] = CheckShape[Invariant, O, S, E]

  // OS →ˢ (NamedError E | OS →ᶜ E)

  implicit def checkShapeProfunctorOps[C[-_, _], O, S, E](a: CheckShape[C, O, S, E]): Profunctor.Ops[Sack, OS[O, S], NamedError[Failure[E]] Or C[OS[O, S], E]] =
    new Profunctor.Ops[Sack, OS[O, S], NamedError[Failure[E]] Or C[OS[O, S], E]](a)

  implicit def checkShapeAProfunctorOps[C[-_, _], A, E](a: CheckShapeA[C, A, E]): Profunctor.Ops[Sack, A, NamedError[Failure[E]] Or C[A, E]] =
    new Profunctor.Ops[Sack, A, NamedError[Failure[E]] Or C[A, E]](a)
}
