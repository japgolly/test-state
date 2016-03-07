package teststate.core

import acyclic.file
import teststate.data._

object Types {

  type SackE[-A, +B, +E] = Sack[A, NamedError[E] Or B]

  type CheckShape1[C[_, _]] = ({  type T[O, S, E] = C[OS[O, S], E]  })

  type CheckShapeA[C[_, _], A,    E] = SackE[A, C[A, E], E]
  type CheckShape [C[_, _], O, S, E] = CheckShapeA[C, OS[O, S], E]

  type Points    [O, S, E] = CheckShape[Point    , O, S, E]
  type Arounds   [O, S, E] = CheckShape[Around   , O, S, E]
  type Invariants[O, S, E] = CheckShape[Invariant, O, S, E]

  // OS →ˢ (NamedError E | OS →ᶜ E)
}
