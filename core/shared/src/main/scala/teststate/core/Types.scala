package teststate.core

import acyclic.file
import teststate.data._

object Types {

  type CheckShapeA[C[_, _], A,    E] = (E Or A) Sack (C[A, E])
  type CheckShape [C[_, _], O, S, E] = CheckShapeA[C, OS[O, S], E]

  type Points    [O, S, E] = CheckShape[Point    , O, S, E]
  type Arounds   [O, S, E] = CheckShape[Around   , O, S, E]
  type Invariants[O, S, E] = CheckShape[Invariant, O, S, E]
}

