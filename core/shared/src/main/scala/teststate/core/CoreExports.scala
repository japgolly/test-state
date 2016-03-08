package teststate.core

import acyclic.file
import teststate.data.Name
import teststate.typeclass.Conditional

trait CoreExports
  extends Name.Implicits
     with CheckOps   .Implicits
     with Conditional.Implicits
     with PCompose   .Implicits
     with PointOps   .Implicits {

  type Points    [-O, -S, E] = Types.Points    [O, S, E]
  type Arounds   [-O, -S, E] = Types.Arounds   [O, S, E]
  type Invariants[-O, -S, E] = Types.Invariants[O, S, E]

  import Types.CheckShapeA
  implicit def autoWidenChecksToInvariants[C[-_, _], A, E](c: CheckShapeA[C, A, E])(implicit t: ToInvariant[CheckShapeA, C]): CheckShapeA[Invariant, A, E] =
    t.toInvariant(c)
}

object CoreExports extends CoreExports