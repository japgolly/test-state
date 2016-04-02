package teststate.core

import acyclic.file
import teststate.data.Name
import teststate.typeclass.{PolyComposable, Conditional, Display}

trait CoreExports
  extends Name.Implicits
     with CheckOps       .Implicits
     with NamedOps       .Implicits
     with Conditional    .Implicits
     with PolyComposable .Implicits
     with CoreComposition.Implicits
     with PointOps       .Implicits
     with Display        .Implicits {

  type Points    [-O, -S, E] = Types.Points    [O, S, E]
  type Arounds   [-O, -S, E] = Types.Arounds   [O, S, E]
  type Invariants[-O, -S, E] = Types.Invariants[O, S, E]

  type Actions[F[_], R, O, S, E] = Action.Actions[F, R, O, S, E]

  import Types._
  implicit def autoWidenChecksToInvariants[C[-_, _], A, E](c: CheckShapeA[C, A, E])(implicit t: ToInvariant[CheckShapeA, C]): CheckShapeA[Invariant, A, E] =
    t.toInvariant(c)

  import teststate.data._
  implicit def checksInstanceDisplay[C[- _, _], A, E](implicit sc: Display[C[A, E]]): Display[CheckShapeA[C, A, E]] = {
    val displayValues = Display[NamedError[Failure[E]] Or C[A, E]](_.fold(_.name.value, sc.apply))
    Sack sackInstanceDisplay displayValues
  }
}

object CoreExports extends CoreExports
