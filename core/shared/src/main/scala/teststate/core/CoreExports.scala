package teststate.core

import acyclic.file
import teststate.data.{Name, Sack}
import teststate.typeclass.{Conditional, Show}

trait CoreExports
  extends Name.Implicits
     with CheckOps   .Implicits
     with Conditional.Implicits
     with PCompose   .Implicits
     with PointOps   .Implicits
     with Show       .Implicits {

  type Points    [-O, -S, E] = Types.Points    [O, S, E]
  type Arounds   [-O, -S, E] = Types.Arounds   [O, S, E]
  type Invariants[-O, -S, E] = Types.Invariants[O, S, E]

  type Actions[F[_], R, O, S, E] = Action.Actions[F, R, O, S, E]

  import Types._
  implicit def autoWidenChecksToInvariants[C[-_, _], A, E](c: CheckShapeA[C, A, E])(implicit t: ToInvariant[CheckShapeA, C]): CheckShapeA[Invariant, A, E] =
    t.toInvariant(c)

  def emptyPoints    [E]: Points    [Any, Any, E] = Sack.empty
  def emptyArounds   [E]: Arounds   [Any, Any, E] = Sack.empty
  def emptyInvariants[E]: Invariants[Any, Any, E] = Sack.empty

  import teststate.data._
  implicit def checksInstanceShow[C[- _, _], A, E](implicit sc: Show[C[A, E]]): Show[CheckShapeA[C, A, E]] = {
    val showValues = Show[NamedError[E] Or C[A, E]](_.fold(_.name.value, sc.apply))
    Sack sackInstanceShow showValues
  }
}

object CoreExports extends CoreExports