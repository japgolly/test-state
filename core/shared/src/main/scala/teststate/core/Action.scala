package teststate.core

import acyclic.file
import teststate.data._
import teststate.typeclass._
import Types._
import Conditional.Implicits._

object Action {
  sealed abstract class Inner[F[_], R, O, S, E]

  type Prepared[F[_], O, S, E] = Option[() => F[E Or (O => E Or S)]]

  final case class Single[F[_], R, O, S, E](run: ROS[R, O, S] => Prepared[F, O, S, E]) extends Inner[F, R, O, S, E] {
    def mod[F2[_], R2, O2, S2, E2](f: (ROS[R, O, S] => Prepared[F, O, S, E]) => ROS[R2, O2, S2] => Prepared[F2, O2, S2, E2]): Single[F2, R2, O2, S2, E2] =
      Single(f(run))
  }

  final case class Group[F[_], R, O, S, E](action: ROS[R, O, S] => Option[Actions[F, R, O, S, E]]) extends Inner[F, R, O, S, E]

  final case class SubTest[F[_], R, O, S, E](action    : Actions[F, R, O, S, E],
                                             invariants: Invariants[O, S, E]) extends Inner[F, R, O, S, E]

  final case class Outer[F[_], R, O, S, E](name : NameFn[ROS[R, O, S]],
                                           inner: Inner[F, R, O, S, E],
                                           check: Arounds[O, S, E])

  type Actions[F[_], R, O, S, E] = SackE[ROS[R, O, S], Outer[F, R, O, S, E], E]

  def empty[F[_], R, O, S, E]: Actions[F, R, O, S, E] =
    Sack.empty

  def liftOuter[F[_], R, O, S, E](outer: Outer[F, R, O, S, E]): Actions[F, R, O, S, E] =
    Sack Value Right(outer)

  def liftInner[F[_], R, O, S, E](inner: Inner[F, R, O, S, E])(name: NameFn[ROS[R, O, S]]): Actions[F, R, O, S, E] =
    liftOuter(Outer(name, inner, Sack.empty))

  implicit def actionOuterInstanceNamedOps[F[_], R, O, S, E]: NamedOps[Outer[F, R, O, S, E], ROS[R, O, S]] =
    NamedOps((a, f) => a.copy(name = f(a.name)))

  implicit def actionOuterInstanceDisplay[F[_], R, O, S, E]: Display[Outer[F, R, O, S, E]] =
    Display(_.name(None).value)

  implicit def actionInnerInstanceConditional[F[_], R, O, S, E]: Conditional[Inner[F, R, O, S, E], ROS[R, O, S]] =
    Conditional((m, f) => m match {
      case a: Single [F, R, O, S, E] => a.copy(run = a.run when f)
      case a: Group  [F, R, O, S, E] => a.copy(action = a.action when f)
      case a: SubTest[F, R, O, S, E] => a.copy(action = a.action when f)
    })

  implicit def actionOuterInstanceConditional[F[_], R, O, S, E]: Conditional[Outer[F, R, O, S, E], ROS[R, O, S]] =
    Conditional((a, f) => a.copy(inner = a.inner when f))
}
