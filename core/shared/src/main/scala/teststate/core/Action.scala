package teststate.core

import acyclic.file
import teststate.data._
import teststate.typeclass._
import Types._
import Conditional.Implicits._

sealed abstract class Action[F[_], R, O, S, E] {
  def name: NameFn[ROS[R, O, S]]
}

object Action {

  type Prepared[F[_], O, S, E] = Option[() => F[E Or (O => E Or S)]]

  final case class Single[F[_], R, O, S, E](name : NameFn[ROS[R, O, S]],
                                            run  : ROS[R, O, S] => Prepared[F, O, S, E],
                                            check: Arounds[O, S, E]) extends Action[F, R, O, S, E]

  final case class Group[F[_], R, O, S, E](name  : NameFn[ROS[R, O, S]],
                                           action: ROS[R, O, S] => Option[Actions[F, R, O, S, E]],
                                           check : Arounds[O, S, E]) extends Action[F, R, O, S, E]

  final case class SubTest[F[_], R, O, S, E](name      : NameFn[ROS[R, O, S]],
                                             action    : Actions[F, R, O, S, E],
                                             invariants: Invariants[O, S, E],
                                             check     : Arounds[O, S, E]) extends Action[F, R, O, S, E]

  type Actions[F[_], R, O, S, E] = SackE[ROS[R, O, S], Action[F, R, O, S, E], E]

  def empty[F[_], R, O, S, E]: Actions[F, R, O, S, E] =
    Sack.empty

  implicit def actionInstanceShow[F[_], R, O, S, E]: Show[Action[F, R, O, S, E]] =
    Show(_.name(None).value)

  implicit def actionInstanceConditional[F[_], R, O, S, E]: Conditional[Action[F, R, O, S, E], ROS[R, O, S]] =
    new Conditional[Action[F, R, O, S, E], ROS[R, O, S]] {
      override def when(action: Action[F, R, O, S, E], f: ROS[R, O, S] => Boolean) =
        action match {
          case a: Single [F, R, O, S, E] => a.copy(run = a.run when f)
          case a: Group  [F, R, O, S, E] => a.copy(action = a.action when f)
          case a: SubTest[F, R, O, S, E] => a.copy(action = a.action when f)
        }
    }
}
