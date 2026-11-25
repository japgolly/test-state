package teststate.core

import java.time.Duration
import teststate.typeclass._
import teststate.data.Or
import CoreExports.Actions
import CoreExports2._

/** The next step to take while in a loop.
  *
  * @since 3.2.0
  */
sealed trait LoopStep[F[_], R, O, S, E] {
  import LoopStep._

  final def trans[G[_]](t: F ~~> G): LoopStep[G, R, O, S, E] =
    this match {
      case Act(actions)       => Act(actions trans t)
      case Break(m)           => Break(m)
      case Fail(failure)      => Fail(failure)
      case Sleep(duration, m) => Sleep(duration, m)
    }

  final def mapR[X](f: X => R): LoopStep[F, X, O, S, E] =
    this match {
      case Act(actions)       => Act(actions mapR f)
      case Break(m)           => Break(m)
      case Fail(failure)      => Fail(failure)
      case Sleep(duration, m) => Sleep(duration, m)
    }

  final def mapOS[X, Y](f: X => O, g: Y => S)(h: (Y, S) => Y)(implicit em: ExecutionModel[F]): LoopStep[F, R, X, Y, E] = {
    def modSS(o: Option[S => E Or S]): Option[Y => E Or Y] = o.map(ses => y => ses(g(y)).map(h(y, _)))
    this match {
      case Act(actions)       => Act(actions.mapOS(f, g)(h))
      case Break(m)           => Break(modSS(m))
      case Fail(failure)      => Fail(failure)
      case Sleep(duration, m) => Sleep(duration, modSS(m))
    }
  }

  final def mapE[X](f: E => X)(implicit em: ExecutionModel[F]): LoopStep[F, R, O, S, X] = {
    def modSS(o: Option[S => E Or S]): Option[S => X Or S] = o.map(_.andThen(_.leftMap(f)))
    this match {
      case Act(actions)       => Act(actions.mapE(f))
      case Break(m)           => Break(modSS(m))
      case Fail(failure)      => Fail(f(failure))
      case Sleep(duration, m) => Sleep(duration, modSS(m))
    }
  }

  final def pmapR[X](f: X => E Or R)(implicit em: ExecutionModel[F]): LoopStep[F, X, O, S, E] =
    this match {
      case Act(actions)       => Act(actions.pmapR(f))
      case Break(m)           => Break(m)
      case Fail(failure)      => Fail(failure)
      case Sleep(duration, m) => Sleep(duration, m)
    }

  final def pmapO[X](f: X => E Or O)(implicit em: ExecutionModel[F]): LoopStep[F, R, X, S, E] =
    this match {
      case Act(actions)       => Act(actions.pmapO(f))
      case Break(m)           => Break(m)
      case Fail(failure)      => Fail(failure)
      case Sleep(duration, m) => Sleep(duration, m)
    }

  final def modS(m: O => S => E Or S, o: O): LoopStep[F, R, O, S, E] =
    // Here we only apply it to the break step because we want a loop-external .updateState call to only run at the end
    // of the loop, not be applied to every step inside the loop.
    this match {
      case Break(oss) =>
        val f = m(o)
        val composed: S => E Or S = oss match {
          case None    => f
          case Some(g) => g(_).flatMap(f)
        }
        Break(Some(composed))
      case Act(_) | Fail(_) | Sleep(_, _) =>
        this
    }
}

object LoopStep {

  /** Run actions and continue the loop. */
  final case class Act[F[_], R, O, S, E](actions: Actions[F, R, O, S, E]) extends LoopStep[F, R, O, S, E]

  /** Break the loop. */
  final case class Break[F[_], R, O, S, E](modState: Option[S => E Or S]) extends LoopStep[F, R, O, S, E]

  /** Report a failure. */
  final case class Fail[F[_], R, O, S, E](failure: E) extends LoopStep[F, R, O, S, E]

  /** Sleep for a specified duration then continue the loop. */
  final case class Sleep[F[_], R, O, S, E](duration: Duration, modState: Option[S => E Or S]) extends LoopStep[F, R, O, S, E]
}
