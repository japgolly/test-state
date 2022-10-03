package teststate.run

import japgolly.microlibs.name_fn._
import java.time.{Duration, Instant}
import teststate.data._
import teststate.typeclass._

/** @since 3.2.0 */
final case class RunCallbacks[F[_], -R, -O, -S, E](aroundAction: (ROS[R, O, S], Name) => F[Option[E] => F[Unit]])
                                                  (implicit val F: ExecutionModel[F]) { self =>

  def trans[G[_]](t: F ~~> G)(implicit G: ExecutionModel[G]): RunCallbacks[G, R, O, S, E] =
    RunCallbacks((ros, n) => G.map(t(self.aroundAction(ros, n)))(g => (oe: Option[E]) => t(g(oe))))

  def mapR[R2](f: R2 => R): RunCallbacks[F, R2, O, S, E] =
    RunCallbacks((ros, n) => self.aroundAction(ros.mapR(f), n))

  def mapO[O2](f: O2 => O): RunCallbacks[F, R, O2, S, E] =
    RunCallbacks((ros, n) => self.aroundAction(ros.mapO(f), n))

  def mapS[S2](f: S2 => S): RunCallbacks[F, R, O, S2, E] =
    RunCallbacks((ros, n) => self.aroundAction(ros.mapS(f), n))

  def mapE[E2](f: E2 => E): RunCallbacks[F, R, O, S, E2] =
    RunCallbacks((ros, n) => F.map(self.aroundAction(ros, n))(g => oe => g(oe.map(f))))

  def ++[R2 <: R, O2 <: O, S2 <: S](next: RunCallbacks[F, R2, O2, S2, E]): RunCallbacks[F, R2, O2, S2, E] =
    RunCallbacks((r, n) =>
      F.flatMap(self.aroundAction(r, n))(f1 =>
      F.flatMap(next.aroundAction(r, n))(f2 =>
        F.pure(oe =>
          F.flatMap(f1(oe))(_ => f2(oe))))))
}

// =====================================================================================================================

object RunCallbacks {

  @inline final implicit class InvariantOps[F[_], R, O, S, E](private val self: RunCallbacks[F, R, O, S, E]) extends AnyVal {
    import self._

    def when(f: (ROS[R, O, S], Name) => Boolean): RunCallbacks[F, R, O, S, E] = {
      val unit = F.pure(())
      val skip = F.pure((_: Option[E]) => unit)
      RunCallbacks((r, n) => if (f(r, n)) aroundAction(r, n) else skip)
    }

    def unless(f: (ROS[R, O, S], Name) => Boolean): RunCallbacks[F, R, O, S, E] =
      when(!f(_, _))
  }

  // ===================================================================================================================

  final class Dsl[F[_], R, O, S, E](implicit F: ExecutionModel[F]) {

    object aroundActions {

      @inline def apply(aroundAction: (ROS[R, O, S], Name) => F[Option[E] => F[Unit]]): RunCallbacks[F, R, O, S, E] =
        RunCallbacks(aroundAction)

      def before(f: (ROS[R, O, S], Name) => F[Unit]): RunCallbacks[F, R, O, S, E] =
        apply((ros, n) => F.pure(_ => f(ros, n)))

      def after(f: (ROS[R, O, S], Name, Option[E]) => F[Unit]): RunCallbacks[F, R, O, S, E] =
        apply((ros, n) => F.pure(oe => f(ros, n, oe)))

      def timed(f: (ROS[R, O, S], Name, Option[E], Duration) => F[Unit]): RunCallbacks[F, R, O, S, E] =
        apply((ros, n) => F.point {
          val start = Instant.now()
          oe => F.flatMap(F.point(Instant.now())) { end =>
            val dur = Duration.between(start, end)
            f(ros, n, oe, dur)
          }
        })
    }
  }

}
