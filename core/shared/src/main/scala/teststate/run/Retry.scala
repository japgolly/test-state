package teststate.run

import java.time.{Duration => JavaDuration, Instant}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration => ScalaDuration}
import teststate.data._
import teststate.typeclass.{ErrorHandler, ExecutionModel}

object Retry {

  /** Instant Arg = current time
    * Output = Time for which to schedule a retry
    */
  final case class Policy(nextTry: (Ctx, Instant) => Option[Instant]) extends AnyVal {

    def unsafeRetryOnException[A](scope: Scope, a: => A)(implicit EM: ExecutionModel[Id]): A =
      unsafeRetryOnException(scope, a, a)

    def unsafeRetryOnException[A](scope: Scope, first: => A, subsequent: => A)(implicit EM: ExecutionModel[Id]): A = {
      type X = Failure.WithCause[Throwable] Or A
      def firstId: Id[X] = ErrorHandler.id.attempt(first)
      def subsequentId: Id[X] = ErrorHandler.id.attempt(subsequent)
      retryI(scope, firstId)(_.isLeft, subsequentId).recover[A](f => throw f.theCause)
    }

    def retry[F[_], A](scope: Scope, task: => F[A])(failed: A => Boolean)
                      (implicit EM: ExecutionModel[F]): F[A] =
      EM.flatMap(task)(retryI(scope, _)(failed, task))

    def retryI[F[_], A](scope: Scope, initialA: A)(failed: A => Boolean, retryF: => F[A])
                       (implicit EM: ExecutionModel[F]): F[A] =
      if (failed(initialA)) {
        type InProgress = (A, Option[Retry.Ctx])
        type Out = InProgress Or A
        EM.tailrec[InProgress, A]((initialA, None)) {
          case (a, retryCtx1) =>
            if (failed(a))
              EM.flatMap(EM.now) { now2 =>
                val retryCtx2 = retryCtx1.fold(Retry.Ctx.init(scope, now2))(_.add(now2))
                val result: F[Out] =
                  nextTry(retryCtx2, now2) match {
                    case Some(at) =>
                      EM.schedule(EM.map(retryF)(a2 => Left((a2, Some(retryCtx2))): Out), at)
                    case None =>
                      // No more retries
                      EM.pure(Right(a))
                  }
                result
              }
            else
              EM.pure(Right(a))
        }
      } else
        EM.pure(initialA)
  }

  object Policy {

    def never: Policy =
      apply((_, _) => None)

    /** @param maxAttempts This many retries will be attempted resulting in this+1 failures before giving up.
      * @param interval Interval to wait between attempts.
      */
    def fixedIntervalAndAttempts(interval: Long, intervalTU: TimeUnit, maxAttempts: Int): Policy =
      fixedIntervalAndAttempts(intervalTU.toMillis(interval), maxAttempts)

    /** @param maxAttempts This many retries will be attempted resulting in this+1 failures before giving up.
      * @param interval Interval to wait between attempts.
      */
    def fixedIntervalAndAttempts(interval: JavaDuration, maxAttempts: Int): Policy =
      fixedIntervalAndAttempts(interval.toMillis, maxAttempts)

    /** @param maxAttempts This many retries will be attempted resulting in this+1 failures before giving up.
      * @param interval Interval to wait between attempts.
      */
    def fixedIntervalAndAttempts(interval: ScalaDuration, maxAttempts: Int): Policy =
      fixedIntervalAndAttempts(interval.toMillis, maxAttempts)

    /** @param maxAttempts This many retries will be attempted resulting in this+1 failures before giving up.
      * @param intervalMs Interval to wait between attempts.
      */
    def fixedIntervalAndAttempts(intervalMs: Long, maxAttempts: Int): Policy =
      apply((ctx, _) =>
        if (ctx.retryCount < maxAttempts)
          Some(ctx.lastFailure.plusMillis(intervalMs))
        else
          None)

    def fixedIntervalWithTimeout(interval: Long, intervalTU: TimeUnit, timeout: Long, timeoutTU: TimeUnit): Policy =
      fixedIntervalWithTimeout(intervalTU.toMillis(interval), timeoutTU.toMillis(timeout))

    def fixedIntervalWithTimeout(interval: JavaDuration, timeout: JavaDuration): Policy =
      fixedIntervalWithTimeout(interval.toMillis, timeout.toMillis)

    def fixedIntervalWithTimeout(interval: ScalaDuration, timeout: ScalaDuration): Policy =
      fixedIntervalWithTimeout(interval.toMillis, timeout.toMillis)

    def fixedIntervalWithTimeout(intervalMs: Long, timeoutMs: Long): Policy =
      apply { (ctx, now) =>
        val deadline = ctx.firstFailure.plusMillis(timeoutMs)
        if (now.isAfter(deadline)) {
          // Because retries are scheduled they aren't guaranteed to execute on time
          // It isn't uncommon with high concurrency to see one attempt then a huuuuugue delay due to
          // thread contention, then finally we arrive here and see that now+interval > deadline
          // Before giving up, we should try once *at* the deadline.
          if (ctx.lastFailure.isBefore(deadline))
            Some(now)
          else
            None
        } else
          Some {
            val t = ctx.lastFailure.plusMillis(intervalMs)
            if (t.isAfter(deadline))
              deadline
            else if (t.isBefore(now))
              now
            else
              t
          }
      }
  }

  // ===================================================================================================================

  final case class Ctx(scope: Scope, failuresFirstToSecondLast: Vector[Instant], lastFailure: Instant) {
    def firstFailure: Instant =
      failuresFirstToSecondLast.headOption getOrElse lastFailure

    def failuresFirstToLast: Vector[Instant] =
      failuresFirstToSecondLast :+ lastFailure

    def retryCount: Int =
      failuresFirstToSecondLast.length

    def failureCount: Int =
      retryCount + 1

    def add(newerFailure: Instant): Ctx =
      Ctx(scope, failuresFirstToLast, newerFailure)
  }

  object Ctx {
    def init(scope: Scope, at: Instant): Ctx =
      apply(scope, Vector.empty, at)
  }

  sealed trait Scope
  object Scope {
    case object Reference extends Scope
    case object Observation extends Scope
    case object InitialInvariants extends Scope
    case object PreConditions extends Scope
    case object Action extends Scope
    /** Re-observation and state update */
    case object PostAction extends Scope
    case object PostConditions extends Scope
  }
}
