package teststate.run

import acyclic.file
import java.time.Instant
import scala.concurrent.duration.Duration
import teststate.data._
import teststate.typeclass.{Attempt, ExecutionModel}

object Retry {

  /** Instant Arg = current time
    * Output = Time for which to schedule a retry
    */
  final case class Policy(nextTry: (Ctx, Instant) => Option[Instant]) extends AnyVal {

    def unsafeRetryOnException[A](a: => A)(implicit EM: ExecutionModel[Id]): A =
      unsafeRetryOnException(a, a)

    def unsafeRetryOnException[A](first: => A, subsequent: => A)(implicit EM: ExecutionModel[Id]): A = {
      type X = Failure.WithCause[Throwable] Or A
      def firstId: Id[X] = Attempt.id.attempt(first)
      def subsequentId: Id[X] = Attempt.id.attempt(subsequent)
      retryI(firstId)(_.isLeft, subsequentId).recover[A](f => throw f.theCause)
    }

    def retry[F[_], A](task: => F[A])(failed: A => Boolean)
                      (implicit EM: ExecutionModel[F]): F[A] =
      EM.flatMap(task)(retryI(_)(failed, task))

    def retryI[F[_], A](initialA: A)(failed: A => Boolean, retryF: => F[A])
                       (implicit EM: ExecutionModel[F]): F[A] =
      if (failed(initialA)) {
        type InProgress = (A, Option[Retry.Ctx])
        type Out = InProgress Or A
        EM.tailrec[InProgress, A]((initialA, None)) {
          case (a, retryCtx1) =>
            if (failed(a))
              EM.flatMap(EM.now) { now2 =>
                val retryCtx2 = retryCtx1.fold(Retry.Ctx.init(now2))(_.add(now2))
                val result: F[Out] =
                  nextTry(retryCtx2, now2) match {
                    case Some(at) =>
                      EM.schedule(EM.map(retryF)(a2 => Left((a2, Some(retryCtx2))): Out), at)
                    case None =>
                      // No more retries
                      EM.pure(Right(initialA))
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
    def fixedIntervalAndAttempts(interval: Duration, maxAttempts: Int): Policy =
      apply((ctx, _) =>
        if (ctx.retryCount < maxAttempts)
          Some(ctx.lastFailure.plusMillis(interval.toMillis))
        else
          None)

    def fixedIntervalWithTimeout(interval: Duration, timeout: Duration): Policy =
      fixedIntervalWithTimeout(interval.toMillis, timeout.toMillis)

    def fixedIntervalWithTimeout(intervalMs: Long, timeoutMs: Long): Policy =
      apply { (ctx, now) =>
        val limit = ctx.firstFailure.plusMillis(timeoutMs)
        if (now.isAfter(limit))
          None
        else
          Some(ctx.lastFailure.plusMillis(intervalMs))
      }
  }

  // ===================================================================================================================

  final case class Ctx(failuresFirstToSecondLast: Vector[Instant], lastFailure: Instant) {
    def firstFailure: Instant =
      failuresFirstToSecondLast.headOption getOrElse lastFailure

    def failuresFirstToLast: Vector[Instant] =
      failuresFirstToSecondLast :+ lastFailure

    def retryCount: Int =
      failuresFirstToSecondLast.length

    def failureCount: Int =
      retryCount + 1

    def add(newerFailure: Instant): Ctx =
      Ctx(failuresFirstToLast, newerFailure)
  }

  object Ctx {
    def init(at: Instant): Ctx =
      apply(Vector.empty, at)
  }
}
