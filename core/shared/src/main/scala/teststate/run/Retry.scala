package teststate.run

import java.time.Instant
import scala.concurrent.duration.Duration

object Retry {

  /** Instant Arg = current time
    * Output = Time for which to schedule a retry
    */
  type Policy = (Ctx, Instant) => Option[Instant]

  object Policy {

    def never: Policy =
      (_, _) => None

    /** @param maxAttempts This many retries will be attempted resulting in this+1 failures before giving up.
      * @param interval Interval to wait between attempts.
      */
    def fixedIntervalAndAttempts(interval: Duration, maxAttempts: Int): Policy =
      (ctx, _) =>
        if (ctx.retryCount < maxAttempts)
          Some(ctx.lastFailure.plusMillis(interval.toMillis))
        else
          None

    def fixedIntervalWithTimeout(interval: Duration, timeout: Duration): Policy =
      fixedIntervalWithTimeout(interval.toMillis, timeout.toMillis)

    def fixedIntervalWithTimeout(intervalMs: Long, timeoutMs: Long): Policy =
      (ctx, now) => {
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
