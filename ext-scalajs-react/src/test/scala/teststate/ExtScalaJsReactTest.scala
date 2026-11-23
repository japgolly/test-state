package teststate

import cats.instances.int._
import cats.instances.long._
import japgolly.microlibs.testutil.TestUtil._
import japgolly.scalajs.react.AsyncCallback
import java.time.Instant
import scala.concurrent.ExecutionContext
import teststate.ExtScalaJsReact._
import teststate.typeclass.{ErrorHandler, ExecutionModel}
import utest._

object ExtScalaJsReactTest extends TestSuite {

  private def em: ExecutionModel[AsyncCallback] =
    implicitly[ExecutionModel[AsyncCallback]]

  override def tests = Tests {
    implicit val ec: ExecutionContext =
      scala.scalajs.concurrent.QueueExecutionContext.timeouts()

    "doFinallySuspendsAndRunsOnSyncThrow" - {
      var mainEval = 0
      var lastEval = 0
      var lastRan = 0

      val ac = em.doFinally[Int, Unit](
        { mainEval += 1; throw new RuntimeException("sync boom") },
        { lastEval += 1; AsyncCallback.delay { lastRan += 1 } }
      )

      assertEq(mainEval, 0)
      assertEq(lastEval, 0)

      ac.attempt.unsafeToFuture().map { res =>
        assert(res.isLeft)
        assertEq(mainEval, 1)
        assertEq(lastEval, 1)
        assertEq(lastRan, 1)
      }
    }

    "scheduleSuspendsTaskUntilDelay" - {
      var taskEvalAt = 0L
      val startAt = Instant.now().plusMillis(40)

      val ac = em.schedule(
        { taskEvalAt = System.currentTimeMillis(); AsyncCallback.pure(1) },
        startAt
      )

      assertEq(taskEvalAt, 0L)

      ac.unsafeToFuture().map { res =>
        assertEq(res, 1)
        assert(taskEvalAt >= startAt.toEpochMilli - 5)
      }
    }

    "scheduleImmediateWhenPast" - {
      var ranSync = false
      val ac = em.schedule(
        { ranSync = true; AsyncCallback.pure(42) },
        Instant.now().minusSeconds(1)
      )

      assert(!ranSync)
      val fut = ac.unsafeToFuture()
      assert(ranSync)

      fut.map(assertEq(_, 42))
    }

    "recoverSuspendsAndReEvaluatesPerRun" - {
      implicit val eh: ErrorHandler[Throwable] = ErrorHandler.id
      var calls = 0

      val ac = em.recover[Throwable, Int] {
        calls += 1
        if (calls == 1)
          throw new RuntimeException("first")
        else
          AsyncCallback.pure(teststate.data.Right(calls))
      }

      assertEq(calls, 0)

      for {
        r1 <- ac.unsafeToFuture()
        r2 <- ac.unsafeToFuture()
      } yield {
        assert(r1.isLeft)
        assertEq(r2.toOption.get, 2)
        assertEq(calls, 2)
      }
    }

  }
}
