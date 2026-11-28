package teststate.selenium

import cats.instances.int._
import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import teststate.data._
import teststate.selenium.TestUtil._
import teststate.typeclass.{ErrorHandler, ExecutionModel}
import utest._

object MutexTest extends TestSuite {

  override def tests = Tests {

    "unlockWhenNotLockedThrows" - {
      val m = Mutex()
      val _ = intercept[IllegalMonitorStateException](m.unlock())
    }

    "unlockFromWrongThreadThrowsAndKeepsLock" - {
      val m = Mutex()
      m.lock()
      try {
        Await.result(Future {
          val _ = intercept[IllegalMonitorStateException](m.unlock())
          assert(!m.tryLock(10.millis))
        }, 2.seconds)
      } finally
        m.unlock()

      val acquiredAfter = Await.result(Future {
        val ok = m.tryLock(10.millis)
        if (ok) m.unlock()
        ok
      }, 2.seconds)
      assert(acquiredAfter)
    }

    "reentrantLockAndUnlock" - {
      val m = Mutex()
      m.lock()
      m.lock()
      m.unlock()
      assert(!Await.result(Future(m.tryLock(10.millis)), 2.seconds))
      m.unlock()
      val ok = Await.result(Future {
        val acquired = m.tryLock(10.millis)
        if (acquired) m.unlock()
        acquired
      }, 2.seconds)
      assert(ok)
    }

    "monadicUnlocksAcrossThreads" - {
      implicit val em: ExecutionModel[Future] = ExecutionModel.scalaFuture
      val m = Mutex()
      val res = Await.result(
        m.monadic(Future { Thread.sleep(20); 42 }, 2.seconds, 5.millis),
        2.seconds
      )
      assertEq(res, 42)
      assert(m.tryLock(10.millis))
      m.unlock()
    }

    "monadicRetryIsStackSafe" - {
      val m = Mutex()
      val locked = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val released = new CountDownLatch(1)
      val holder = Future {
        m.lock()
        locked.countDown()
        val _ = release.await(5, TimeUnit.SECONDS)
        m.unlock()
        released.countDown()
      }
      assert(locked.await(5, TimeUnit.SECONDS))

      var retries = 0
      implicit val em: ExecutionModel[Id] = new ExecutionModel[Id] {
        override def point[A](a: => A): A = a
        override def pure[A](a: A): A = a
        override def map[A, B](a: A)(f: A => B): B = f(a)
        override def flatMap[A, B](a: A)(f: A => B): B = f(a)
        override def tailrec[A, B](a: A)(rec: A => A Or B): B = ExecutionModel.Immediate.tailrec[A, B](a)(rec)
        override def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => A): A = ExecutionModel.Immediate.tailrecA[A](a)(stop)(rec)
        override def recover[E, A](f: => Failure[E] Or A)(implicit attempt: ErrorHandler[E]): Failure[E] Or A = attempt.recover(f, Left(_))
        override def now: Instant = Instant.now()
        override def doFinally[A, B](main: => A, last: => B): A = try main finally { val _ = last }
        override def schedule[A](task: => A, startAt: Instant): A = {
          retries += 1
          if (retries == 15000) {
            release.countDown()
            assert(released.await(5, TimeUnit.SECONDS))
          }
          task
        }
      }

      try {
        val res = m.monadic[Id, Int](99, Duration.Zero, Duration.Zero)
        assertEq(res, 99)
        assertEq(retries, 15000)
      } finally {
        release.countDown()
        Await.result(holder, 5.seconds)
      }
    }

  }
}
