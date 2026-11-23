package teststate.selenium

import cats.instances.int._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import teststate.selenium.TestUtil._
import teststate.typeclass.ExecutionModel
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

  }
}
