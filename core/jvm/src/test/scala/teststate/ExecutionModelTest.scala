package teststate

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import teststate.Exports._
import teststate.TestUtil._
import utest._

object ExecutionModelTest extends TestSuite {

  override def tests = Tests {

    "scalaFuture" - {
      val EM = ExecutionModel.scalaFuture

      "schedule" - {
        "normal" - {
          val f = EM.schedule(Future.successful(123), Instant.now())
          val res = Await.result(f, 2.seconds)
          assertEq(res, 123)
        }

        "syncException" - {
          val f = EM.schedule[Int](throw new RuntimeException("schedule boom"), Instant.now())
          val err = Await.result(f.failed, 2.seconds)
          assertEq(err.getMessage, "schedule boom")
        }
      }

      "doFinally" - {
        "success" - {
          var lastRan = false
          val f = EM.doFinally[Int, Unit](
            Future.successful(42),
            Future { lastRan = true }
          )
          val res = Await.result(f, 2.seconds)
          assertEq(res, 42)
          assert(lastRan)
        }

        "mainFailedAsync" - {
          var lastRan = false
          val f = EM.doFinally[Int, Unit](
            Future.failed(new RuntimeException("main async fail")),
            Future { lastRan = true }
          )
          val err = Await.result(f.failed, 2.seconds)
          assertEq(err.getMessage, "main async fail")
          assert(lastRan)
        }

        "lastFailedAsync" - {
          val f = EM.doFinally[Int, Unit](
            Future.successful(42),
            Future.failed(new RuntimeException("last async fail"))
          )
          val err = Await.result(f.failed, 2.seconds)
          assertEq(err.getMessage, "last async fail")
        }

        "syncExceptionInMain" - {
          var lastRan = false
          val f = EM.doFinally[Int, Unit](
            throw new RuntimeException("main boom"),
            Future { lastRan = true }
          )
          val err = Await.result(f.failed, 2.seconds)
          assertEq(err.getMessage, "main boom")
          assert(lastRan)
        }

        "syncExceptionInLast" - {
          val f = EM.doFinally[Int, Unit](
            Future.successful(42),
            throw new RuntimeException("last boom")
          )
          val err = Await.result(f.failed, 2.seconds)
          assertEq(err.getMessage, "last boom")
        }

        "syncExceptionInBoth" - {
          var lastRan = false
          val f = EM.doFinally[Int, Unit](
            throw new RuntimeException("main boom"), {
              lastRan = true
              throw new RuntimeException("last boom")
            }
          )
          val err = Await.result(f.failed, 2.seconds)
          assertEq(err.getMessage, "last boom")
          assert(lastRan)
        }
      }

      "threadLeak" - {
        val before = Thread.getAllStackTraces.keySet().asScala.count(_.getName.contains("Timer"))
        for (_ <- 1 to 20) {
          val em = ExecutionModel.scalaFuture
          em.pure(1)
        }
        val after = Thread.getAllStackTraces.keySet().asScala.count(_.getName.contains("Timer"))
        assert(after - before <= 1)
      }
    }
  }
}
