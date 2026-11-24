package teststate.selenium

import cats.instances.string._
import cats.instances.vector._
import org.openqa.selenium.WebDriver
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import teststate.selenium.TestUtil._
import teststate.typeclass.ExecutionModel
import utest._

object TabTest extends TestSuite {

  private final class StubTabSupport extends TabSupport[WebDriver] {
    override type TabHandle = String
    private var nextId = 0
    private var current = "root"
    override def active()(implicit driver: WebDriver): String = current
    override def open(root: String)(implicit driver: WebDriver): String = {
      nextId += 1
      current = s"tab-$nextId"
      current
    }
    override def activate(tab: String)(implicit driver: WebDriver): Unit =
      current = tab
    override def closeActive()(implicit driver: WebDriver): Unit =
      current = "root"
  }

  private def newTab(): Tab[WebDriver] = {
    val ts = new StubTabSupport
    Tab(null: WebDriver, Mutex(), ts)("root")
  }

  private def aroundMod(label: String, append: String => Unit): Tab.ProcMod =
    new Tab.ProcMod {
      override def apply[F[_], A](proc: () => F[A])(implicit EM: ExecutionModel[F]): () => F[A] = () => {
        append(s"$label.before")
        EM.map(proc()) { a =>
          append(s"$label.after")
          a
        }
      }
    }

  override def tests = Tests {

    "afterFirstUseOrder" - {
      var log = Vector.empty[String]
      val t = newTab()
        .beforeFirstUse(_ => log :+= "beforeFirst")
        .afterFirstUse(_ => log :+= "afterFirst")
        .beforeEachUse(_ => log :+= "beforeEach")
        .afterEachUse(_ => log :+= "afterEach")

      t.use(_ => log :+= "use1")
      t.use(_ => log :+= "use2")

      assertEq(
        log,
        Vector(
          "beforeFirst", "beforeEach", "use1", "afterEach", "afterFirst",
          "beforeEach", "use2", "afterEach"
        )
      )
    }

    "aroundFirstUseWrapsFirstUse" - {
      var log = Vector.empty[String]
      val t = newTab()
        .aroundFirstUse(_ => aroundMod("aroundFirst", log :+= _))
        .aroundEachUse(_ => aroundMod("aroundEach", log :+= _))

      t.use(_ => log :+= "use1")
      t.use(_ => log :+= "use2")

      assertEq(
        log,
        Vector(
          "aroundFirst.before", "aroundEach.before", "use1", "aroundEach.after", "aroundFirst.after",
          "aroundEach.before", "use2", "aroundEach.after"
        )
      )
    }

    "useMOrderWithFuture" - {
      implicit val em: ExecutionModel[Future] = ExecutionModel.scalaFuture
      var log = Vector.empty[String]
      def record(s: String): Unit = synchronized { log :+= s }

      val t = newTab()
        .beforeFirstUse(_ => record("beforeFirst"))
        .afterFirstUse(_ => record("afterFirst"))
        .beforeEachUse(_ => record("beforeEach"))
        .afterEachUse(_ => record("afterEach"))

      val f1 = t.useM(_ => Future { Thread.sleep(20); record("useM1") }, 5.seconds, 5.millis)
      Await.result(f1, 5.seconds)

      val f2 = t.useM(_ => Future { Thread.sleep(20); record("useM2") }, 5.seconds, 5.millis)
      Await.result(f2, 5.seconds)

      assertEq(
        log,
        Vector(
          "beforeFirst", "beforeEach", "useM1", "afterEach", "afterFirst",
          "beforeEach", "useM2", "afterEach"
        )
      )
    }

  }
}
