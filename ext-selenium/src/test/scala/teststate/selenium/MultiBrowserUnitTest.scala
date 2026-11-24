package teststate.selenium

import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import org.openqa.selenium.WebDriver
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import teststate.selenium.TestUtil._
import utest._

object MultiBrowserUnitTest extends TestSuite {

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

  override def tests = Tests {

    "onNewDriverFailureDoesNotRetainBrowser" - {
      implicit val ts: StubTabSupport = new StubTabSupport
      var driversCreated = 0
      var onNewDriverSuccess = 0
      var failFirst = true

      val mb = MultiBrowser({ driversCreated += 1; null: WebDriver }, GrowthStrategy.singleBrowser)
        .onNewDriver { _ =>
          if (failFirst) {
            failFirst = false
            throw new RuntimeException("init boom")
          }
          onNewDriverSuccess += 1
        }

      intercept[RuntimeException] {
        val _ = mb.openTab()
      }

      val _ = mb.openTab()
      assertEq("driversCreated", driversCreated, 2)
      assertEq("onNewDriverSuccess", onNewDriverSuccess, 1)
    }

    "onNewTabRunsAfterOnNewDriverWithTempTab" - {
      implicit val ts: StubTabSupport = new StubTabSupport
      var log = Vector.empty[String]

      val mb = MultiBrowser(null: WebDriver, GrowthStrategy.singleBrowser)
        .onNewDriverWithTempTab { t =>
          log :+= "setup.start"
          t.use(_ => log :+= "setup.use")
          log :+= "setup.end"
        }
        .onNewTab { t =>
          log :+= "onNewTab.start"
          t.use(_ => log :+= "onNewTab.use")
          log :+= "onNewTab.end"
        }

      val _ = mb.openTab()
      assertEq(
        log,
        Vector("setup.start", "setup.use", "setup.end", "onNewTab.start", "onNewTab.use", "onNewTab.end")
      )
    }

    "onNewTabExceptionDoesNotDeadlockBrowser" - {
      implicit val ts: StubTabSupport = new StubTabSupport
      var failFirst = true

      val mb = MultiBrowser(null: WebDriver, GrowthStrategy.singleBrowser)
        .onNewDriverWithTempTab(_.use(_ => ()))
        .onNewTab { _ =>
          if (failFirst) {
            failFirst = false
            throw new RuntimeException("onNewTab boom")
          }
        }

      intercept[RuntimeException] {
        val _ = mb.openTab()
      }

      val res = Await.result(Future {
        val t2 = mb.openTab()
        t2.use(_ => "ok")
      }, 2.seconds)
      assertEq(res, "ok")
    }

  }
}
