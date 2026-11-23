package teststate.selenium

import cats.instances.int._
import org.openqa.selenium.WebDriver
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

  }
}
