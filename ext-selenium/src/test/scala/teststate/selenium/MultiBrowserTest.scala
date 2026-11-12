package teststate.selenium

import cats.instances.string._
import java.util.concurrent.{CountDownLatch, TimeUnit}
import org.openqa.selenium.chrome.ChromeDriver
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import teststate.selenium.TestUtil._
import utest._

object MultiBrowserTest extends TestSuite {

  implicit lazy val d: ChromeDriver = newChrome()
  lazy val mb = MultiBrowser(d, GrowthStrategy.singleBrowser)
  def tabCount() = d.getWindowHandles.size

  def testOpenAndClose(): Unit = {
    var beforeFirst = 0
    var afterFirst = 0
    var beforeEach = 0
    var afterEach = 0
    def assertCB(expectBeforeFirst: Int,
                 expectAfterFirst: Int,
                 expectBeforeEach: Int,
                 expectAfterEach: Int): Unit =
      assertEq(
        s"$beforeFirst, $afterFirst, $beforeEach, $afterEach",
        s"$expectBeforeFirst, $expectAfterFirst, $expectBeforeEach, $expectAfterEach")

    val t = mb.openTabTo(google)
      .beforeFirstUse(_ => beforeFirst += 1)
      .afterFirstUse(_ => afterFirst += 1)
      .beforeEachUse(_ => beforeEach += 1)
      .afterEachUse(_ => afterEach += 1)
    assertCB(0, 0, 0, 0) // tab usage is lazy
    assertEq("Tab count before new tab is used", tabCount(), 1)
    assertContains(t.use(_.getCurrentUrl), google) // assertContains because https://www.google.com/?zx=1794277290288
    assertCB(1, 1, 1, 1)
    assertContains(t.use(_.getCurrentUrl), google)
    assertCB(1, 1, 2, 2)
    assertEq("Tab count after new tab is used", tabCount(), 2)
    assertEq("Tab close when effective", t.closeTab(), true)
    assertEq("Tab count after tab closed", tabCount(), 1)
    assertEq("Tab close when not effective", t.closeTab(), false)
    assertEq("Tab count after double close", tabCount(), 1)
  }

  override def tests = Tests {

    "onNewDriverWithTempTab" - {
      var tabCountAtTempTab = -1
      var tempTabInvocations = 0
      mb.onNewDriverWithTempTab { t =>
        t.use { _ =>
          tabCountAtTempTab = tabCount()
          tempTabInvocations += 1
        }
      }
      val t = mb.openTab()
      t.use { _ =>
        assertEq("open tabs (A)", tabCount(), 2)
      }
      assertEq("open tabs (B)", tabCount(), 2)
      t.closeTab()
      assertEq("open tabs (C)", tabCount(), 1)
      assertEq("open tabs (S)", tabCountAtTempTab, 2)
      assertEq("invocations (S)", tempTabInvocations, 1)
    }

    "openAndClose1" - testOpenAndClose()
    "openAndClose2" - testOpenAndClose()

    "setupTabNotTriggerOnNewTab" - {
      var newTabs = Vector.empty[Tab[ChromeDriver]]
      var setupTab: Tab[ChromeDriver] = null

      val mb2 = MultiBrowser(newChrome(), GrowthStrategy.singleBrowser)
        .onNewDriverWithTempTab(t => setupTab = t)
        .onNewTab(t => newTabs :+= t)

      try {
        val userTab = mb2.openTab()
        assertEq("onNewTab call count", newTabs.length, 1)
        assert(newTabs.head eq userTab)
        assert(setupTab != null)
        assert(!newTabs.exists(_ eq setupTab))
      } finally {
        mb2.close()
      }
    }

    "growthStrategyTabCountNotInflatedByTempTab" - {
      val setupStarted = new CountDownLatch(1)
      val secondTabRequested = new CountDownLatch(1)

      var browserCount = 0
      def createDriver() = {
        browserCount += 1
        newChrome()
      }

      var observedTabCounts = Vector.empty[IndexedSeq[Int]]
      val recordingStrategy = GrowthStrategy { counts =>
        observedTabCounts :+= counts
        if (counts.nonEmpty)
          secondTabRequested.countDown()
        GrowthStrategy.maxTabs(2).nextBrowser(counts)
      }

      val mb2 = MultiBrowser(createDriver(), recordingStrategy)
        .onNewDriverWithTempTab { _ =>
          setupStarted.countDown()
          if (!secondTabRequested.await(5, TimeUnit.SECONDS))
            sys.error("Timed out waiting for secondTabRequested")
        }

      try {
        val t1 = Future { mb2.openTab() }

        if (!setupStarted.await(5, TimeUnit.SECONDS))
          sys.error("Timed out waiting for setupStarted")

        val t2 = Future { mb2.openTab() }

        Await.result(t1, 10.seconds)
        Await.result(t2, 10.seconds)

        assert(observedTabCounts.last == Vector(1))
        assertEq("Number of browsers created", browserCount, 1)
      } finally {
        mb2.close()
      }
    }

    "close" - mb.close()
  }
}
