package teststate.selenium

import scalaz.std.string._
import scalaz.std.anyVal._
import TestUtil._
import utest._

object MultiBrowserTest extends TestSuite {

  implicit lazy val d = newChrome()
  lazy val mb = MultiBrowser(d, GrowthStrategy.singleBrowser)
  def tabCount() = d.getWindowHandles.size

  def testOpenAndClose(): Unit = {
    var beforeFirst = 0
    var afterFirst = 0
    var beforeEach = 0
    var afterEach = 0
    def assertCB(
                  expectBeforeFirst: Int,
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
    assertEq(t.use(_.getCurrentUrl), google)
    assertCB(1, 1, 1, 1)
    assertEq(t.use(_.getCurrentUrl), google)
    assertCB(1, 1, 2, 2)
    assertEq("Tab count after new tab is used", tabCount(), 2)
    assertEq("Tab close when effective", t.closeTab(), true)
    assertEq("Tab count after tab closed", tabCount(), 1)
    assertEq("Tab close when not effective", t.closeTab(), false)
    assertEq("Tab count after double close", tabCount(), 1)
  }

  override def tests = TestSuite {

    'onNewDriverWithTempTab - {
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

    'openAndClose1 - testOpenAndClose()
    'openAndClose2 - testOpenAndClose()

    'close - mb.close()
  }
}
