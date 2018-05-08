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

  override def tests = CI match {
    case Some(_) => TestSuite {}
    case None => TestSuite {

      'openAndClose1 - testOpenAndClose()
      'openAndClose2 - testOpenAndClose()

      'close - mb.closeAllBrowsers()
    }
  }
}
