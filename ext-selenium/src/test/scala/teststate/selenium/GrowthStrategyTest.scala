package teststate.selenium

import japgolly.microlibs.testutil.TestUtil._
import scalaz.std.string._
import utest._

object GrowthStrategyTest extends TestSuite {

  private class Tester(strategy: GrowthStrategy) {
    override def toString = s"${strategy.toString} : ${state.mkString("[", ",", "]")}"

    var state = Array.empty[Int]

    def closeTab(browser: Int)(expect: Int*): this.type = {
      state(browser) = state(browser) - 1
      assertState(expect: _*)
      this
    }

    def closeBrowser(browser: Int)(expect: Int*): this.type = {
      state = state.zipWithIndex.filter(_._2 != browser).map(_._1)
      assertState(expect: _*)
      this
    }

    def testAndApply(expect: Int*): this.type = {
      val i = strategy.nextBrowser(state.toVector)
      if (state.indices.contains(i))
        state(i) = state(i) + 1
      else
        state :+= 1
      assertState(expect: _*)
      this
    }

    def assertState(expect: Int*): Unit =
      assertEq(state.mkString(","), expect.mkString(","))
  }

  override def tests = Tests {

    "singleBrowser" - {
      new Tester(GrowthStrategy.singleBrowser)
        .testAndApply(1)
        .testAndApply(2)
        .testAndApply(3)
        .closeTab(0)(2)
        .testAndApply(3)
        .testAndApply(4)
        .closeBrowser(0)()
        .testAndApply(1)
        .testAndApply(2)
    }

    "maxBrowsers" - {
      new Tester(GrowthStrategy.maxBrowsers(3))
        .testAndApply(1)
        .testAndApply(1, 1)
        .closeTab(0)(0, 1)
        .testAndApply(1, 1)
        .testAndApply(1, 1, 1)
        .testAndApply(2, 1, 1)
        .testAndApply(2, 2, 1)
        .testAndApply(2, 2, 2)
        .testAndApply(3, 2, 2)
        .closeTab(2)(3, 2, 1)
        .testAndApply(3, 2, 2)
        .closeTab(1)(3, 1, 2)
        .testAndApply(3, 2, 2)
        .closeTab(0)(2, 2, 2)
        .testAndApply(3, 2, 2)
        .closeTab(1)(3, 1, 2)
        .closeTab(1)(3, 0, 2)
        .testAndApply(3, 1, 2)
        .testAndApply(3, 2, 2)
        .closeBrowser(0)(2, 2)
        .testAndApply(2, 2, 1)
        .testAndApply(2, 2, 2)
    }

    "maxTabs" - {
      new Tester(GrowthStrategy.maxTabs(3))
        .testAndApply(1)
        .testAndApply(2)
        .testAndApply(3)
        .testAndApply(3, 1)
        .testAndApply(3, 2)
        .testAndApply(3, 3)
        .testAndApply(3, 3, 1)
        .closeTab(1)(3, 2, 1)
//        .testAndApply(3, 2, 2) // browser already open
        .testAndApply(3, 3, 1)
        .closeTab(1)(3, 2, 1)
        .closeTab(1)(3, 1, 1)
        .closeTab(1)(3, 0, 1)
        .testAndApply(3, 1, 1)
        .testAndApply(3, 2, 1)
        .closeTab(0)(2, 2, 1)
        .testAndApply(3, 2, 1)
        .closeTab(2)(3, 2, 0)
        .testAndApply(3, 3, 0)
        .testAndApply(3, 3, 1)
    }

  }
}
