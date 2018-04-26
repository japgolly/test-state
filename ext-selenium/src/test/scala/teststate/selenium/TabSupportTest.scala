package teststate.selenium

import utest._
import TestUtil._
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}
import scala.collection.JavaConverters._
import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.vector._

object TabSupportTest extends TestSuite {

  def test[D <: WebDriver](driver: D, url: Int => String)(implicit tabSupport: TabSupport[D]): Unit =
    try {
      import tabSupport.TabHandle
      implicit val equalTH = Equal.equalA[TabHandle]
      implicit def d = driver
      val root = tabSupport.active()
      def tabs() = driver.getWindowHandles().asScala.toVector.sorted
      def tabsOpen() = driver.getWindowHandles().size

      def testAdd(): TabHandle = {
        val tab = assertDifference(tabsOpen())(1) {
          val before = tabs()
          val t = tabSupport.open(root)
          assertEq(tabs(), (before :+ t.asInstanceOf[String]).sorted)
          t
        }
        assertNoChange("Activating new tab does nothing", tabSupport.active())(tabSupport.activate(tab))
        assertEq("Active tab is new tab", tabSupport.active(), tab)
        tab
      }

      def testClose(tab: TabHandle): Unit = {
        tabSupport.activate(tab)
        assertEq(tabSupport.active(), tab)
        assertDifference(tabsOpen())(-1) {
          val before = tabs()
          tabSupport.closeActive()
          assertEq(tabs(), before.filterNot(_ == tab))
        }
      }

      val tab1 = testAdd()
      val tab2 = testAdd()
      assert(tab1 != tab2)
      tabSupport.activate(tab1)
      assertEq(tabSupport.active(), tab1)
      val tab3 = testAdd()
      assert(tab1 != tab3, tab2 != tab3)
      driver.get(url(3))
      tabSupport.activate(tab2); driver.get(url(2))
      tabSupport.activate(tab1); driver.get(url(1))
      tabSupport.activate(tab2); assertEq(driver.getCurrentUrl, url(2))
      tabSupport.activate(tab1); assertEq(driver.getCurrentUrl, url(1))
      tabSupport.activate(tab3); assertEq(driver.getCurrentUrl, url(3))
      testClose(tab2)
      testClose(tab1)
      val tab4 = testAdd()
      tabSupport.activate(tab3); assertEq(driver.getCurrentUrl, url(3))
      testClose(tab3)
      testClose(tab4)

    } finally
      driver.quit()

  override def tests = CI match {
    case None => TestSuite {

      'chrome {
        val options = new ChromeOptions()
        options.setHeadless(true)
        test(new ChromeDriver(options), i => s"file://nope-$i.html/")
      }

      'firefox {
        val options = new FirefoxOptions()
        options.setHeadless(true)
        test(new FirefoxDriver(options), "https://www.google.com/?tab=" + _)
      }
    }

    case Some(_) => TestSuite {}
  }
}
