package teststate.selenium

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.firefox.FirefoxDriver
import scala.jdk.CollectionConverters._

/** A means of managing tabs in a browser.
  *
  * Selenium lacks this functionality directly.
  */
trait TabSupport[-D <: WebDriver] {

  type TabHandle

  /** Handle of the currently active tab. */
  def active()(implicit driver: D): TabHandle

  /** Opens a new tab. It will be the active tab upon return. */
  def open(root: TabHandle)(implicit driver: D): TabHandle

  /** Make the specified tab active (i.e. on with focus). */
  def activate(tab: TabHandle)(implicit driver: D): Unit

  /** Closes the currently active tab. */
  def closeActive()(implicit driver: D): Unit
}

object TabSupport {

  trait Typical[D <: WebDriver] extends TabSupport[D] {
    override final type TabHandle = String

    protected def _open()(implicit driver: D): Unit
    protected def _close()(implicit driver: D): Unit

    override def active()(implicit driver: D): TabHandle =
      driver.getWindowHandle

    override def open(root: TabHandle)(implicit driver: D): TabHandle = {
      activate(root)
      val before = driver.getWindowHandles.asScala.toSet
      _open()
      val added = driver.getWindowHandles.asScala.diff(before)
      assert(added.size == 1, s"Tab creation failed. Expected 1 new windowHandle, got ${added.size}")
      val tab = added.head
      activate(tab)
      tab
    }

    override def activate(tab: TabHandle)(implicit driver: D): Unit = {
      driver.switchTo.window(tab)
      assert(driver.getWindowHandle == tab, "Failed to switch tabs.")
    }

    override def closeActive()(implicit driver: D): Unit = {
      val expect = driver.getWindowHandles.size - 1
      if (expect == 0) {
        // Firefox: _close() doesn't work
        // Chrome: after _close() driver.getWindowHandles throws
        driver.close()
      } else {
        _close()
        val after = driver.getWindowHandles.size
        assert(after == expect, s"Failed to close tab. Expected tab count to become $expect, but is instead $after")
      }
    }
  }

  implicit object Chrome extends Typical[ChromeDriver] {
    override protected def _open()(implicit driver: ChromeDriver): Unit = {
      driver.executeScript("window.open()")
      ()
    }

    override protected def _close()(implicit driver: ChromeDriver): Unit = {
      driver.executeScript("window.close()")
      ()
    }
  }

  implicit object Firefox extends Typical[FirefoxDriver] {
    override protected def _open()(implicit driver: FirefoxDriver): Unit = {
      driver.executeScript("window.open()")
      ()
    }

    override protected def _close()(implicit driver: FirefoxDriver): Unit = {
      driver.executeScript("window.close()")
      ()
    }
  }

}