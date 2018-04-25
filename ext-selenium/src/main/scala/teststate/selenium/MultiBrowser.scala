package teststate.selenium

import org.openqa.selenium.WebDriver

/** Create tabs across multiple browsers */
trait MultiBrowser[D <: WebDriver] extends MultiTab[D] {
  def closeBrowser(browserIndex: Int, quit: Boolean = true): Unit
  def closeAllBrowsers(quit: Boolean = true): Unit
}

object MultiBrowser {
  def apply[D <: WebDriver](newDriver: => D, growthStrategy: GrowthStrategy)
                           (implicit tabSupport: TabSupport[D]): MultiBrowser[D] =
    new MultiBrowser[D] {
      import tabSupport.TabHandle

      private val outerMutex = Mutex()

      private case class Browser(driver  : D,
                                 mutex   : Mutex,
                                 rootTab : TabHandle,
                                 multiTab: MultiTab[D],
                                 tabs    : Vector[Tab[D]])

      private var instances: Vector[Browser] =
        Vector.empty

      // Locks: outer
      override def openTab(): Tab[D] = outerMutex {
        val browserIndex = {
          val i = growthStrategy.nextBrowser(instances.map(_.tabs.length))
          if (instances.indices.contains(i))
            i
          else {
            val driver   = newDriver
            val mutex    = Mutex.blocking()
            val rootTab  = tabSupport.active()(driver)
            val multiTab = MultiTab(driver, mutex)(tabSupport)(rootTab)
            instances = instances :+ Browser(driver, mutex, rootTab, multiTab, Vector.empty)
            instances.length - 1
          }
        }
        val browser  = instances(browserIndex)
        var tab      = null: Tab[D]
        tab          = browser.multiTab.openTab().withOnClose(removeTab(browser.driver, tab))
        val browser2 = browser.copy(tabs = browser.tabs :+ tab)
        instances    = instances.updated(browserIndex, browser2)
        tab
      }

      // Locks: outer
      private def removeTab(driver: D, tab: Tab[D]): Unit = outerMutex {
        instances.indexWhere(_.driver eq driver) match {
          case -1 =>
          case i =>
            val browser = instances(i)
            val newTabs = browser.tabs.filter(_ ne tab)
            if (newTabs.length != browser.tabs.length)
              instances = instances.updated(i, browser.copy(tabs = newTabs))
        }
      }

      // Locks: outer(browser)
      override def closeBrowser(i: Int, quit: Boolean = true): Unit =
        outerMutex {
          for (b <- instances.lift(i)) {
            b.mutex {
              if (quit)
                b.driver.quit()
              // Hmmm ↑ this would cause tab.closeTab() to fail
              else {
                implicit def d = b.driver
                b.tabs.foreach(_.closeTab()) // locks browser,outer
                tabSupport.activate(b.rootTab) // just in case
                b.driver.close() // closes root which should close the browser
              }
            }
            instances = instances.filter(_ ne b)
          }
        }

      // Locks: outer(browser)
      override def closeAllBrowsers(quit: Boolean = true): Unit =
        outerMutex {
          // Parallelism would be nice
          instances.indices.reverse.foreach(closeBrowser(_, quit))
        }
    }
}
