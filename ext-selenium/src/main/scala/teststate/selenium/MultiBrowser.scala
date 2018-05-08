package teststate.selenium

import java.util.concurrent.Executors
import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Create tabs across multiple browsers */
trait MultiBrowser[+D <: WebDriver] extends MultiTab[D] {
  def closeBrowser(browserIndex: Int, quit: Boolean = true): Unit
  def closeAllBrowsers(quit: Boolean = true): Unit

  /** Creates a brand-new, unrelated MultiBrowser instance that, for all new driver instances before use,
    * executes the given proc on the driver. */
  def withDriverSetup(f: D => Unit): MultiBrowser[D]

  /** Creates a brand-new, unrelated MultiBrowser instance that, for all new driver instances before use,
    * creates a new tab, runs the given proc, then closes the tab. */
  def withSetupInTab(f: Tab[D] => Unit): MultiBrowser[D]
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

      override def withDriverSetup(f: D => Unit): MultiBrowser[D] = {
        def newDriver2 = {
          val d = newDriver
          f(d)
          d
        }
        MultiBrowser(newDriver2, growthStrategy)(tabSupport)
      }

      override def withSetupInTab(f: Tab[D] => Unit): MultiBrowser[D] =
        withDriverSetup { implicit d =>
          val root = tabSupport.active()
          val tab = Tab(d, Mutex(), tabSupport)(root)
          f(tab)
          tab.closeTab()
          tabSupport.activate(root)
        }

      // Locks: outer
      override def openTab(): Tab[D] = outerMutex {
        val browserIndex = {
          val i = growthStrategy.nextBrowser(instances.map(_.tabs.length))
          if (instances.indices.contains(i))
            i
          else {
            val driver   = newDriver
            val mutex    = Mutex()
            val rootTab  = tabSupport.active()(driver)
            val multiTab = MultiTab(driver, mutex)(tabSupport)(rootTab)
            instances = instances :+ Browser(driver, mutex, rootTab, multiTab, Vector.empty)
            instances.length - 1
          }
        }
        val browser  = instances(browserIndex)
        var tab      = null: Tab[D]
        tab          = browser.multiTab.openTab().afterClose(removeTab(_, tab))
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
      override def closeAllBrowsers(quit: Boolean = true): Unit = {
        val after: () => Unit =
          outerMutex {
            val bs = instances
            bs.length match {
              case 0 =>
                () => ()
              case 1 =>
                _closeBrowser(bs.head, quit)
                () => ()
              case n =>
                val threadPool = Executors.newFixedThreadPool(n)
                implicit val ec = ExecutionContext.fromExecutorService(threadPool)
                val f = Future.traverse(bs)(b => Future(_closeBrowser(b, quit)))
                () => {
                  Await.result(f, Duration.Inf)
                  threadPool.shutdown()
                  ec.shutdown()
                }
            }
          }
        after()
      }

      // Locks: outer(browser)
      override def closeBrowser(i: Int, quit: Boolean = true): Unit =
        outerMutex {
          instances.lift(i).foreach(_closeBrowser(_, quit))
        }

      // Locks: outer(browser)
      private def _closeBrowser(b: Browser, quit: Boolean): Unit =
        outerMutex {
          if (instances.exists(_ eq b)) {
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

    }
}
