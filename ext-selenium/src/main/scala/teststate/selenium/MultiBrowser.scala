package teststate.selenium

import java.util.concurrent.Executors
import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import Internal._

/** Creates tabs across multiple browsers.
  *
  * Note: This is mutable.
  */
trait MultiBrowser[+D <: WebDriver] extends MultiTab[D] {

  /** Closes all opened browsers.
    *
    * @param quit If true, calls `.quit()` on [[WebDriver]] which terminates the browser entirely, regardless of its state.
    *             If false, closes all managed tabs and then the root tab which, if no other tabs are open, results in
    *             the browser instance closing.
    */
  def close(quit: Boolean = true): Unit

  /** Close all tabs in all browsers that have been opened by this. */
  def closeManagedTabs(): Unit

  /** Close all root (empty) tabs in all browsers, and disassociate the browsers if they remain open.
    *
    * Caution: this will prevent associated [[Tab]] instances from focusing themselves.
    * Do not call this method if you intend to continue using associated tabs.
    */
  def closeRoot(): Unit

  def onNewDriver(f: D => Unit): this.type
  def onNewDriverWithTempTab(f: Tab[D] => Unit): this.type
  def onNewTab(f: Tab[D] => Unit): this.type
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
                                 tabs    : Vector[Tab[D]]) {
        def closeRoot(): Unit = mutex {
          implicit def d = driver
          tabSupport.activate(rootTab)
          tabSupport.closeActive()
        }
      }

      private var instances    = Vector.empty[Browser]
      private var onNewDriver  = doNothing1: D => Unit
      private var onNewDriverT = doNothing1: Tab[D] => Unit
      private var onNewTab     = doNothing1: Tab[D] => Unit

      // Locks: outer
      override def onNewDriver(f: D => Unit): this.type =
        outerMutex {
          onNewDriver = onNewDriver >> f
          this
        }

      // Locks: outer
      override def onNewDriverWithTempTab(f: Tab[D] => Unit): this.type =
        outerMutex {
          onNewDriverT = onNewDriverT >> f
          this
        }

      // Locks: outer
      override def onNewTab(f: Tab[D] => Unit): this.type =
        outerMutex {
          onNewTab = onNewTab >> f
          this
        }

      // Expects outer lock
      private def createTabWithoutLocking(browserIndex: Int): Tab[D] = {
        val browser  = instances(browserIndex)
        var tab      = null: Tab[D]
        tab          = browser.multiTab.openTab().afterClosed(removeTab(browser.driver, tab))
        val browser2 = browser.copy(tabs = browser.tabs :+ tab)
        instances    = instances.updated(browserIndex, browser2)
        onNewTab(tab)
        tab
      }

      // Locks: outer, browser
      override def openTab(): Tab[D] = {

        // Hold outer lock and be quick
        val (tab, setup) = outerMutex {
          val i = growthStrategy.nextBrowser(instances.map(_.tabs.length))

          if (instances.indices.contains(i)) {
            // Use existing browser
            val browserIndex = i
            val tab = createTabWithoutLocking(browserIndex)
            (tab, None)

          } else {
            // Start new browser
            val driver       = newDriver
            val mutex        = Mutex()
            val rootTab      = tabSupport.active()(driver)
            val multiTab     = MultiTab(driver, mutex)(tabSupport)(rootTab)
            val browser      = Browser(driver, mutex, rootTab, multiTab, Vector.empty)
            val browserIndex = instances.length
            instances = instances :+ browser

            onNewDriver(driver)

            val setup =
              if (onNewDriverT eq doNothing1) None else Some {
                val setupTab    = createTabWithoutLocking(browserIndex)
                val setupFn     = onNewDriverT
                val browserLock = mutex.lock
                // Lock browser so that no one else can create a new tab in this browser before setup completes
                browserLock.lock()
                () => {
                  try setupFn(setupTab)
                  finally browserLock.unlock()
                  // closeTab() will attempt to acquire the outer lock through the afterClosed callback so the
                  // browser lock must be released beforehand to maintain lock-ordering (i.e. outer before browser)
                  setupTab.closeTab()
                }
              }
            val tab = createTabWithoutLocking(browserIndex)
            (tab, setup)
          }
        }

        // Do per-browser setup with browser lock only
        setup.foreach(_.apply())

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

      // Locks: outer, browser, outer
      override def close(quit: Boolean = true): Unit =
        if (quit)
          foreachBrowser(true)(b => b.mutex(b.driver.quit()))
        else
          foreachBrowser(true) { b =>
            b.tabs.foreach(_.closeTab())
            b.closeRoot()
          }

      // Locks: outer, browser, outer
      override def closeManagedTabs(): Unit =
        foreachBrowser(false)(_.tabs.foreach(_.closeTab()))

      // Locks: outer, browser
      override def closeRoot(): Unit =
        foreachBrowser(true)(_.closeRoot())

      // Locks: outer. `f` is executed without any locks held.
      private def foreachBrowser(clear: Boolean)(f: Browser => Unit): Unit = {
        val bs = outerMutex {
          val copy = instances
          if (clear)
            instances = Vector.empty
          copy
        }
        par(bs)(f)
      }

      private def par[A](as: Vector[A])(f: A => Unit): Unit = {
        as.length match {
          case 0 => ()
          case 1 => f(as.head)
          case n =>
            val threadPool = Executors.newFixedThreadPool(n)
            implicit val ec = ExecutionContext.fromExecutorService(threadPool)
            val fs = Future.traverse(as)(b => Future(f(b)))
            Await.result(fs, Duration.Inf)
            threadPool.shutdown()
            ec.shutdown()
        }
      }
    }
}
