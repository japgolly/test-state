package teststate.selenium

import org.openqa.selenium.WebDriver

trait MultiTab[+D <: WebDriver] {
  def openTab(): Tab[D]

  def openTabTo(url: String): Tab[D] =
    openTab().withBeforeFirstUse(_.get(url))
}

object MultiTab {

  def apply[D <: WebDriver](driver: D)
                           (implicit tabSupport: TabSupport[D]): MultiTab[D] =
    apply(driver, Mutex())(tabSupport)(tabSupport.active()(driver))

  def apply[D <: WebDriver](driver    : D,
                            mutex     : Mutex)
                           (tabSupport: TabSupport[D])
                           (rootTab   : tabSupport.TabHandle): MultiTab[D] =
    new MultiTab[D] {
      override def openTab(): Tab[D] =
        Tab(driver, mutex, tabSupport)(rootTab)
    }
}
