package teststate.selenium

import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import teststate.typeclass.ExecutionModel

trait MultiTab[+D <: WebDriver] {
  def openTab(): Tab[D]

  def openTabTo(url: String): Tab[D] =
    openTab().tap(_.get(url))
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
      import tabSupport.TabHandle

      override def openTab(): Tab[D] =
        new Tab[D] {
          private var tab: Option[TabHandle] = None
          private var closed = false

          private def prepareWithoutLocking(): D = {
            implicit def d = driver

            if (closed)
              throw new TabAlreadyClosed()

            val t: TabHandle =
              tab.getOrElse {
                val newTab = tabSupport.open(rootTab)
                tab = Some(newTab)
                newTab
              }
            tabSupport.activate(t)
            d
          }

          override def use[A](f: D => A): A =
            mutex(f(prepareWithoutLocking()))

          override def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                                    (implicit EM: ExecutionModel[M]): M[A] = {
            def driver = EM.point(prepareWithoutLocking())
            mutex.monadic(EM.flatMap(driver)(f), lockWait, lockRetry)
          }

          override def closeTab(): Boolean =
            mutex {
              val affect = !closed
              if (affect) {
                implicit def d = driver
                for (t <- tab) {
                  tabSupport.activate(t)
                  tabSupport.closeActive()
                }
                closed = true
                tab = None
              }
              affect
            }
        }
    }

}
