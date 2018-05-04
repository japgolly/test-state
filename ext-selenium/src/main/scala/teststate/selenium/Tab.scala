package teststate.selenium

import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import teststate.typeclass.ExecutionModel

/** Access to a specific tab in a browser.
  *
  * Ensure that you call `.withSeleniumTab` on your TestState DSL when using this.
  */
trait Tab[+D <: WebDriver] {

  /** Focus the tab and perform action within it */
  def use[A](f: D => A): A

  /** Monadic version of [[use()]].
    *
    * @param lockWait Maximum time to wait attempting to acquire the tab lock.
    * @param lockRetry If the lock cannot be acquired, how long to wait before trying again.
    */
  def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                   (implicit EM: ExecutionModel[M]): M[A]

  final def tap(f: D => Any): this.type = {
    use(f)
    this
  }

  /** Close the tab.
    *
    * @return whether this call was effective.
    *         Specifically, true if this call closed the tab, false if the tab was already closed.
    */
  def closeTab(): Boolean

  def withOnClose(callback: => Any): Tab[D] = {
    val underlying = this
    new Tab[D] {

      override def use[A](f: D => A): A =
        underlying.use(f)

      override def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                                (implicit EM: ExecutionModel[M]): M[A] =
        underlying.useM(f, lockWait, lockRetry)(EM)

      override def closeTab(): Boolean = {
        val wasEffective = underlying.closeTab()
        if (wasEffective)
          callback
        wasEffective
      }
    }
  }
}

final class TabAlreadyClosed() extends RuntimeException("You cannot use a tab after you've closed it.")
