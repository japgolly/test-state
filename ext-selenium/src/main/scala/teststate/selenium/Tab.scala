package teststate.selenium

import org.openqa.selenium.WebDriver

/** Access to a specific tab in a browser. */
trait Tab[+D <: WebDriver] {

  /** Focus the tab and perform action within it */
  def use[A](f: D => A): A

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
