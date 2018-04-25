package teststate.selenium

import org.openqa.selenium.WebDriver

/** Access to a specific tab in a browser. */
trait Tab[+D <: WebDriver] {

  /** Focus the tab and perform action within it */
  def use[A](f: D => A): A

  def closeTab(): Unit
}

final class TabAlreadyClosed() extends RuntimeException("You cannot use a tab after you've closed it.")
