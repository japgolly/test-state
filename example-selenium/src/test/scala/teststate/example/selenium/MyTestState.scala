package teststate.example.selenium

import java.util.concurrent.TimeUnit
import org.openqa.selenium.PageLoadStrategy
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}

/**
  * Build a TestState configuration for this project.
  *
  * See `/doc/USAGE.md` for more info.
  */
object MyTestState
  extends teststate.Exports
    with teststate.selenium.Exports
    with teststate.domzipper.selenium.Exports {

  lazy val CI: Boolean =
    Option(System.getProperty("CI")).exists(_.nonEmpty)

  def newChrome() = {
    val options = new ChromeOptions()
    options.setHeadless(true)
    options.setPageLoadStrategy(PageLoadStrategy.NONE)

    val driver = new ChromeDriver(options)

    // Tell Selenium not to wait (block). TestState will manage retries itself.
    // When parallel testing, the allows TestState to move around various tabs.
    driver.manage().timeouts().implicitlyWait(1, TimeUnit.MILLISECONDS)

    driver
  }

}
