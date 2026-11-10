package teststate.selenium

import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}

object TestUtil extends japgolly.microlibs.testutil.TestUtil {

  lazy val CI: Boolean =
    Option(System.getProperty("CI")).exists(_.nonEmpty)

  val google = "https://www.google.com/"

  def newChrome() = {
    val options = new ChromeOptions()
    options.setHeadless(true)
    new ChromeDriver(options)
  }

  def newFirefox() = {
    val options = new FirefoxOptions()
    options.setHeadless(true)
    new FirefoxDriver(options)
  }

}
