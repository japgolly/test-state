package teststate.example.selenium

import org.openqa.selenium.WebDriver
import scala.concurrent.duration._
import teststate.example.selenium.TestState._
import utest._

object SeleniumExample extends TestSuite {

  type Ref = WebDriver

  def openBrowser(): WebDriver = {
    val driver = newChrome()
    driver.get("https://japgolly.github.io/scalajs-react/#examples/ajax-1")
    driver
  }

  class Obs($: FastDomZipperSelenium) {

    val clickButton: () => Unit =
      $("button", 1 of 3).prepare(_.dom().click())

    val responseText: Option[String] =
      $.collect01("table").map(_("td", 2 of 2).innerText)

    // println(s"responseText = ${responseText.toString.replace("\n", " ").take(60)}")
  }

  val dsl = Dsl[Ref, Obs, Unit]

  val observer = dsl.observer(ref => new Obs(FastDomZipperSelenium.html(ref)))

  val clickGet = dsl.action("Click GET")(_.obs.clickButton())

  val responseText = dsl.focus("Response text").option(_.obs.responseText)

  def runTest() = {
    val driver = openBrowser()
    val plan = Plan.action(clickGet +> responseText.assert.exists("Response", _ contains "Response"))
    val report = plan
      .withObserver(observer)
      .withoutState
      .withRef(driver)
      .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200 millis, 60 seconds))
      .run()
    driver.quit()
    report.assert()
  }

  override def tests = Tests {
    runTest()
  }
}
