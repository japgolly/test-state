package teststate.example.selenium

import org.openqa.selenium.{WebDriver, WebElement}
import scala.concurrent.duration._
import teststate.example.selenium.MyTestState._
import utest._

object SeleniumExample extends TestSuite {

  type Ref = WebDriver

  def openBrowser(): WebDriver = {
    val driver = newChrome()
    driver.get("https://japgolly.github.io/scalajs-react/#examples/ajax")
    driver
  }

  class Obs(ref: Ref) {
    private val $ = DomZipperSelenium.html(ref)

    val button: WebElement =
      $("button", 1 of 3).dom

    val responseText: Option[String] =
      $.collect01("table").map(_("td", 2 of 2).innerText)

    // println(s"responseText = ${responseText.toString.replace("\n", " ").take(60)}")
  }

  val observer: Observer[Ref, Obs, String] = Observer(new Obs(_))

  val * = Dsl[Ref, Obs, Unit]

  val clickGet = *.action("Click GET")(_.obs.button.click())

  val responseText = *.focus("Response text").option(_.obs.responseText)

  override def tests = TestSuite {

    val driver = openBrowser()
    val plan = Plan.action(clickGet +> responseText.assert.exists("Response", _ contains "Response"))
    val report = plan
      .test(observer)
      .stateless
      .withRef(driver)
      .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200 millis, 60 seconds))
      .run()
    driver.quit()
    report.assert()

  }
}
