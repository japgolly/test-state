package teststate.example.selenium

import utest._
import MyTestState._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import org.openqa.selenium.{WebDriver, WebElement}
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}

object SeleniumExample extends TestSuite {

  type Ref = WebDriver

  def openBrowser(): WebDriver = {
    val options = new ChromeOptions()
    options.setHeadless(true)
    val driver = new ChromeDriver(options)
    driver.manage().timeouts().implicitlyWait(1, TimeUnit.MILLISECONDS)
    driver.get("https://japgolly.github.io/scalajs-react/#examples/ajax")
    driver
  }

  class Obs(ref: Ref) {
    private val $ = DomZipperSelenium.html(ref)

    val button: WebElement =
      $("button", 1 of 3).webElement

    val responseText: Option[String] =
      $.collect01("table").mapZippers(_("td", 2 of 2).innerText)

    // println(s"responseText = ${responseText.toString.replace("\n", " ").take(60)}")
  }

  val observer: Observer[Ref, Obs, String] = Observer(new Obs(_))

  val * = Dsl[Ref, Obs, Unit]

  val clickGet = *.action("Click GET")(_.obs.button.click())

  val responseText = *.focus("Response text").option(_.obs.responseText)

  override def tests = CI match {
    case None => TestSuite {

      val driver = openBrowser()
      val plan = Plan.action(clickGet +> responseText.assert.exists("Response", _ contains "Response"))
      val report = plan
        .test(observer)
        .stateless
        .withRef(driver)
        .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200 millis, 12 seconds))
        .run()
      driver.quit()
      report.assert()
    }

    case Some(_) => TestSuite {}
  }
}
