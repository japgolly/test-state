package teststate.example.selenium

import utest._
import MyTestState._
import java.util.concurrent.TimeUnit
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

  // TODO Need focus().option()
  val responseText = *.focus("Response text").collection(_.obs.responseText.toList)

  override def tests = CI match {
    case None => TestSuite {

      val plan = Plan.action(clickGet +> responseText.assert.exists("Has Response", _.contains("Response")))
      plan
        .test(observer)
        .stateless
        .withLazyRef(openBrowser())
        .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200, 12000))
        .run()
        .assert()
    }

    case Some(_) => TestSuite {}
  }
}
