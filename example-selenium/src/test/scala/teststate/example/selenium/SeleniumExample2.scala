package teststate.example.selenium

import utest._
import MyTestState._
import java.time.Instant
import java.util.concurrent.{Executors, TimeUnit}
import org.openqa.selenium.{Keys, WebDriver, WebElement}
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import teststate.selenium.{GrowthStrategy, Tab}

object SeleniumExample2 extends TestSuite {

  def openBrowser() = {
    val options = new ChromeOptions()
    options.setHeadless(true)
    val driver = new ChromeDriver(options)
    driver.manage().timeouts().implicitlyWait(1, TimeUnit.MILLISECONDS)
    driver
  }

  case class Ref(name: String, tab: Tab[WebDriver]) {
    def observe(): Obs = {
      tab.use(new Obs(_, name))
    }
  }

  val resultRegex = "^About ([0-9,]+) .*".r

  private var debugOn = false
  private val debugLock = new AnyRef

  private def debug(msg: => String): Unit =
    if (debugOn)
      debugLock.synchronized {
        System.out.println("%s [%-6d] %s".format(Instant.now().toString, Thread.currentThread().getId, msg))
        System.out.flush()
      }

  class Obs(driver: WebDriver, name: String) {
    private val $ = DomZipperSelenium.html(driver)

    debug(s"Observing $name...")

    val searchField: WebElement =
      $.collect0n("[name=q]").doms.head

    val resultStats: Option[String] =
      $.collect01("#resultStats").mapZippers(_.innerText)

    val resultCount: Option[Long] =
      resultStats.map {
        case resultRegex(numStr) => numStr.replace(",", "").toLong
      }

    debug(s"Observed $name. Result count = ${resultCount.fold("N/A")(_.toString)}")
  }

  val observer: Observer[Ref, Obs, String] = Observer(_.observe())

  val * = Dsl[Ref, Obs, Unit].withSeleniumTab(_.tab)

  def searchFor(term: String) =
    *.action(s"Search for '$term'")(_.obs.searchField.sendKeys(term + Keys.ENTER))

  val resultCount = *.focus("Search result count").option(_.obs.resultCount)

  def simpleTest(searchTerm: String, minimumResults: Long): *.TestWithInitialState =
    Plan.action(
      searchFor(searchTerm) +> resultCount.assert.exists(s"≥ $minimumResults", _ >= minimumResults)
    )
      .test(observer)
      .stateless
      .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200 millis, 2 seconds))

  def testInParallel() = {
    implicit val ec  = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    val mb           = MultiBrowser(openBrowser(), GrowthStrategy.maxBrowsers(2))
    val url          = "http://www.google.com"
    val searchTerms  = List("one", "two", "three", "four", "five", "six", "seven")
    val tests        = searchTerms.map(t => simpleTest(t, 1000000).withLazyRef(Ref(t, mb.openTabTo(url))))
    val testsAsync   = tests.map(t => t.trans(ExecutionModel.toFuture))
    val asyncResults = Future.traverse(testsAsync)(_.run())
    val results      = Await.result(asyncResults, 1 minute)
    mb.closeAllBrowsers()
    results.foreach(_.assert(true))
  }

  override def tests = CI match {
    case None => TestSuite {
      testInParallel()
    }

    case Some(_) => TestSuite {}
  }
}
