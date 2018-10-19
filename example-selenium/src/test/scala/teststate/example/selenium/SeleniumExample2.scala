package teststate.example.selenium

import java.time.Instant
import java.util.concurrent.Executors
import org.openqa.selenium.{Keys, WebDriver}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import teststate.example.selenium.MyTestState._
import teststate.selenium.{GrowthStrategy, Tab}
import utest._

object SeleniumExample2 extends TestSuite {

  case class Ref(name: String, tab: Tab[WebDriver]) {
    def observe(): Obs = {
      tab.use(webDriver =>                    // Ensure we're on our allocated tab
        FastDomZipperSelenium.html(webDriver) // Create a DomZipper starting at the <html> tag
          .ensureConsistency($ =>             // Ensure page doesn't change mid-observation
            new Obs($, name)))                // Observe the page
    }
  }

  val resultRegex = "^About ([0-9,]+) .*".r

  private val debugOn = false
  private val debugLock = new AnyRef

  private def debug(msg: => String): Unit =
    if (debugOn)
      debugLock.synchronized {
        System.out.println("%s [%-6d] %s".format(Instant.now().toString, Thread.currentThread().getId, msg))
        System.out.flush()
      }

  class Obs($: FastDomZipperSelenium, name: String) {

    debug(s"Observing $name...")

    private val searchField =
      $.collect1n("[name=q]").head.dom()

    def typeIntoSearch(keys: String): Unit =
      searchField.sendKeys(keys)

    val resultStats: Option[String] =
      $.collect01("#resultStats").map(_.innerText)

    val resultCount: Option[Long] =
      resultStats.map {
        case resultRegex(numStr) => numStr.replace(",", "").toLong
      }

    debug(s"Observed $name. Result count = ${resultCount.fold("N/A")(_.toString)}")
  }

  val observer: Observer[Ref, Obs, String] = Observer(_.observe())

  val * = Dsl[Ref, Obs, Unit].withSeleniumTab(_.tab)

  def searchFor(term: String) =
    *.action(s"Search for '$term'")(_.obs.typeIntoSearch(term + Keys.ENTER))

  val resultCount = *.focus("Search result count").option(_.obs.resultCount)

  def simpleTest(searchTerm: String, minimumResults: Long): *.TestWithInitialState =
    Plan.action(
      searchFor(searchTerm) +> resultCount.assert.exists(s"≥ $minimumResults", _ >= minimumResults)
    )
      .test(observer)
      .stateless
      .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200 millis, 20 seconds))

  def testInParallel() = {
    implicit val ec  = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    val mb           = MultiBrowser(newChrome(), GrowthStrategy.maxBrowsers(2))
    val url          = "http://www.google.com"
    val searchTerms  = List("one", "two", "three", "four", "five", "six", "seven")
    val tests        = searchTerms.map(t => simpleTest(t, 1000000).withLazyRef(Ref(t, mb.openTabTo(url))))
    val testsAsync   = tests.map(t => t.trans(ExecutionModel.toFuture))
    val asyncResults = Future.traverse(testsAsync)(_.run())
    val results      = Await.result(asyncResults, 1 minute)
    mb.close()
    results.foreach(_.assert(true))
  }

  override def tests = CI match {

    case None => Tests {
      testInParallel()
    }

    case Some(_) => Tests {}
  }
}
