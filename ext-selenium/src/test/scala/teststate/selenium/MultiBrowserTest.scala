package teststate.selenium

import org.openqa.selenium.{By, Keys, WebDriver}
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

// TODO This should be a proper test-state test with retries on
object MultiBrowserTest {

  def main(args: Array[String]): Unit = {

    def newChrome() = {
      val options = new ChromeOptions()
      options.setHeadless(false)
      new ChromeDriver(options)
    }
    def newFirefox() = {
      val options = new FirefoxOptions()
      options.setHeadless(false)
      new FirefoxDriver(options)
    }

    val mb = MultiBrowser(newChrome(), GrowthStrategy.maxTabs(2))

    def newTab() = Future(mb.openTab())

    def useTab(t: Tab[WebDriver], search: String) = Future {
      t.use { driver =>
        driver.get("http://www.google.com")
        driver.findElement(By.name("q")).sendKeys(search)
        driver.findElement(By.name("q")).sendKeys(Keys.ENTER)
      }
    }

    def useTab2(t: Tab[WebDriver], search: String) = Future {
      t.use { driver =>
        val realSearch = driver.findElement(By.id("lst-ib")).getAttribute("value")
        val stats = driver.findElement(By.id("resultStats")).getText()
        System.out.println(s"[$search] '$realSearch' - $stats")
      }
    }

    val searches = List("one", "two", "three", "four")

    val rnd = new Random()
    val f =
    for {
      tabAndSearches <- Future.traverse(rnd shuffle searches)(s => newTab().map((_, s)))
      _ <- Future.traverse(rnd shuffle tabAndSearches)(x => useTab(x._1, x._2))
      _ <- Future(Thread.sleep(3000))
      _ <- Future.traverse(rnd shuffle tabAndSearches)(x => useTab2(x._1, x._2))
    } yield ()

    Await.result(f, 60 seconds)

    mb.closeAllBrowsers(quit = false)
    ()
  }

}
