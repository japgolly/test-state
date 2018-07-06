package teststate.selenium.util

import org.openqa.selenium._
import org.openqa.selenium.interactions.Actions
import scala.collection.JavaConverters._

object SeleniumExt extends SeleniumExt

trait SeleniumExt {
  implicit def testStateExtWebDriver (d: WebDriver) : Internals.WebDriverExt  = new Internals.WebDriverExt(d)
  implicit def testStateExtWebElement(e: WebElement): Internals.WebElementExt = new Internals.WebElementExt(e)
  implicit def testStateExtPoint     (p: Point)     : Internals.PointExt      = new Internals.PointExt(p)

  final type JavaScriptNotSupported = Internals.JavaScriptNotSupported
  final val  JavaScriptNotSupported = Internals.JavaScriptNotSupported
}

object Internals {
  private implicit def WebDriverExt(d: WebDriver): WebDriverExt = new WebDriverExt(d)

  class WebDriverExt(private val self: WebDriver) extends AnyVal {
    def executeJsOrThrow(cmd: String): Unit =
      self match {
        case j: JavascriptExecutor =>
          j.executeScript(cmd)
          ()
        case _ =>
          throw JavaScriptNotSupported(cmd)
      }

    def unsetOnBeforeUnload(): Unit =
      executeJsOrThrow("window.onbeforeunload = undefined")
  }

  private val childrenXpath = By.xpath("./*")

  class WebElementExt(private val self: WebElement) extends AnyVal {
    def classes(): Set[String] = {
      val clsStr = self.getAttribute("class").trim
      if (clsStr.isEmpty)
        Set.empty
      else
        clsStr.split(" +").toSet
    }

    def children(): Vector[WebElement] =
      self.findElements(childrenXpath).asScala.toVector

    def getValue(): Option[String] =
      Option(self.getAttribute("value"))

    def setValueTo(value: String): Unit =
      if (value != self.getAttribute("value"))
        clearAndSendKeys(value)

    def clearAndSendKeys(keys: String): Unit = {
      self.clear()
      self.sendKeys(keys)
    }

    def moveMouseTo(d: WebDriver): Unit =
      new Actions(d).moveToElement(self).build().perform()

    def scrollTo(d: WebDriver): Unit = {
      val p = self.getLocation
      _scrollTo(d, Some(p.x), Some(p.y))
    }

    def scrollToX(d: WebDriver): Unit =
      _scrollTo(d, Some(self.getLocation.x), None)

    def scrollToY(d: WebDriver): Unit =
      _scrollTo(d, None, Some(self.getLocation.y))

    private def _scrollTo(d: WebDriver, x: Option[Int], y: Option[Int]): Unit = {
      val xx = x.fold("window.scrollX")(_.toString)
      val yy = y.fold("window.scrollY")(_.toString)
      d.executeJsOrThrow(s"window.scrollTo($xx,$yy)")
    }

    def scrollToAndClick(d: WebDriver): Unit = {
      scrollTo(d)
      self.click()
    }
  }

  class PointExt(private val p: Point) extends AnyVal {
    def isPositive: Boolean =
      p.x >= 0 && p.y >= 0
  }

  final case class JavaScriptNotSupported(cmd: String) extends RuntimeException("Unable to execute: " + cmd)
}
