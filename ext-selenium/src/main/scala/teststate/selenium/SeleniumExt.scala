package teststate.selenium

import org.openqa.selenium._

object SeleniumExt {

  class WebDriverExt(private val self: WebDriver) extends AnyVal {
    def executeJsOrThrow(cmd: String): Unit =
      self match {
        case j: JavascriptExecutor =>
          j.executeScript(cmd)
          ()
        case _ =>
          throw new JavaScriptNotSupported(cmd)
      }
  }

  class WebElementExt(private val self: WebElement) extends AnyVal {
    def clearAndSendKeys(keys: String): Unit = {
      self.clear()
      self.sendKeys(keys)
    }

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
      new WebDriverExt(d).executeJsOrThrow(s"window.scrollTo($xx,$yy)")
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

  final class JavaScriptNotSupported(cmd: String) extends RuntimeException("Unable to execute: " + cmd)
}

trait SeleniumExt {
  import SeleniumExt._
  implicit def WebDriverExt (d: WebDriver) : WebDriverExt  = new WebDriverExt(d)
  implicit def WebElementExt(e: WebElement): WebElementExt = new WebElementExt(e)
  implicit def PointExt     (p: Point)     : PointExt      = new PointExt(p)
}