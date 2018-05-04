package teststate.selenium

import org.openqa.selenium._

object SeleniumExt {

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

    private def _scrollTo(d: WebDriver, x: Option[Int], y: Option[Int]): Unit =
      d match {
        case j: JavascriptExecutor =>
          val xx = x.fold("window.scrollX")(_.toString)
          val yy = y.fold("window.scrollY")(_.toString)
          j.executeScript(s"window.scrollTo($xx,$yy)")
          ()
        case _ =>
          throw new JavaScriptNotSupported("Scrolling without JS not supported.")
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

  final class JavaScriptNotSupported(msg: String) extends RuntimeException
}

trait SeleniumExt {
  import SeleniumExt._
  implicit def WebElementExt(e: WebElement): WebElementExt = new WebElementExt(e)
  implicit def PointExt     (p: Point)     : PointExt      = new PointExt(p)
}