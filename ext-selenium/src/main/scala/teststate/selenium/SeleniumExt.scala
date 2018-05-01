package teststate.selenium

import org.openqa.selenium.{Point, WebElement}

object SeleniumExt {

  class WebElementExt(private val e: WebElement) extends AnyVal {
    def clearAndSendKeys(keys: String): Unit = {
      e.clear()
      e.sendKeys(keys)
    }
  }

  class PointExt(private val p: Point) extends AnyVal {
    def isPositive: Boolean =
      p.x >= 0 && p.y >= 0
  }

}

trait SeleniumExt {
  import SeleniumExt._
  implicit def WebElementExt(e: WebElement): WebElementExt = new WebElementExt(e)
  implicit def PointExt     (p: Point)     : PointExt      = new PointExt(p)
}