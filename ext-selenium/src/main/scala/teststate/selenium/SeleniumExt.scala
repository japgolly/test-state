package teststate.selenium

import org.openqa.selenium.WebElement


object SeleniumExt {

  class WebElementExt(private val e: WebElement) extends AnyVal {
    def clearAndSendKeys(keys: String): Unit = {
      e.clear()
      e.sendKeys(keys)
    }
  }

}

trait SeleniumExt {
  import SeleniumExt._
  implicit def WebElementExt(e: WebElement): WebElementExt = new WebElementExt(e)
}