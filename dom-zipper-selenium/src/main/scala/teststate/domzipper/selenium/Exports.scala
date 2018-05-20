package teststate.domzipper.selenium

import org.openqa.selenium.WebElement
import teststate.domzipper._
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports with teststate.selenium.util.SeleniumExt {

  final type DomZipperSelenium = DomZipperSeleniumModule.DomZipper[WebElement, WebElement, Id]
  final val  DomZipperSelenium = new DomZipperSeleniumModule.Constructors[Id]()(Throw)

}

object Exports extends Exports
