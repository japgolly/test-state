package teststate.domzipper.selenium

import teststate.domzipper._
import DomZipperSeleniumF.Dom
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports with teststate.selenium.util.SeleniumExt {

  final type DomZipperSeleniumF[F[_], A] = teststate.domzipper.selenium.DomZipperSeleniumF[F, A]

  final type DomZipperSelenium           = DomZipperSeleniumF[Id, Dom]
  final val  DomZipperSelenium           = new DomZipperSeleniumF.Constructors[Id]()(Throw)

  final type DomCollectionF[F[_], C[_]]  = DomZipperSeleniumF.DomCollection[F, C]
  final type DomCollection [C[_]]        = DomZipperSeleniumF.DomCollection[Id, C]

}

object Exports extends Exports
