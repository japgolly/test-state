package teststate.domzipper.selenium

import teststate.domzipper._
import DomZipperSeleniumF.Dom
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports with teststate.selenium.util.SeleniumExt {

  final type DomZipperSeleniumF[F[_], A] = teststate.domzipper.selenium.DomZipperSeleniumF[F, A]

  final type DomZipperSelenium           = DomZipperSeleniumF[Id, Dom]
  final val  DomZipperSelenium           = new DomZipperSeleniumF.Constructors[Id]()(Throw)

  final type FastDomZipperSeleniumF[F[_]] = FastDomZipperSeleniumF.Type[F]
  final type FastDomZipperSelenium        = FastDomZipperSeleniumF[Id]
  final val  FastDomZipperSelenium        = new FastDomZipperSeleniumF.Constructors[Id]()(Throw)

}

object Exports extends Exports
