package teststate.domzipper.selenium

import teststate.domzipper.ErrorHandler.{Id, Throw}
import teststate.domzipper._
import teststate.domzipper.selenium.DomZipperSeleniumF.Dom

trait Exports extends SharedExports with teststate.selenium.util.SeleniumExt {

  final type DomZipperSeleniumF[F[_], A] = teststate.domzipper.selenium.DomZipperSeleniumF[F, A]

  final type DomZipperSelenium           = DomZipperSeleniumF[Id, Dom]
  final val  DomZipperSelenium           = new DomZipperSeleniumF.Constructors[Id]()(Throw)

  final type FastDomZipperSeleniumF[F[_]] = FastDomZipperSeleniumF.FastDomZipperSeleniumF[F]
  final type FastDomZipperSelenium        = FastDomZipperSeleniumF[Id]
  final val  FastDomZipperSelenium        = new FastDomZipperSeleniumF.Constructors[Id]()(Throw)

}

object Exports extends Exports
