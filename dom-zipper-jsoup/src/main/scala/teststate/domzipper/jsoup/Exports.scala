package teststate.domzipper.jsoup

import teststate.domzipper._
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports {

  final type DomZipperJsoup = DomZipperJsoupModule.DomZipper[Element, Element, Id]
  final val  DomZipperJsoup = new DomZipperJsoupModule.Constructors[Id]()(Throw)

}

object Exports extends Exports
