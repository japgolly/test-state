package teststate.domzipper.jsoup

import teststate.domzipper._
import DomZipperJsoupF.Dom
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports {

  final type DomZipperJsoupF[F[_], A]   = teststate.domzipper.jsoup.DomZipperJsoupF[F, A]

  final type DomZipperJsoup             = DomZipperJsoupF[Id, Dom]
  final val  DomZipperJsoup             = new DomZipperJsoupF.Constructors[Id]()(Throw)

}

object Exports extends Exports
