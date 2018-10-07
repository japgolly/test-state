package teststate.domzipper.jsoup

import teststate.domzipper._
import DomZipperJsoupF.Dom
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports {

  final type DomZipperJsoupF[F[_], A]   = teststate.domzipper.jsoup.DomZipperJsoupF[F, A]

  final type DomZipperJsoup             = DomZipperJsoupF[Id, Dom]
  final val  DomZipperJsoup             = new DomZipperJsoupF.Constructors[Id]()(Throw)

  final type DomCollectionF[F[_], C[_]] = DomZipperJsoupF.DomCollection[F, C]
  final type DomCollection [C[_]]       = DomZipperJsoupF.DomCollection[Id, C]

}

object Exports extends Exports
