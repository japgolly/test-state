package teststate.domzipper.jsoup

import teststate.domzipper._
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports {

  final type DomZipperJsoupF[F[_]]      = teststate.domzipper.jsoup.DomZipperJsoupF[F]

  final type DomZipperJsoup             = DomZipperJsoupF[Id]
  final val  DomZipperJsoup             = new DomZipperJsoupF.Constructors[Id]()(Throw)

  final type DomCollectionF[F[_], C[_]] = DomZipperJsoupF.DomCollection[F, C]
  final type DomCollection [C[_]]       = DomZipperJsoupF.DomCollection[Id, C]

}

object Exports extends Exports
