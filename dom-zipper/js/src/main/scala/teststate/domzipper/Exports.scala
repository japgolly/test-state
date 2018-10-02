package teststate.domzipper

import DomZipperJsF.Dom
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports with DomCollectionJsExt.Exports with JsDomExt {

  final type DomZipperJsF[F[_]] = teststate.domzipper.DomZipperJsF[F]

  final type DomZipperJs = DomZipperJsF[Id]
  final val  DomZipperJs = new teststate.domzipper.DomZipperJsF.Constructors[Id]()(Throw)

  final type DomCollectionF[F[_], C[_]] = DomZipperJsF.DomCollection[F, C]
  final type DomCollection [C[_]]       = DomZipperJsF.DomCollection[Id, C]

  final type CssSelEngine = DomZipper.CssSelEngine[Dom, Dom]
  final def CssSelEngine(run: (String, Dom) => Vector[Dom]): CssSelEngine = DomZipper.CssSelEngine(run)
}

object Exports extends Exports
