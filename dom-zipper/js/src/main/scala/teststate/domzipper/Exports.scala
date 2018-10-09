package teststate.domzipper

import DomZipperJsF.Dom
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports with DomCollectionJsExt.Exports with JsDomExt {

  final type DomZipperJsF[F[_], A] = teststate.domzipper.DomZipperJsF[F, A]

  final type DomZipperJs = DomZipperJsF[Id, Dom]
  final val  DomZipperJs = new teststate.domzipper.DomZipperJsF.Constructors[Id]()(Throw)

  final type CssSelEngine = DomZipper.CssSelEngine[Dom, Dom]
  final def CssSelEngine(run: (String, Dom) => Vector[Dom]): CssSelEngine = DomZipper.CssSelEngine(run)
}

object Exports extends Exports
