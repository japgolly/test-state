package teststate.domzipper

import org.scalajs.dom.html
import teststate.domzipper.DomZipperJS.{DomZipper => DZ}
import DomZipperJS.{Base, NextBase, Root, Constructors}
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports {

  final type DomZipperAt[+D <: Base] = DZ[D, NextBase, Id]
  final type DomZipper               = DomZipperAt[Base]
  final type DomZipperRoot           = DomZipperAt[Root]
  final val  DomZipper               = new Constructors[NextBase, Id]()(Throw)

  final type HtmlDomZipperAt[+D <: Base] = DZ[D, html.Element, Id]
  final type HtmlDomZipper               = HtmlDomZipperAt[html.Element]
  final type HtmlDomZipperRoot           = HtmlDomZipperAt[Root]
  final val  HtmlDomZipper               = new Constructors[html.Element, Id]()(Throw)

  final implicit def domZipperNextCovariance[D <: Base, A <: NextBase, B >: A <: NextBase, Out[_]](z: DZ[D, A, Out]): DZ[D, B, Out] =
    z.widenChildren
}

object Exports extends Exports
