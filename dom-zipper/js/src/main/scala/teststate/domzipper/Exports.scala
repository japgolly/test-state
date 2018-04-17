package teststate.domzipper

import org.scalajs.dom.html
import teststate.domzipper.DomZipperJS.{DomZipper => DZ}
import DomZipperJS.{Base, NextBase, Root, Constructors}
import ErrorHandler.{Id, Throw}

trait Exports extends SharedExports {

  type DomZipperAt[+D <: Base] = DZ[D, NextBase, Id]
  type DomZipper               = DomZipperAt[Base]
  type DomZipperRoot           = DomZipperAt[Root]
  val  DomZipper               = new Constructors[NextBase, Id]()(Throw)

  type HtmlDomZipperAt[+D <: Base] = DZ[D, html.Element, Id]
  type HtmlDomZipper               = HtmlDomZipperAt[html.Element]
  type HtmlDomZipperRoot           = HtmlDomZipperAt[Root]
  val  HtmlDomZipper               = new Constructors[html.Element, Id]()(Throw)

  implicit def domZipperNextCovariance[D <: Base, A <: NextBase, B >: A <: NextBase, Out[_]](z: DZ[D, A, Out]): DZ[D, B, Out] =
    z.widenChildren
}

object Exports extends Exports
