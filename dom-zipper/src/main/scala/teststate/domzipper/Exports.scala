package teststate.domzipper

import org.scalajs.dom.html
import teststate.domzipper.{DomZipper => DZ}
import DomZipper.{Base, NextBase, Root}
import ErrorHandler.{Id, Throw}

trait Exports {

  implicit def toMofNOps(i: Int): MofN.IntExt =
    new MofN.IntExt(i)

  type DomZipperAt[+D <: Base] = DZ[D, NextBase, Id]
  type DomZipper               = DomZipperAt[Base]
  type DomZipperRoot           = DomZipperAt[Root]
  val  DomZipper               = new DZ.Constructors[NextBase, Id]()(Throw)

  type HtmlDomZipperAt[+D <: Base] = DZ[D, html.Element, Id]
  type HtmlDomZipper               = HtmlDomZipperAt[html.Element]
  type HtmlDomZipperRoot           = HtmlDomZipperAt[Root]
  val  HtmlDomZipper               = new DZ.Constructors[html.Element, Id]()(Throw)

  type HtmlScrub = teststate.domzipper.HtmlScrub
  val  HtmlScrub = teststate.domzipper.HtmlScrub

  implicit def domZipperNextCovariance[D <: Base, A <: NextBase, B >: A <: NextBase, Out[_]](z: DZ[D, A, Out]): DZ[D, B, Out] =
    z.widenChildren

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default
}

object Exports extends Exports
