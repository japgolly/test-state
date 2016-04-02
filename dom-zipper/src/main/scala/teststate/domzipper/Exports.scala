package teststate.domzipper

import org.scalajs.dom.html
import teststate.domzipper.{DomZipper => DZ}
import DomZipper.{Base, NextBase}
import ErrorHandler.{Id, Throw}

trait Exports {

  implicit def toMofNOps(i: Int): MofN.IntExt =
    new MofN.IntExt(i)

  type DomZipper[+D <: Base] = DZ[D, NextBase, Id]
  val DomZipper = new DZ.Constructors[NextBase, Id]()(Throw)

  type HtmlDomZipper[+D <: Base] = DZ[D, html.Element, Id]
  val HtmlDomZipper = new DZ.Constructors[html.Element, Id]()(Throw)
}

object Exports extends Exports
