package teststate.domzipper.sizzle

import teststate.domzipper.DomZipper.CssSelEngine

trait Exports
  extends teststate.domzipper.Exports {

  type Sizzle = teststate.domzipper.sizzle.Sizzle.type
  val Sizzle = teststate.domzipper.sizzle.Sizzle

  implicit val cssSelEngine: CssSelEngine =
    CssSelEngine(Sizzle(_, _))
}

object Exports extends Exports
