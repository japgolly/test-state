package teststate.domzipper.sizzle

import teststate.domzipper.DomZipper.CssSelEngine

trait Exports extends teststate.domzipper.Exports {

  implicit val cssSelEngine: CssSelEngine =
    CssSelEngine(Sizzle(_, _))
}

object Exports extends Exports
