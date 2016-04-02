package testate.domzipper.sizzle

import testate.domzipper.DomZipper.CssSelEngine

trait Exports
  extends testate.domzipper.Exports {

  type Sizzle = testate.domzipper.sizzle.Sizzle.type
  val Sizzle = testate.domzipper.sizzle.Sizzle

  implicit val cssSelEngine: CssSelEngine =
    CssSelEngine(Sizzle(_, _))
}

object Exports extends Exports
