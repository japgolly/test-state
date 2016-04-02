package testate.external

import testate.Exports._

case class X1(i: Int)
case class X2(i: Int)
case class X3(i: Int)

object X3 {
  implicit def displayX3: Display[X3] = Display("X3=" + _.i)
}
