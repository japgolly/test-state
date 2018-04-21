package teststate

import nyaya.prop._
import nyaya.test.PropTest._
import scalaz.std.string.stringInstance
import utest._
import teststate.typeclass.PolyComposable
import teststate.Exports._
import teststate.TestUtil._
import PolyComposable.{Can, SeqOp}
import RandomData.*

object CompositionTest extends TestSuite {

  def associativity[A, B, C, AB, BC](implicit
                                     x1: PolyComposable[SeqOp, A, B, AB],
                                     x2: PolyComposable[SeqOp, B, C, BC],
                                     x3: PolyComposable[SeqOp, AB, C, *.Actions],
                                     x4: PolyComposable[SeqOp, A, BC, *.Actions]) = {

    implicit def anyCanSeq[X]: Can[SeqOp, X] = Can

    def results(a: *.Actions): String =
      "\n" + Plan.action(a).stateless.testU.runU.format(inspectionFormat)

    Prop.equal[(A, B, C)]("associativity")(
      { case (a, b, c) => results((a >> b) >> c) },
      { case (a, b, c) => results(a >> (b >> c)) })
  }

  override def tests = TestSuite {
    'coproduct {
      import CoproductExample._
      import Top._
      val top = new Top(7, "e")
      val r = test.withInitialState(State(Type.Num, 7, "e")).withRef(top).run()
      r.assert()
    }

    import RandomData._
    "a >> a >> a" -
      action.triple.mustSatisfy(associativity) //(defaultPropSettings.setDebug)

    /*
    associativity doesn't hold for the following:
    Come to think of it, I wouldn't want it to. If I deliberately group somethings together I wouldn't want it unpacked.

    "a >> p >> a" -
      Gen.tuple3(action, point, action).mustSatisfy(associativity)(defaultPropSettings.setDebug.setSeed(1).setSingleThreaded)

    "a >> a >> p" -
      Gen.tuple3(action, action, point).mustSatisfy(associativity)(defaultPropSettings.setDebug.setSeed(1).setSingleThreaded)

    "p >> a >> a" -
      Gen.tuple3(point, action, action).mustSatisfy(associativity)(defaultPropSettings.setDebug.setSeed(1).setSingleThreaded)
    */
  }
}
