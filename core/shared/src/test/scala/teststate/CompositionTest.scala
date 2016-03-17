package teststate

import nyaya.prop._
import nyaya.test.PropTest._
import teststate.typeclass.PolyComposable
import teststate.typeclass.PolyComposable.CanSeq
import scalaz.std.string.stringInstance
import utest._
import Exports._
import RandomData.*

object CompositionTest extends TestSuite {


  def associativity[A, B, C, AB, BC](implicit
                                     x1: PolyComposable[A, B, AB],
                                     x2: PolyComposable[B, C, BC],
                                     x3: PolyComposable[AB, C, *.Action],
                                     x4: PolyComposable[A, BC, *.Action]) = {

    implicit def anyCanSeq[X]: CanSeq[X] = CanSeq

    def results(a: *.Action): String =
      "\n" + Test(a).observe(_ => ()).run((), ()).format(History.Options.uncolored)

    Prop.equal[(A, B, C)]("associativity")(
      { case (a, b, c) => results((a >> b) >> c) },
      { case (a, b, c) => results(a >> (b >> c)) })
  }

  override def tests = TestSuite {
    'coproduct {
      import CoproductExample._
      import Top._
      val top = new Top(7, "e")
      val h = test.run(State(Type.Num, 7, "e"), top)
      h.assert(History.Options.colored)
      // println(h.format(History.Options.colored.alwaysShowChildren))
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
