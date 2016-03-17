package teststate

import utest._
import teststate.Exports._

object CompositionTest extends TestSuite {
  override def tests = TestSuite {
    'coproduct {
      import CoproductExample._
      import Top._
      val top = new Top(7, "e")
      val h = test.run(State(Type.Num, 7, "e"), top)
      h.assert(History.Options.colored)
      println(h.format(History.Options.colored.alwaysShowChildren))
    }
  }
}
