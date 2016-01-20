package teststate

import utest._

object WholeTest extends TestSuite {

  case class Example(count: Int) {
    def inc = copy(count + 1)
    def dec = copy(count - 1)
  }

  override def tests = TestSuite {

//    Runner.run()

  }
}
