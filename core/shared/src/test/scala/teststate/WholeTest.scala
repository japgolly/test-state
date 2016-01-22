package teststate

import utest._

object WholeTest extends TestSuite {

  class Example(start: Int) {
    private var c = start
    def inc() = if (c < 10) c += 1
    def dec() = if (c > 0) c -= 1
    def count() = c
  }

  // ==============================================================================================================

  case class Obs(count: Int)

  val * = Dsl[Example, Obs, Int, String]

  val inc = *.action("Count increases by 1").act(_.ref.inc()).updateState(_ + 1)

  val objMatchesState =
    Check.Point.Single[Obs, Int, String](
      _.fold("Count = ?")(t => s"Count = " + t._2),
      (o, s) => if (o.count == s) None else Some(s"Expected $s, got ${o.count}."))

  val countNeverNegative =
    Check.Point.Single[Obs, Any, String](
      _ => "Count is never negative",
      (o, _) => if (o.count >= 0) None else Some(s"Count = ${o.count}."))

  val invariants =
    objMatchesState & countNeverNegative

  val test = Test0(inc.times(5) >> inc.times(3), invariants).observe(eg => Obs(eg.count()))

  // ==============================================================================================================

  override def tests = TestSuite {

    println()
    def go(init: Int) = {
      val eg = new Example(init)
      val h = test.run(init, eg)
      println(h)
      println(eg.count())
      println()
      println(formatHistory(h, Options.colored.alwaysShowChildren))
      println()
    }
    go(2)
    go(4)
  }
}
