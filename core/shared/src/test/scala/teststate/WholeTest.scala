package teststate

import utest._

object WholeTest extends TestSuite {

  class Example(start: Int) {
    private var _count = start
    private var _changes = 0

    def inc() = if (_count < 10) {_count += 1; _changes += 1}
    def dec() = if (_count >  0) {_count -= 1; _changes += 1}
    def count() = _count
    def changes() = _changes
  }

  // ==============================================================================================================

  case class Obs(count: Int, changes: Int)

  val * = Dsl[Example, Obs, Int, String]

  val inc = *.action("Count increases by 1").act(_.ref.inc()).updateState(_ + 1)
    .addCheck(*.check("Number of changes").obsTo(_.changes).assertChange("+1", _ + 1))

  val objMatchesState =
    Check.Point.Single[Obs, Int, String](
      _.fold("Count = <state>")(t => s"Count = " + t._2),
      (o, s) => if (o.count == s) None else Some(s"Expected $s, got ${o.count}."))

  val countNeverNegative =
    *.check("Count").obsTo(_.count).test(_ + " is never negative.", _ >= 0)
//    Check.Point.Single[Obs, Any, String](
//      _ => "Count is never negative",
//      (o, _) => if (o.count >= 0) None else Some(s"Count = ${o.count}."))

  val invariants =
    objMatchesState & countNeverNegative

  val test = Test0(inc.times(5) >> inc.times(3), invariants).observe(eg => Obs(eg.count(), eg.changes()))

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
