package teststate

/*
import utest._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  val * = Dsl.future[Example, Obs, Int, String]

  val inc = *.action("Count increases by 1")(i => Future(i.ref.inc())).updateState(_ + 1)
    .addCheck(*.focus("Number of changes").value(_.obs.changes).assertChange("+1", _ + 1))

  val count = *.bifocus("Count", _.count, identity)

  val objMatchesState    = count.assertEqual
  val countNeverNegative = count.obs.test(_ + " is never negative.", _ >= 0)

  val invariants = objMatchesState & countNeverNegative

  val actions = inc.times(5) >> inc.times(3)

  val test = *.test(actions, invariants)(eg => Obs(eg.count(), eg.changes()))

  // ==============================================================================================================

  override def tests = TestSuite {

    println()
    def go(init: Int) = {
      val eg = new Example(init)
      test.run(init, eg).map { h =>
        println(h)
        println(eg.count())
        println()
        println(formatHistory(h, Options.colored.alwaysShowChildren))
        println()
      }
    }
    "2" - go(2)
    "4" - go(4)
  }
}
*/