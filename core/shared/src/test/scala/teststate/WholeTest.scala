package teststate

import utest._

object WholeTest extends TestSuite {

  class Example(start: Int) {
    private var c = start
    def inc() = if (c < 10) c += 1
    def dec() = if (c > 0) c -= 1
    def count() = c
  }

  case class Obs(count: Int)

  val inc =
    Action.Single[Example, Obs, Int, String](_ => "Increment", i =>
      Some(() => {
        i.ref.inc()
        Right(_ => i.state + 1)
      }),
      Check[Obs, Int, String, Int](_ => "Count increases by 1",
        (o, _) => Right(o.count),
        (o, _, n) => if (o.count == n + 1) None else Some(s"Expected ${n + 1}, not ${o.count}.")
      )
    )

  inc.when(_.ref.count() == 3)

  val countNeverNegative =
    Invariant[Obs, Any, String](_ => "Count is never negative",
      (o, _) => if (o.count >= 0) None else Some(s"Count = ${o.count}."))

  val test = Test0(inc.times(5) >> inc.times(2), countNeverNegative)
    .observe((eg: Example) => Obs(eg.count()))

  override def tests = TestSuite {

    println()
    val eg = new Example(3)
    val h = test.run(3, eg)
    println(h)
    println(eg.count())
    println()
    println(formatHistory(h, Options.colored))
    println()
  }
}
