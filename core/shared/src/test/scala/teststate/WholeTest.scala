package teststate

import utest._

object WholeTest extends TestSuite {

  class Example(start: Int) {
    private var c = start
    def inc() = c += 1
    def dec() = c -= 1
    def count() = c
  }

  case class Obs(count: Int)

  val inc =
    Action.Single[Example, Obs, Int, Obs, Int, String](_ => "Increment", (eg, _, s) =>
      Some(() => {
        eg.inc()
        Right(_ => s + 1)
      }),
      Check[Obs, Int, Obs, Int, String, Int](_ => "Count increases by 1",
        (o, _) => Right(o.count),
        (o, _, n) => if (o.count == n + 1) None else Some(s"Expected ${n + 1}, not ${o.count}.")
      )
    )


  override def tests = TestSuite {

    println()
    val eg = new Example(3)
    val h = Runner.run(inc)(3, eg)(eg => Obs(eg.count()))
    println(h)
    println(eg.count())
    println()
  }
}
