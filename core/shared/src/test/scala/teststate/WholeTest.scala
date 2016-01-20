package teststate

import utest._

object WholeTest extends TestSuite {

  class Example(start: Int) {
    private var c = start
    def inc() = c += 1
    def dec() = c -= 1
    def count() = c
  }


  override def tests = TestSuite {

    val eg = new Example(3)

    val inc =
      Action.Single[Int, Int, Int, Int, String](_ => "Increment", (_, o) =>
        Some(() => {
          eg.inc()
          Right((_: Int) + 1)
        }),
        Check[Int, Int, Int, Int, String, Int](_ => "Count increases by 1",
          (_, o) => Right(o),
          (_, o, n) => if (o == n + 1) None else Some(s"Expected ${n + 1}, not $o.")
        )
      )

    println()
    val h = Runner.run(inc)(3, () => eg.count())
    println(h)
    println(eg.count())
    println()
  }
}
