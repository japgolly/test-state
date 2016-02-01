package teststate

import scala.io.AnsiColor._

object TestUtil extends TestUtil

trait TestUtil {

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEq(null, actual, expect)

  def assertEq[A](name: => String, actual: A, expect: A)(implicit eq: Equal[A]): Unit =
    if (!eq.equal(actual, expect)) {
      println()
      Option(name).foreach(n => println(s">>>>>>> $n"))

      val toString: Any => String = {
        case s: Stream[_] => s.force.toString() // SI-9266
        case a            => a.toString
      }

      var as = toString(actual)
      var es = toString(expect)
      var pre = "["
      var post = "]"
      if ((as + es) contains "\n") {
        pre = "↙[\n"
      }
      println(s"expect: $pre$BOLD$BLUE$es$RESET$post")
      println(s"actual: $pre$BOLD$RED$as$RESET$post")
      println()
      fail("assertEq failed.")
    }

  def fail(msg: String): Nothing = {
    val e = new AssertionError(msg)
    e.setStackTrace(Array.empty)
    throw e
  }

  def assertDefined[A](o: Option[A], expectDefined: Boolean): Unit =
    assertDefined(null, o, expectDefined)

  def assertDefined[A](name: => String, o: Option[A], expectDefined: Boolean): Unit =
    if (expectDefined)
      assertEq(name, o.isDefined, true)
    else
      assertEq(name, o, None)(Equal.byUnivEq)
}
