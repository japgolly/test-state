package teststate

import scala.io.AnsiColor._

object TestUtil extends TestUtil

trait TestUtil {

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEq(None, actual, expect)

  def assertEq[A: Equal](name: String, actual: A, expect: A): Unit =
    assertEq(Some(name), actual, expect)

  def assertEq[A](name: Option[String], actual: A, expect: A)(implicit eq: Equal[A]): Unit =
    if (!eq.equal(actual, expect)) {
      println()
      name.foreach(n => println(s">>>>>>> $n"))

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
}
