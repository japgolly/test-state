package teststate

import scala.io.AnsiColor._
import Exports._

object TestUtil extends TestUtil

case class ActualExpect[A](actual: A, expect: A) {
  def map[B](f: A => B) = ActualExpect[B](f(actual), f(expect))

  def forall(f: A => Boolean) = f(actual) && f(expect)
  def exists(f: A => Boolean) = f(actual) || f(expect)
}

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

      val strs = ActualExpect(actual, expect) map toString
      val lines = strs.map(_ split '\n')

      if (lines.actual.length == lines.expect.length && lines.actual.length > 1) {
        // Print side-by-side
        val norm = lines.map(_.map(_.replace("\t", "\\t")))
        val maxs = norm.map(_.iterator.map(_.length).max max 6)
        val fmt = s"%s%-${maxs.expect}s$RESET | %s%s$RESET"
        val sep = s"${"-" * maxs.expect}-+-${"-" * maxs.actual}"
        val B = BOLD + RED
        val G = ""
        println(sep)
        println(fmt.format(G, "Expect", G, "Actual"))
        println(sep)
        for ((l, r) <- norm.expect zip norm.actual) {
          println(if (l == r)
              fmt.format(G, l, G, r)
          else
              fmt.format(B, l, B, r))
        }
        println(sep)

      } else {
        var pre = "["
        val post = "]"
        if ((strs.actual + strs.expect) contains "\n") {
          pre = "↙[\n"
        }
        println(s"expect: $pre$BOLD$BLUE${strs.expect}$RESET$post")
        println(s"actual: $pre$BOLD$RED${strs.actual}$RESET$post")
      }

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
      assertEq(name, o, None)(Equal.by_==)
}
