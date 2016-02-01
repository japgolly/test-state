package teststate

import utest._
import CollectionAssertions._
import TestUtil._

import scala.annotation.tailrec

object CollectionAssertionsTest extends TestSuite {

  def set(s: String): Set[Char] =
    s.toCharArray.toSet

  val z    = set("")
  val a    = set("a")
  val d    = set("d")
  val ab   = set("ab")
  val cd   = set("cd")
  val abc  = set("abc")
  val bcd  = set("bcd")
  val abcd = set("abcd")

  val n0 = 0 to 2

  val data =
    for {
      a1 <- n0
      b1 <- n0
      c1 <- n0
      a2 <- n0
      b2 <- n0
    } yield {
      val sb = new StringBuilder
      @tailrec def add(c: Char, n: Int): Unit =
        if (n != 0) {
        sb append c
          add(c, n - 1)
      }
      add('a', a1)
      add('b', b1)
      add('c', c1)
      add('a', a2)
      add('b', b2)
      sb.result()
    }

  val dataSets = data.map(_.toSet)

  type D = String

  def test1(name: String, f: (Boolean, D) => Option[Any], expectPass: D => Boolean) =
    for (d <- data) {
      val e = expectPass(d)
      val n = s"$name: [$d]"
      assertDefined(n, f(true, d), !e)
      assertDefined("¬" + n, f(false, d), e)
    }

  def test2(name: String, f: (Boolean, D, Set[Char]) => Option[Any], expectPass: (D, Set[Char]) => Boolean) =
    for {d <- data; s <- dataSets} {
      val e = expectPass(d, s)
      val n = s"$name: [$d] $s"
      assertDefined(n, f(true, d, s), !e)
      assertDefined("¬" + n, f(false, d, s), e)
    }

  def test2d(name: String, f: (Boolean, D, D) => Option[Any], expectPass: (D, D) => Boolean) =
    for {d <- data; s <- data} {
      val e = expectPass(d, s)
      val n = s"$name: [$d] [$s]"
      assertDefined(n, f(true, d, s), !e)
      assertDefined("¬" + n, f(false, d, s), e)
    }

  override def tests = TestSuite {
    'containsAll  - test2("containsAll" , ContainsAll (_)(_, _), (d, s) => s.forall(d contains _))
    'containsAny  - test2("containsAny" , ContainsAny (_)(_, _), (d, s) => s.exists(d contains _))
    'containsAny2 - test2("containsAny2", ContainsAny (_)(_, _), (d, s) => d.exists(s.contains))
    'containsOnly - test2("containsOnly", ContainsOnly(_)(_, _), (d, s) => d.forall(s.contains))

    'distinct - test1("distinct", Distinct(_)(_), d => d.sorted.distinct.length == d.length)

    'equalIgnoringOrder - test2d("equalIgnoringOrder", EqualIgnoringOrder(_)(_, _), (a, b) => a.sorted == b.sorted)
  }
}
