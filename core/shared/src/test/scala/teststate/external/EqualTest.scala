package teststate.external

import japgolly.univeq.UnivEq
import teststate.Exports._
import utest._

// Argh implicits
object EqualTest extends TestSuite {

  case class I(i: Int)
  implicit def univEqI: UnivEq[I] = UnivEq.derive

  case class L(l: Long)
  implicit def equalL: Equal[L] = Equal.by_==

  def assertId[A](a: A)(implicit e: Equal[A]): Unit =
    assert(e.equal(a, a))

  def assertPass[A](as: A*)(implicit e: Equal[A]): Unit = {
    as foreach (assertId(_))
    if (as.size > 1) {
      val h = as.head
      as.tail.foreach(t => assert(!e.equal(h, t)))
    }
  }

  class Nope

  override def tests = Tests {

    'builtin {
      'unit    - assertId(())
      'long    - assertPass[Long   ](4L, 5L)
      'int     - assertPass[Int    ](3, 5)
      'short   - assertPass[Short  ](2, 7)
      'byte    - assertPass[Byte   ](1, 3)
      'char    - assertPass('a', 'H')
      'boolean - assertPass(true, false)
      'string  - assertPass("x", "")
    }

    'byUnivEq - assertPass(I(1), I(2))
    'byUnivEqK - assertPass(Some(I(1)), None, Some(I(2)))

    'byEqual  - assertPass(L(1), L(2))
    'byEqualK - assertPass(Some(L(1)), None, Some(L(2)))

    'nope {
      compileError("implicitly[Equal[Nope]]")
      compileError("implicitly[Equal[Option[Nope]]]")
      ()
    }
  }
}
