package teststate.typeclass

import japgolly.univeq.UnivEq
import scala.annotation.nowarn

class Equal[A](val equal: (A, A) => Boolean) extends AnyVal

object Equal {

  private val anyEqualFn: (Any, Any) => Boolean =
    _ == _

  def apply[A](fn: (A, A) => Boolean): Equal[A] =
    new Equal(fn)

  def by_==[A]: Equal[A] =
    Equal(anyEqualFn.asInstanceOf[(A, A) => Boolean])

  trait ImplicitsLowPri {
    @nowarn("cat=unused")
    implicit def testStateEqualByUnivEq[A: UnivEq]: Equal[A] =
      by_==
  }

  trait Implicits extends ImplicitsLowPri {
    // Cache oft-used instances
    implicit val testStateEqualChar   : Equal[Char   ] = by_==
    implicit val testStateEqualString : Equal[String ] = by_==
    implicit val testStateEqualBoolean: Equal[Boolean] = by_==
    implicit val testStateEqualInt    : Equal[Int    ] = by_==
    implicit val testStateEqualLong   : Equal[Long   ] = by_==

    implicit def testStateEqualOption[A](implicit e: Equal[A]): Equal[Option[A]] =
      Equal((a, b) => a match {
        case None    => b.isEmpty
        case Some(x) => b.exists(e.equal(x, _))
      })

    implicit def testStateEqualSeq[C[x] <: Seq[x], A](implicit e: Equal[A]): Equal[C[A]] =
      Equal((x, y) =>
        (x eq y)
          || x.corresponds(y)(e.equal))

    implicit def testStateEqualIndexedSeq[C[x] <: IndexedSeq[x], A](implicit e: Equal[A]): Equal[C[A]] =
      Equal { (x, y) =>
        (x eq y) || {
          val len = x.length
          if (len != y.length)
            false
          else {
            var i = 0
            var eq = true
            while (eq && i < len) {
              eq = e.equal(x(i), y(i))
              i += 1
            }
            eq
          }
        }
      }
  }
}
