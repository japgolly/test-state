package testate.external

import japgolly.univeq.UnivEq
import cats.{Eq => Equal2}

object Blah1
  extends testate.TestateCats
     with testate.Exports

object Blah2
  extends testate.Exports
     with testate.TestateCats

object TestateCatsTest {

  case class U(i: Int)
  implicit def univEqU: UnivEq[U] = UnivEq.force

  case class E(i: Int)
  object E {
    import testate.typeclass.Equal
    implicit def equalE: Equal[E] = Equal.by_==
  }

  object test {
    import testate.typeclass.Equal
    def apply[A](implicit e : Equal [A], ek : Equal [List[A]],
                          e2: Equal2[A], ek2: Equal2[List[A]]) = ()
  }

  object Test1 {
    import Blah1._
    test[Int]
    test[U]
    test[E]
  }

  object Test2 {
    import Blah2._
    test[Int]
    test[U]
    test[E]
  }
}
