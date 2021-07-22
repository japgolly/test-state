package teststate.external

import cats.{Eq => Equal2}
import japgolly.univeq.UnivEq
import scala.annotation.nowarn

object Blah1
  extends teststate.ExtCats
     with teststate.Exports

object Blah2
  extends teststate.Exports
     with teststate.ExtCats

object ExtCatsTest {

  case class U(i: Int)
  implicit def univEqU: UnivEq[U] = UnivEq.force

  case class E(i: Int)
  object E {
    import teststate.typeclass.Equal
    implicit def equalE: Equal[E] = Equal.by_==
  }

  object test {
    import teststate.typeclass.Equal
    @nowarn("cat=unused")
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
