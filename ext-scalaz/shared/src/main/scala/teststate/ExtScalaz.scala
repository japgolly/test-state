package teststate

import acyclic.file
import scalaz._
import teststate.{data => D, typeclass => T}
import D.Or
import Exports.{Equal => _, _}
import Isomorphism._

trait ExtScalaz extends T.Equal.ImplicitsLowPri {

  implicit def scalazMonoidMonoComposableEmpty[Op, A](implicit e: Empty[A], c: T.PolyComposable.Mono[Op, A]): Monoid[A] =
    new Monoid[A] {
      override def zero                  = e.instance
      override def append(x: A, y: => A) = c.compose(x, y)
    }

  implicit lazy val scalazMonoidReportStats: Monoid[Report.Stats] =
    new Monoid[Report.Stats] {
      override def zero                                        = Report.Stats.empty
      override def append(x: Report.Stats, y: => Report.Stats) = x + y
    }

  implicit def scalazProfunctorFromTestState[M[_, _]](implicit p: T.Profunctor[M]): Profunctor[M] =
    new Profunctor[M] {
      override def mapfst[A, B, C]   (m: M[A, B])(f: C => A)            = p.lmap(m)(f)
      override def mapsnd[A, B, C]   (m: M[A, B])(f: B => C)            = p.rmap(m)(f)
      override def dimap [A, B, C, D](m: M[A, B])(f: C => A)(g: B => D) = p.dimap(m)(f, g)
    }

  implicit def scalazDisjunctionFromTestState[A, B](o: A Or B): A \/ B =
    o.fold(-\/.apply, \/-.apply)

  implicit def scalazDisjunctionToTestState[A, B](d: A \/ B): A Or B =
    d.fold(D.Left.apply, D.Right.apply)

  implicit lazy val scalazIsoDisjunctionTestState: \/ <~~> Or =
    new (\/ <~~> Or) {
      override val to  : \/ ~~> Or = new (\/ ~~> Or) { def apply[A, B](d: A \/ B) = d }
      override val from: Or ~~> \/ = new (Or ~~> \/) { def apply[A, B](o: A Or B) = o }
    }

  implicit lazy val scalazIsoEitherTestState: Either <~~> Or =
    new (Either <~~> Or) {
      override val to  : Either ~~> Or = new (Either ~~> Or) { def apply[A, B](d: A Either B) = d }
      override val from: Or ~~> Either = new (Or ~~> Either) { def apply[A, B](o: A Or B) = o.fold(scala.Left.apply, scala.Right.apply) }
    }

  implicit def scalazNatTransFromTestState[F[_], G[_]](implicit t: F ~> G): T.~~>[F, G] =
    new T.~~>[F, G] { def apply[A](fa: => F[A]) = t(fa) }

  implicit def scalazNatTransToTestState[F[_], G[_]](implicit t: T.~~>[F, G]): F ~> G =
    new (F ~> G) { def apply[A](fa: F[A]) = t(fa) }

  implicit def scalazEqualFromTestState[A](implicit e: T.Equal[A]): Equal[A] =
    Equal.equal(e.equal)

  implicit def scalazEqualToTestState[A](implicit e: Equal[A]): T.Equal[A] =
    T.Equal(e.equal)
}

object ExtScalaz extends ExtScalaz
