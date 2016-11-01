package teststate

import acyclic.file
import cats._
import cats.data._
import cats.functor._
import teststate.{data => D, typeclass => T}
import D.Or
import Exports._

trait ExtCats extends T.Equal.ImplicitsLowPri {

  implicit def catsMonoidMonoComposableEmpty[Op, A](implicit e: Empty[A], c: T.PolyComposable.Mono[Op, A]): Monoid[A] =
    new Monoid[A] {
      override def empty               = e.instance
      override def combine(x: A, y: A) = c.compose(x, y)
    }

  implicit lazy val catsMonoidReportStats: Monoid[Report.Stats] =
    new Monoid[Report.Stats] {
      override def empty                                     = Report.Stats.empty
      override def combine(x: Report.Stats, y: Report.Stats) = x + y
    }

  implicit def catsProfunctorFromTestState[M[_, _]](implicit p: T.Profunctor[M]): Profunctor[M] =
    new Profunctor[M] {
      override def lmap [A, B, C]   (m: M[A, B])(f: C => A)            = p.lmap(m)(f)
      override def rmap [A, B, C]   (m: M[A, B])(f: B => C)            = p.rmap(m)(f)
      override def dimap[A, B, C, D](m: M[A, B])(f: C => A)(g: B => D) = p.dimap(m)(f, g)
    }

  implicit def catsNatTransFromTestState[F[_], G[_]](implicit t: F ~> G): T.~~>[F, G] =
    new T.~~>[F, G] { def apply[A](fa: => F[A]) = t(fa) }

  implicit def catsNatTransToTestState[F[_], G[_]](implicit t: T.~~>[F, G]): F ~> G =
    new (F ~> G) { def apply[A](fa: F[A]) = t(fa) }

  implicit def catsEqualFromTestState[A](implicit e: Equal[A]): Eq[A] =
    Eq.instance(e.equal)

  implicit def catsEqualToTestState[A](implicit e: Eq[A]): Equal[A] =
    Equal(e.eqv)
}

object ExtCats extends ExtCats
