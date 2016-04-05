package testate

import acyclic.file
import cats._
import cats.data._
import cats.functor._
import testate.{data => D, typeclass => T}
import D.Or
import Exports._

trait TestateCats extends T.Equal.ImplicitsLowPri {

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

  implicit def catsProfunctorFromTestate[M[_, _]](implicit p: T.Profunctor[M]): Profunctor[M] =
    new Profunctor[M] {
      override def lmap [A, B, C]   (m: M[A, B])(f: C => A)            = p.lmap(m)(f)
      override def rmap [A, B, C]   (m: M[A, B])(f: B => C)            = p.rmap(m)(f)
      override def dimap[A, B, C, D](m: M[A, B])(f: C => A)(g: B => D) = p.dimap(m)(f, g)
    }

  implicit def catsDisjunctionFromTestate[A, B](o: A Or B): A Xor B =
    o.fold(Xor.Left.apply, Xor.Right.apply)

  implicit def catsDisjunctionToTestate[A, B](d: A Xor B): A Or B =
    d.fold(D.Left.apply, D.Right.apply)

  implicit def catsNatTransFromTestate[F[_], G[_]](implicit t: F ~> G): T.~~>[F, G] =
    new T.~~>[F, G] { def apply[A](fa: => F[A]) = t(fa) }

  implicit def catsNatTransToTestate[F[_], G[_]](implicit t: T.~~>[F, G]): F ~> G =
    new (F ~> G) { def apply[A](fa: F[A]) = t(fa) }

  implicit def catsEqualFromTestate[A](implicit e: Equal[A]): Eq[A] =
    Eq.instance(e.equal)

  implicit def catsEqualToTestate[A](implicit e: Eq[A]): Equal[A] =
    Equal(e.eqv)
}

object TestateCats extends TestateCats
