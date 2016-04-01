package teststate

import acyclic.file
import cats._
import cats.data._
import cats.functor._
import teststate.{data => D, typeclass => T}
import D.Or
import Exports._

trait TestStateCats {

  implicit def catsMonoidPlan[F[_], R, O, S, E](implicit em: ExecutionModel[F]): Monoid[Plan[F, R, O, S, E]] = {
    type A = Plan[F, R, O, S, E]
    new Monoid[A] {
      override def empty               = Plan.empty(em)
      override def combine(x: A, y: A) = x >> y
    }
  }

  implicit def catsMonoidActions[F[_], R, O, S, E]: Monoid[Actions[F, R, O, S, E]] = {
    type A = Actions[F, R, O, S, E]
    new Monoid[A] {
      override def empty               = emptyAction
      override def combine(x: A, y: A) = x >> y
    }
  }

  implicit def catsMonoidInvariants[O, S, E]: Monoid[Invariants[O, S, E]] = {
    type A = Invariants[O, S, E]
    new Monoid[A] {
      override def empty               = emptyInvariants
      override def combine(x: A, y: A) = x & y
    }
  }

  implicit def catsMonoidPoints[O, S, E]: Monoid[Points[O, S, E]] = {
    type A = Points[O, S, E]
    new Monoid[A] {
      override def empty               = emptyPoints
      override def combine(x: A, y: A) = x & y
    }
  }

  implicit def catsMonoidArounds[O, S, E]: Monoid[Arounds[O, S, E]] = {
    type A = Arounds[O, S, E]
    new Monoid[A] {
      override def empty               = emptyArounds
      override def combine(x: A, y: A) = x & y
    }
  }

  implicit def catsProfunctorFromTestState[M[_, _]](implicit p: T.Profunctor[M]): Profunctor[M] =
    new Profunctor[M] {
      override def lmap [A, B, C]   (m: M[A, B])(f: C => A)            = p.lmap(m)(f)
      override def rmap [A, B, C]   (m: M[A, B])(f: B => C)            = p.rmap(m)(f)
      override def dimap[A, B, C, D](m: M[A, B])(f: C => A)(g: B => D) = p.dimap(m)(f, g)
    }

  implicit def catsDisjunctionFromTestState[A, B](o: A Or B): A Xor B =
    o.fold(Xor.Left.apply, Xor.Right.apply)

  implicit def catsDisjunctionToTestState[A, B](d: A Xor B): A Or B =
    d.fold(D.Left.apply, D.Right.apply)

  implicit def catsNatTransFromTestState[F[_], G[_]](implicit t: F ~> G): T.~~>[F, G] =
    new T.~~>[F, G] { def apply[A](fa: => F[A]) = t(fa) }

  implicit def catsNatTransToTestState[F[_], G[_]](implicit t: T.~~>[F, G]): F ~> G =
    new (F ~> G) { def apply[A](fa: F[A]) = t(fa) }

  implicit def catsEqualFromTestState[A](implicit e: Equal[A]): Eq[A] =
    Eq.instance(e.equal)

  implicit def catsEqualToTestState[A](implicit e: Eq[A]): Equal[A] =
    Equal(e.eqv)

  implicit lazy val catsMonoidReportStats: Monoid[Report.Stats] =
    new Monoid[Report.Stats] {
      override def empty                                     = Report.Stats.empty
      override def combine(x: Report.Stats, y: Report.Stats) = x + y
    }
}

object TestStateCats extends TestStateCats
