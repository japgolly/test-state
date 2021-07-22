package teststate.dsl

import teststate.data.Name.Implicits._
import teststate.data.Name
import teststate.typeclass._

object OptionAssertions {

  sealed abstract class Contains {
    def name(subject: => String, queryName: => String): Name
    def apply[A, B](source: Option[A], query: B)(implicit ev: A <:< B, eb: Equal[B], sb: Display[B]): Option[Contains.Failure[B]]
    protected final def found[A, B](source: Option[A], query: B)(implicit ev: A <:< B, eb: Equal[B]) =
      source.exists(a => eb.equal(query, a))
  }

  object Contains {
    def name(expect: Boolean, subject: => String, queryName: => String): Name =
      Name.lazily(apply(expect).name(subject, queryName))

    def apply(positive: Boolean): Contains =
      if (positive) Pos else Neg

    object Pos extends Contains {
      override def name(subject: => String, queryName: => String): Name =
        s"$subject should contain $queryName."
      override def apply[A, B](source: Option[A], query: B)(implicit ev: A <:< B, eb: Equal[B], sb: Display[B]): Option[Missing[B]] =
        if (found(source, query))
          None
        else
          Some(Missing(source.map(ev), query))
    }

    object Neg extends Contains {
      override def name(subject: => String, queryName: => String): Name =
        s"$subject shouldn't contain $queryName."
      override def apply[A, B](source: Option[A], query: B)(implicit ev: A <:< B, eb: Equal[B], sb: Display[B]): Option[Present[B]] =
        if (found(source, query))
          Some(Present(query))
        else
          None
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable
    final case class Missing[A](source: Option[A], query: A)(implicit d: Display[A]) extends Failure[A] {
      override def errorString =
        source match {
          case None => s"Option was empty, expected to contain ${d(query)}."
          case Some(a) => s"Contained ${d(a)}, expected ${d(query)}."
        }
    }
    final case class Present[A](query: A)(implicit d: Display[A]) extends Failure[Nothing] {
      override def errorString = s"Shouldn't contain ${d(query)}."
    }
  }

  // ===================================================================================================================

  sealed abstract class Forall {
    def name(source: => String, criteria: => String): Name
    def apply[A](as: Option[A])(f: A => Boolean)(implicit d: Display[A]): Option[Forall.Failure[A]]
  }

  object Forall {
    def apply(positive: Boolean): Forall =
      if (positive) Pos else Neg

    object Pos extends Forall {
      override def name(source: => String, criteria: => String): Name =
        s"$source should be empty or $criteria."
      override def apply[A](oa: Option[A])(f: A => Boolean)(implicit d: Display[A]): Option[Contained[A]] =
        oa match {
          case None    => None
          case Some(a) => if (f(a)) None else Some(Contained(a))
        }
    }

    object Neg extends Forall {
      override def name(source: => String, criteria: => String): Name =
        s"$source should be defined and not $criteria."
      override def apply[A](oa: Option[A])(f: A => Boolean)(implicit d: Display[A]): Option[Failure[A]] =
        oa match {
          case Some(a) => if (f(a)) Some(Contained(a)) else None
          case None    => Some(Empty)
        }
    }

    sealed trait Failure[+A] extends HasErrorString with Product with Serializable
    final case class Contained[+A](value: A)(implicit d: Display[A]) extends Failure[A] {
      override def errorString = s"Contained ${d(value)}."
    }
    case object Empty extends Failure[Nothing] {
      override def errorString = "Option was empty."
    }
  }

  // ===================================================================================================================

  sealed abstract class Exists {
    def name(source: => String, criteria: => String): Name
    def apply[A](as: Option[A])(f: A => Boolean)(implicit d: Display[A]): Option[Exists.Failure[A]]
  }

  object Exists {
    def apply(positive: Boolean): Exists =
      if (positive) Pos else Neg

    import Forall.{Contained, Empty}
    type Failure[+A] = Forall.Failure[A]

    object Pos extends Exists {
      override def name(source: => String, criteria: => String): Name =
        s"$source should be defined and $criteria."
      override def apply[A](oa: Option[A])(f: A => Boolean)(implicit d: Display[A]): Option[Failure[A]] =
        oa match {
          case None    => Some(Empty)
          case Some(a) => if (f(a)) None else Some(Contained(a))
        }
    }

    object Neg extends Exists {
      override def name(source: => String, criteria: => String): Name =
        s"$source should be empty or not $criteria."
      override def apply[A](oa: Option[A])(f: A => Boolean)(implicit d: Display[A]): Option[Contained[A]] =
        oa match {
          case Some(a) => if (f(a)) Some(Contained(a)) else None
          case None    => None
        }
    }
  }

}
