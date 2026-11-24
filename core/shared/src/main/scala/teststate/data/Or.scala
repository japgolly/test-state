package teststate.data

import japgolly.univeq._
import scala.annotation.nowarn

sealed abstract class Or[+A, +B] extends Product with Serializable {
  def fold[C](l: A => C, r: B => C): C
  def cata[C](l: Left[A] => C, r: Right[B] => C): C

  final def isLeft: Boolean =
    !isRight

  final def isRight: Boolean =
    this match {
      case _: Right[B] => true
      case _ => false
    }

  final def exists(f: B => Boolean): Boolean =
    this match {
      case Right(b) => f(b)
      case Left(_)  => false
    }

  final def flatMap[C >: A, D](f: B => C Or D): C Or D =
    this match {
      case Right(b)   => f(b)
      case l: Left[A] => l
    }

  final def map[C >: A, D](f: B => D): C Or D =
    this match {
      case Right(b)   => Right(f(b))
      case l: Left[A] => l
    }

  final def flatten[C, D](implicit ev: Or[A, B] <:< Or[C, C Or D]): C Or D =
    ev(this) match {
      case Right(r)   => r
      case l: Left[C] => l
    }

  final def >>[C >: A, D](r: => C Or D): C Or D =
    this match {
      case Right(_)   => r
      case l: Left[A] => l
    }

  final def set[C >: A, D](d: => D): C Or D =
    this match {
      case Right(_)   => Right(d)
      case l: Left[A] => l
    }

  final def bimap[C, D](l: A => C, r: B => D): C Or D =
    this match {
      case Right(b) => Right(r(b))
      case Left(a)  => Left(l(a))
    }

  final def recover[C >: B](f: A => C): C =
    this match {
      case Right(b) => b
      case Left(a)  => f(a)
    }

  final def leftMap[C, D >: B](f: A => C): C Or D =
    this match {
      case r: Right[B] => r
      case Left(a)     => Left(f(a))
    }

  final def test[C >: A](f: B => Option[C]): C Or B =
    this match {
      case r: Right[B] =>
        f(r.right) match {
          case None    => r
          case Some(a) => Left(a)
        }
      case l: Left[A] => l
    }

  final def leftOption: Option[A] =
    this match {
      case Left(a)  => Some(a)
      case Right(_) => None
    }

  final def toOption: Option[B] =
    this match {
      case Right(b) => Some(b)
      case Left(_)  => None
    }

  final def toOptionMap[C](f: B => C): Option[C] =
    this match {
      case Right(b) => Some(f(b))
      case Left(_)  => None
    }

  final def toTri: Tri[A, B] =
    this match {
      case Right(b) => Passed(b)
      case Left(a)  => Failed(a)
    }

  final def toTriMap[C](f: B => C): Tri[A, C] =
    this match {
      case Right(b) => Passed(f(b))
      case Left(a)  => Failed(a)
    }

  final def toTriFlatMap[C >: A, D](f: B => Tri[C, D]): Tri[C, D] =
    this match {
      case Right(b) => f(b)
      case Left(a)  => Failed(a)
    }

  final def toEither: Either[A, B] =
    this match {
      case Right(b) => scala.Right(b)
      case Left(a)  => scala.Left(a)
    }
}

final case class Left[+A](left: A) extends Or[A, Nothing] {
  override def fold[C](l: A => C, r: Nothing => C): C = l(left)
  override def cata[C](l: Left[A] => C, r: Right[Nothing] => C): C = l(this)
}

final case class Right[+B](right: B) extends Or[Nothing, B] {
  override def fold[C](l: Nothing => C, r: B => C): C = r(right)
  override def cata[C](l: Left[Nothing] => C, r: Right[B] => C): C = r(this)
}

object Or {
  val rightUnit: Right[Unit]    = Right(())
  val rightTrue: Right[Boolean] = Right(true)

  def liftLeft[A, B](o: Option[A]): A Or Unit =
    o match {
      case None    => rightUnit
      case Some(a) => Left(a)
    }

  def liftLeft[A, B](o: Option[A], b: => B): A Or B =
    o match {
      case None    => Right(b)
      case Some(a) => Left(a)
    }

  def fromScalaEither[A, B](e: A Either B): A Or B =
    e match {
      case scala.Right(b) => Right(b)
      case scala.Left(a)  => Left(a)
    }

  @nowarn("cat=unused")
  implicit def univEq[A: UnivEq, B: UnivEq]: UnivEq[Or[A, B]] =
    UnivEq.derive
}
