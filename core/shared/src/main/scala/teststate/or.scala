package teststate

final case class Left[+A](left: A) extends Or[A, Nothing] {
  override def fold[C](l: A => C, r: Nothing => C): C = l(left)
  override def cata[C](l: Left[A] => C, r: Right[Nothing] => C): C = l(this)
}

final case class Right[+B](right: B) extends Or[Nothing, B] {
  override def fold[C](l: Nothing => C, r: B => C): C = r(right)
  override def cata[C](l: Left[Nothing] => C, r: Right[B] => C): C = r(this)
}

sealed abstract class Or[+A, +B] {
  def fold[C](l: A => C, r: B => C): C
  def cata[C](l: Left[A] => C, r: Right[B] => C): C

  final def flatMap[C >: A, D](f: B => C Or D): C Or D =
    this match {
      case Right(b) => f(b)
      case l: Left[A] => l
    }

  final def map[C >: A, D](f: B => D): C Or D =
    flatMap(b => Right(f(b)))

  final def flatten[C, D](implicit ev: Or[A, B] <:< Or[C, C Or D]): C Or D =
    ev(this).flatMap(identity)

  final def >>[C >: A, D](r: => C Or D): C Or D =
    flatMap(_ => r)

  final def set[C >: A, D](d: => D): C Or D =
    this >> Right(d)

  final def bimap[C, D](l: A => C, r: B => D): C Or D =
    fold(a => Left(l(a)), b => Right(r(b)))

  final def leftMap[C, D >: B](f: A => C): C Or D =
    this match {
      case r: Right[B] => r
      case Left(a) => Left(f(a))
    }

  final def test[C >: A](f: B => Option[C]): C Or B =
    this match {
      case r: Right[B] =>
        f(r.right) match {
          case None => r
          case Some(a) => Left(a)
        }
      case l: Left[A] => l
    }

  final def leftOption: Option[A] =
    fold(Some(_), _ => None)

  final def toOption: Option[B] =
    fold(_ => None, Some(_))

  final def toOptionMap[C](f: B => C): Option[C] =
    fold(_ => None, b => Some(f(b)))

  final def toTriResult: TriResult[A, B] =
    toTriResultFlatMap(Passed(_))

  final def toTriResultMap[C](f: B => C): TriResult[A, C] =
    toTriResultFlatMap(b => Passed(f(b)))

  final def toTriResultFlatMap[C >: A, D](f: B => TriResult[C, D]): TriResult[C, D] =
    fold(Failed(_), f)
}

object Or {
  def liftLeft[A, B](o: Option[A]): A Or Unit =
    liftLeft(o, ())

  def liftLeft[A, B](o: Option[A], b: => B): A Or B =
    o.fold[A Or B](Right(b))(Left(_))
}