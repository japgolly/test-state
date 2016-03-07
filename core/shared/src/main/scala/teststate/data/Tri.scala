package teststate.data

import acyclic.file

sealed abstract class Tri[+E, +A] extends Product with Serializable {
  def mapE[X](f: E => X): Tri[X, A]
  def map[X](f: A => X): Tri[E, X]
  def flatMap[X, EE >: E](f: A => Tri[EE, X]): Tri[EE, X]

  final def toResult: Result[E] =
    this match {
      case Passed(_) => Pass
      case Skipped   => Skip
      case Failed(e) => Fail(e)
    }
}

case object Skipped extends Tri[Nothing, Nothing] {
  override def mapE[X](f: Nothing => X) = this
  override def map[X](f: Nothing => X) = this
  override def flatMap[X, EE](f: Nothing => Tri[EE, X]) = this
}

final case class Passed[+A](result: A) extends Tri[Nothing, A] {
  override def mapE[X](f: Nothing => X) = this
  override def map[X](f: A => X) = Passed(f(result))
  override def flatMap[X, EE](f: A => Tri[EE, X]) = f(result)
}

final case class Failed[+E](failure: E) extends Tri[E, Nothing] {
  override def mapE[X](f: E => X) = Failed(f(failure))
  override def map[X](f: Nothing => X) = this
  override def flatMap[X, EE >: E](f: Nothing => Tri[EE, X]) = this
}

object Tri {
  val pass = Passed(())

  def failedOption[E](o: Option[E]): Tri[E, Unit] =
    o match {
      case None    => pass
      case Some(e) => Failed(e)
    }
}
