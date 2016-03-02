package teststate

sealed abstract class TriResult[+E, +A] extends Product with Serializable {
  def mapE[X](f: E => X): TriResult[X, A]
  def map[X](f: A => X): TriResult[E, X]
  def flatMap[X, EE >: E](f: A => TriResult[EE, X]): TriResult[EE, X]
}

case object Skipped extends TriResult[Nothing, Nothing] {
  override def mapE[X](f: Nothing => X) = this
  override def map[X](f: Nothing => X) = this
  override def flatMap[X, EE](f: Nothing => TriResult[EE, X]) = this
}

final case class Passed[+A](result: A) extends TriResult[Nothing, A] {
  override def mapE[X](f: Nothing => X) = this
  override def map[X](f: A => X) = Passed(f(result))
  override def flatMap[X, EE](f: A => TriResult[EE, X]) = f(result)
}

final case class Failed[+E](failure: E) extends TriResult[E, Nothing] {
  override def mapE[X](f: E => X) = Failed(f(failure))
  override def map[X](f: Nothing => X) = this
  override def flatMap[X, EE >: E](f: Nothing => TriResult[EE, X]) = this
}

object TriResult {
  val pass = Passed(())

  def failedOption[E](o: Option[E]): TriResult[E, Unit] =
    o match {
      case None    => pass
      case Some(e) => Failed(e)
    }
}
