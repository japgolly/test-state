package testate.data

sealed abstract class Failure[+A] {
  val failure: A
  def cause: Option[Throwable]
  def map[B](f: A => B): Failure[B]
}

object Failure {

  final case class NoCause[+A](failure: A) extends Failure[A] {
    override def cause = None
    override def map[B](f: A => B) = NoCause(f(failure))
  }

  final case class WithCause[+A](failure: A, theCause: Throwable) extends Failure[A] {
    override def cause = Some(theCause)
    override def map[B](f: A => B) = WithCause(f(failure), theCause)
  }
}
