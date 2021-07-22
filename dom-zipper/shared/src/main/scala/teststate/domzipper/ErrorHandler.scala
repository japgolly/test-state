package teststate.domzipper

import scala.util.control.NonFatal

trait ErrorHandler[Result[_]] {

  def pass[A](a: A): Result[A]
  def fail[A](e: => String): Result[A]
  def map[A, B](r: Result[A])(f: A => B): Result[B]
  def flatMap[A, B](r: Result[A])(f: A => Result[B]): Result[B]

  def apply[A](e: Either[String, A]): Result[A] =
    e match {
      case Right(a) => pass(a)
      case Left(s) => fail(s)
    }

  def option[A](o: Option[A], err: => String): Result[A] =
    o match {
      case Some(a) => pass(a)
      case None => fail(err)
    }

  def attempt[A](a: => A): Result[A] =
    try pass(a) catch {
      case NonFatal(t) => fail(t.getMessage)
    }

  private[domzipper] final val C01 = new DomZipper.DomCollection.Container01()(this)
  private[domzipper] final val C0N = new DomZipper.DomCollection.Container0N()(this)
  private[domzipper] final val C1N = new DomZipper.DomCollection.Container1N()(this)
}

object ErrorHandler {

  implicit class ErrorHandlerResultOps[Result[_], A](private val self: Result[A]) extends AnyVal {
    def map[B](f: A => B)(implicit h: ErrorHandler[Result]): Result[B] =
      h.map(self)(f)
    def flatMap[B](f: A => Result[B])(implicit h: ErrorHandler[Result]): Result[B] =
      h.flatMap(self)(f)
  }

  implicit class ErrorHandlerOptionOps[A](private val self: Option[A]) extends AnyVal {
    def orFail[Result[_]](f: => String)(implicit h: ErrorHandler[Result]): Result[A] =
      self.fold[Result[A]](h fail f)(h.pass)
  }

  type Id[A] = A

  object Throw extends ErrorHandler[Id] {
    override def pass[A](a: A)                  = a
    override def fail[A](e: => String)          = sys error e
    override def map[A, B](r: A)(f: A => B)     = f(r)
    override def flatMap[A, B](r: A)(f: A => B) = f(r)

    override def attempt[A](a: => A): A =
      try pass(a) catch {
        case NonFatal(t) => throw t
      }
  }

  type ErrMsgOr[A] = Either[String, A]

  object ReturnEither extends ErrorHandler[ErrMsgOr] {
    override def pass[A](a: A)                                      = Right(a)
    override def fail[A](e: => String)                              = Left(e)
    override def map[A, B](r: ErrMsgOr[A])(f: A => B)               = r map f
    override def flatMap[A, B](r: ErrMsgOr[A])(f: A => ErrMsgOr[B]) = r flatMap f
    override def apply[A](e: ErrMsgOr[A])                           = e
  }

  object ReturnOption extends ErrorHandler[Option] {
    override def pass[A](a: A)                                  = Some(a)
    override def fail[A](e: => String)                          = None
    override def map[A, B](r: Option[A])(f: A => B)             = r map f
    override def flatMap[A, B](r: Option[A])(f: A => Option[B]) = r flatMap f
  }
}
