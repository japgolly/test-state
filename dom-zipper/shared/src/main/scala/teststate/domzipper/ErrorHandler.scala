package teststate.domzipper

trait ErrorHandler[Result[_]] {

  def pass[A](a: A): Result[A]
  def fail[A](e: => String): Result[A]
  def map[A, B](r: Result[A])(f: A => B): Result[B]

  def apply[A](e: Either[String, A]): Result[A] =
    e match {
      case Right(a) => pass(a)
      case Left(s) => fail(s)
    }
}

object ErrorHandler {

  implicit class ErrorHandlerResultOps[Result[_], A](private val self: Result[A]) extends AnyVal {
    def map[B](f: A => B)(implicit h: ErrorHandler[Result]): Result[B] =
      h.map(self)(f)
  }

  implicit class ErrorHandlerOptionOps[A](private val self: Option[A]) extends AnyVal {
    def orFail[Result[_]](f: => String)(implicit h: ErrorHandler[Result]): Result[A] =
      self.fold[Result[A]](h fail f)(h.pass)
  }

  type Id[A] = A

  object Throw extends ErrorHandler[Id] {
    override def pass[A](a: A)              = a
    override def fail[A](e: => String)      = sys error e
    override def map[A, B](r: A)(f: A => B) = f(r)
  }

  type ErrMsgOr[A] = Either[String, A]

  object ReturnEither extends ErrorHandler[ErrMsgOr] {
    override def pass[A](a: A)                        = Right(a)
    override def fail[A](e: => String)                = Left(e)
    override def map[A, B](r: ErrMsgOr[A])(f: A => B) = r.right map f
    override def apply[A](e: ErrMsgOr[A])             = e
  }

  object ReturnOption extends ErrorHandler[Option] {
    override def pass[A](a: A)                      = Some(a)
    override def fail[A](e: => String)              = None
    override def map[A, B](r: Option[A])(f: A => B) = r map f
  }

//  object ReturnDisjunction extends ErrorHandler {
//    import scalaz._
//    override type Result[A]                         = String \/ A
//    override def pass[A](a: A)                      = \/-(a)
//    override def fail[A](e: => String)              = -\/(e)
//    override def map[A, B](r: Result[A])(f: A => B) = r map f
//  }
}
