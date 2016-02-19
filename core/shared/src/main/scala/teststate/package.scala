import scala.annotation.tailrec

package object teststate extends teststate.Name.Implicits {

  @inline private[teststate] def vector1[A](a: A): Vector[A] =
    Vector.empty[A] :+ a


  trait HasErrorString {
    def errorString: String
  }

  implicit def formatHasErrorString(e: HasErrorString): String = e.errorString

  /*
  case class Plan[State, Obj, Err](steps: Plan.Steps[State, Obj, Err]) {
    def andThen(next: Plan[State, Obj, Err]): Plan[State, Obj, Err] =
      Plan(steps ++ next.steps)
  }

  object Plan {
    type Steps[State, Obj, Err] = Vector[Step[State, Obj, Err]]
    case class Step[State, Obj, Err](indent: Int, name: String, action: Action.NonComposite[State, Obj, Err])
  }
  */

  implicit class TestStateExtMethodsForOption[A](private val self: Option[A]) extends AnyVal {
    def leftOrF[L >: A, R](e: => Either[L, R]): Either[L, R] =
      self match {
        case None    => e
        case Some(a) => Left(a)
      }

    @inline def leftOr[L >: A, R](r: => R): Either[L, R] =
      leftOrF(Right(r))
  }

  @inline implicit class TestStateExtMethodsForLeft[A, B](private val self: Left[A, B]) extends AnyVal {
    @inline def recast = self.asInstanceOf[Left[A, Nothing]]
  }
  @inline implicit class TestStateExtMethodsForRight[A, B](private val self: Right[A, B]) extends AnyVal {
    @inline def recast = self.asInstanceOf[Right[Nothing, B]]
  }

  implicit class TestStateExtMethodsForEither[A, B](private val self: Either[A, B]) extends AnyVal {
    def fmap[C](f: B => Either[A, C]): Either[A, C] =
      self match {
        case Right(b) => f(b)
        case l: Left[A, B] => l.recast
      }

    @inline def map[C](f: B => C): Either[A, C] =
      fmap(b => Right(f(b)))

    def leftMap[C](f: A => C): Either[C, B] =
      self match {
        case r: Right[A, B] => r.recast
        case Left(a) => Left(f(a))
      }

    def check[C](f: B => Option[A]): Either[A, B] =
      self match {
        case r: Right[A, B] => f(r.b).leftOrF(r) //.fold[Either[A, B]](r)(Left(_))
        case l: Left[A, B]  => l
      }

    def bimap[C, D](f: A => C, g: B => D): Either[C, D] =
      self match {
        case Right(b) => Right(g(b))
        case Left(a) => Left(f(a))
      }

    def toOptionLeft(f: B => Option[A]): Option[A] =
      self match {
        case Right(b) => f(b)
        case Left(a) => Some(a)
      }
  }

  // Actually this is ShowValue
  case class Show[A](show: A => String) extends AnyVal {
    @inline def apply(a: A): String =
      show(a)

    def map(f: String => String): Show[A] =
      Show(a => f(show(a)))

    def mkString[C[X] <: TraversableOnce[X]](start: String, mid: String, end: String): Show[C[A]] =
      Show(_.toIterator.map(show).mkString(start, mid, end))

    def coll[C[X] <: TraversableOnce[X]]: Show[C[A]] =
      mkString("[", ", ", "]")
  }

  object Show {
    def byToString[A]: Show[A] = Show(_.toString)
    implicit val showBoolean: Show[Boolean] = byToString
    implicit val showInt: Show[Int] = byToString

    implicit val showString: Show[String] = Show[String](s =>
      // Handle \n, \t, spaces (so surrounds), long strings (?)
      "\"" + s + "\""
    )
    implicit val showChar: Show[Char] = Show[Char](s =>
      // Handle \n, \t, spaces (so surrounds), long strings (?)
      "'" + s + "'"
    )

    implicit def showTraversable[C[X] <: Traversable[X], A](implicit show: Show[A]): Show[C[A]] =
      Show(_.toIterator.map(show(_)).mkString(", "))
  }

  case class ShowError[A](show: A => String) extends AnyVal
  object ShowError {
    implicit val showErrorString: ShowError[String] = ShowError(identity)
  }

  implicit def sadfhasdlfkj[F[_], R, O, S, E](b: Dsl.ActionB[F, R, O, S, E]) = b.noStateUpdate

//  implicit def focusDslX2ToCheck[F[_], R, O, S, E](b: Dsl[F, R, O, S, E]#A2) = b.noStateUpdate
//  implicit def focusDsla2ToCheck[O, S, E, A](b: FocusDsl[O, S, E]#A2[A]) = b.check
//  implicit def focusDsli2ToCheck[O, S, E, A](b: FocusDsl[O, S, E]#I2[A]) = b.check
//  implicit def focusDsli2ToChec1[O, S, E, A](b: FocusDsl[O, S, E]#C0[A]) = b.point

  case class ROS[+Ref, +Obs, +State](ref: Ref, obs: Obs, state: State) {
    val os: OS[Obs, State] =
      OS(obs, state)

    val sos: Some[OS[Obs, State]] =
      Some(os)

    def mapR[A](f: Ref => A): ROS[A, Obs, State] =
      copy(ref = f(ref))
  }

  case class OS[+O, +S](obs: O, state: S)

  trait ~~>[F[_], G[_]] {
    def apply[A](fa: => F[A]): G[A]
  }

  trait ExecutionModel[M[_]] {
    final type F[A] = M[A]
    def pure[A](a: A): F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def tailrec[A](a: A)(stop: A => Boolean)(rec: A => F[A]): F[A]
    def recover[E, A](f: => F[Either[E, A]])(implicit recover: Recover[E]): F[Either[E, A]]
  }
  object ExecutionModel {

    trait AlreadyStackSafe[M[_]] extends ExecutionModel[M] {
      override def tailrec[A](start: A)(stop: A => Boolean)(rec: A => F[A]): F[A] = {
        def go(a: A): F[A] =
          if (stop(a))
            pure(a)
          else
            flatMap(rec(a))(go)
        go(start)
      }
    }

    implicit val Immediate: ExecutionModel[Id] =
      new ExecutionModel[Id] {
        override def pure   [A]   (a: A)             = a
        override def map    [A, B](a: A)(f: A => B) = f(a)
        override def flatMap[A, B](a: A)(f: A => B) = f(a)
        override def tailrec[A](start: A)(stop: A => Boolean)(rec: A => A): A = {
          @tailrec
          def go(a: A): A =
            if (stop(a))
              a
            else
              go(rec(a))
          go(start)
        }

        override def recover[E, A](f: => Either[E, A])(implicit recover: Recover[E]): Either[E, A] =
          recover.recover(f, Left(_))
      }

    import scala.concurrent._
    import scala.concurrent.duration._

    implicit def scalaFuture(implicit ec: ExecutionContext): ExecutionModel[Future] =
      new AlreadyStackSafe[Future] {
        override def pure   [A]   (a: A)                   = Future successful a
        override def map    [A, B](fa: F[A])(f: A => B)    = fa.map(f)
        override def flatMap[A, B](fa: F[A])(f: A => F[B]) = fa.flatMap(f)
        override def recover[E, A](f: => F[Either[E, A]])(implicit recover: Recover[E]): F[Either[E, A]] =
          recover.recover(
            f.recover { case t: Throwable => Left(recover apply t) },
            Future successful Left(_))
      }

    def toFuture(implicit ec: ExecutionContext): Id ~~> Future =
      new (Id ~~> Future) {
        override def apply[A](a: => A) = Future(a)
      }

    /*
    https://github.com/jducoeur/jsext/blob/master/src/main/scala/org/querki/jsext/RichFuture.scala

    def applyTimeout(duration: FiniteDuration)(implicit ec: ExecutionContext): Future ~~> Future =
      new (Future ~~> Future) {
        override def apply[A](a: => Future[A]) = {
          val promise = Promise[T]
          fut.onComplete {
            case Success(s) => promise.success(s)
            case Failure(f) => promise.failure(f)
          }
          setTimeout(duration) {
            if (!fut.isCompleted) {
              promise.failure(new TimeoutException(msg))
            }
          }
          promise.future
        }
      }
      */
  }

//  @inline implicit private[teststate] class ExecutionModelOps1[F[_], A](private val fa: F[A]) extends AnyVal {
//    @inline def mapEM[B](f: A => B)(implicit em: ExecutionModel[F]): F[B] =
//      em.map(fa)(f)
//    @inline def flatMapEM[B](f: A => F[B])(implicit em: ExecutionModel[F]): F[B] =
//      em.flatMap(fa)(f)
//  }
//  @inline implicit private[teststate] class ExecutionModelOps2[A](private val a: A) extends AnyVal {
//    @inline def pure[F](implicit em: ExecutionModel[F]): F[B] =
//      em.map(fa)(f)
//  }

  type Id[A] = A

  final case class Observe[-Ref, +Obs, +Err](val apply: Ref => Either[Err, Obs]) extends AnyVal
  object Observe {
    def Ops[R, O, E, Out](f: Observe[R, O, E] => Out): Ops[R, O, E, Out] =
      new Ops[R, O, E, Out](f)

    final class Ops[R, O, E, Out](private val observeOut: Observe[R, O, E] => Out) extends AnyVal {

      def observe(f: R => O): Out =
        observeTry(r => Right(f(r)))

      def observeTry(f: R => Either[E, O]): Out =
        observeOut(Observe(f))

    }
  }

}
