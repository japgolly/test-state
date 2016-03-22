package teststate.typeclass

import acyclic.file
import scala.annotation.tailrec
import teststate.data._

trait ExecutionModel[M[_]] {
  final type F[A] = M[A]
  def point[A](a: => A): F[A]
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def tailrec[A](a: A)(stop: A => Boolean)(rec: A => F[A]): F[A]
  def recover[E, A](f: => F[E Or A])(implicit recover: Recover[E]): F[E Or A]

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)
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
      override def point  [A]   (a: => A)         = a
      override def pure   [A]   (a: A)            = a
      override def map    [A, B](a: A)(f: A => B) = f(a)
      override def flatMap[A, B](a: A)(f: A => B) = f(a)
      override def flatten[A]   (a: A)            = a
      override def tailrec[A](start: A)(stop: A => Boolean)(rec: A => A): A = {
        @tailrec
        def go(a: A): A =
          if (stop(a))
            a
          else
            go(rec(a))
        go(start)
      }

      override def recover[E, A](f: => E Or A)(implicit recover: Recover[E]): E Or A =
        recover.recover(f, Left(_))
    }

  import scala.concurrent._
  import scala.concurrent.duration._

  implicit def scalaFuture(implicit ec: ExecutionContext): ExecutionModel[Future] =
    new AlreadyStackSafe[Future] {
      override def point  [A]   (a: => A)                = Future(a)
      override def pure   [A]   (a: A)                   = Future successful a
      override def map    [A, B](fa: F[A])(f: A => B)    = fa.map(f)
      override def flatMap[A, B](fa: F[A])(f: A => F[B]) = fa.flatMap(f)
      override def recover[E, A](f: => F[E Or A])(implicit recover: Recover[E]): F[E Or A] =
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

