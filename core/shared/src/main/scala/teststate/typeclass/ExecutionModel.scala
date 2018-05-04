package teststate.typeclass

import acyclic.file
import java.time.Instant
import java.util.{Date, Timer, TimerTask}
import scala.annotation.tailrec
import scala.concurrent._
import teststate.data._

trait ExecutionModel[M[_]] {
  final type F[A] = M[A]
  def point[A](a: => A): F[A]
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def tailrec[A, B](a: A)(rec: A => F[A Or B]): F[B]
  def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => F[A]): F[A]
  def recover[E, A](f: => F[Failure[E] Or A])(implicit attempt: Attempt[E]): F[Failure[E] Or A]
  def now: F[Instant]
  def schedule[A](task: => F[A], startAt: Instant): F[A]
  def doFinally[A, B](main: => F[A], last: => F[B]): F[A]

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)
}

object ExecutionModel {

  trait AlreadyStackSafe[M[_]] extends ExecutionModel[M] {
    def tailrec[A, B](start: A)(rec: A => F[A Or B]): F[B] = {
      def go(e: A Or B): F[B] =
        e match {
          case Left(a) => flatMap(rec(a))(go)
          case Right(b) => pure(b)
        }
      go(Left(start))
    }
    override def tailrecA[A](start: A)(stop: A => Boolean)(rec: A => F[A]): F[A] = {
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
      override def now                            = Instant.now()

      override def doFinally[A, B](main: => A, last: => B): A =
        try main finally {last; ()}

      def tailrec[A, B](start: A)(rec: A => A Or B): F[B] = {
        @tailrec
        def go(e: A Or B): B =
          e match {
            case Left(a) => go(rec(a))
            case Right(b) => b
          }
        go(Left(start))
      }

      override def tailrecA[A](start: A)(stop: A => Boolean)(rec: A => A): A = {
        @tailrec
        def go(a: A): A =
          if (stop(a))
            a
          else
            go(rec(a))
        go(start)
      }

      override def recover[E, A](f: => Failure[E] Or A)(implicit attempt: Attempt[E]): Failure[E] Or A =
        attempt.recover(f, Left(_))

      override def schedule[A](task: => A, startAt: Instant): A = {
        val waitTime = startAt.toEpochMilli - Instant.now().toEpochMilli
        if (waitTime > 0)
          Platform.threadSleep(waitTime)
        task
      }
    }

  implicit def scalaFuture(implicit ec: ExecutionContext): ExecutionModel[Future] =
    new AlreadyStackSafe[Future] {
      import scala.util.{Failure => ScalaFailure, Success => ScalaSuccess}

      private val timer = new Timer(true)

      override def point  [A]   (a: => A)                = Future(a)
      override def pure   [A]   (a: A)                   = Future successful a
      override def map    [A, B](fa: F[A])(f: A => B)    = fa.map(f)
      override def flatMap[A, B](fa: F[A])(f: A => F[B]) = fa.flatMap(f)

      override def recover[E, A](f: => F[Failure[E] Or A])(implicit attempt: Attempt[E]): F[Failure[E] Or A] =
        attempt.recover(
          f.recover { case t: Throwable => Left(attempt(t)) },
          Future successful Left(_))

      override def now =
        Future successful Instant.now()

      override def schedule[A](task: => F[A], startAt: Instant) = {
        val promise = Promise[A]()
        val timerTask = new TimerTask {
          override def run(): Unit =
            ec.execute(new Runnable {
              override def run(): Unit = {
                promise.completeWith(task)
                ()
              }
            })
        }
        timer.schedule(timerTask, new Date(startAt.toEpochMilli))
        promise.future
      }

      override def doFinally[A, B](main: => Future[A], last: => Future[B]): Future[A] = {

        // Scala 2.12+
        // main.transformWith(ta => last.transform {
        //   case ScalaSuccess(_) => ta
        //   case ScalaFailure(t) => ScalaFailure(t)
        // })

        val p = Promise[A]()
        val fa = main
        fa.onComplete { ta =>
          val fb = last
          fb.onComplete {
            case ScalaSuccess(_) => p.complete(ta)
            case ScalaFailure(f) => p.failure(f)
          }
        }
        p.future
      }
    }

  def toFuture(implicit ec: ExecutionContext): Id ~~> Future =
    new (Id ~~> Future) {
      override def apply[A](a: => A) = Future(a)
    }
}
