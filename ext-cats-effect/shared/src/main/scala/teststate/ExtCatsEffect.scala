package teststate

import cats.effect.IO
import java.time.{Duration => JavaDuration, Instant}
import scala.concurrent.duration._
import teststate.data.{Failure, Left, Or, Right}
import teststate.dsl.Dsl
import teststate.typeclass.{ErrorHandler, ExecutionModel}

trait ExtCatsEffect {

  implicit lazy val executionModelIO: ExecutionModel[IO] =
    new ExecutionModel[IO] {

      override def point[A](a: => A): IO[A] =
        IO.delay(a)

      override def pure[A](a: A): IO[A] =
        IO.pure(a)

      override def map[A, B](fa: IO[A])(f: A => B): IO[B] =
        fa map f

      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
        fa flatMap f

      override def tailrec[A, B](a: A)(rec: A => IO[Or[A, B]]): IO[B] = {
        def go(cur: A): IO[B] =
          IO.defer(rec(cur)).flatMap {
            case Left(next) => go(next)
            case Right(b)   => IO.pure(b)
          }
        go(a)
      }

      override def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => IO[A]): IO[A] = {
        def go(cur: A): IO[A] =
          IO.defer(if (stop(cur)) IO.pure(cur) else rec(cur).flatMap(go))
        go(a)
      }

      override def recover[E, A](f: => IO[Or[Failure[E], A]])(implicit attempt: ErrorHandler[E]): IO[Or[Failure[E], A]] =
        IO.defer(attempt.recover(f, e => IO.pure(Left(e))))
          .handleError(t => Left(attempt(t)))

      override val now: IO[Instant] =
        IO.delay(Instant.now())

      override def schedule[A](task: => IO[A], startAt: Instant): IO[A] =
        now.flatMap { n =>
          val ns = JavaDuration.between(n, startAt).toNanos
          if (ns <= 0)
            IO.defer(task)
          else
            IO.sleep(Duration(ns, NANOSECONDS)) >> IO.defer(task)
        }

      override def doFinally[A, B](main: => IO[A], last: => IO[B]): IO[A] =
        IO.defer(main).guarantee(IO.defer(last).void)
    }

  final implicit def toCatsEffectExtDsl(self: Dsl.type): ExtCatsEffect.CatsEffectExtDsl =
    new ExtCatsEffect.CatsEffectExtDsl(self)
}

object ExtCatsEffect extends ExtCatsEffect {

  final class CatsEffectExtDsl(private val self: Dsl.type) extends AnyVal {
    def io[R, O, S]: Dsl[IO, R, O, S, String] =
      self.full
  }
}
