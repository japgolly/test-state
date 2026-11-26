package teststate

import java.time.{Duration => JavaDuration, Instant}
import teststate.data.{Failure, Left, Or, Right}
import teststate.dsl.Dsl
import teststate.typeclass.{ErrorHandler, ExecutionModel}
import zio.{Clock, Exit, Task, ZIO}

trait ExtZio {

  implicit lazy val executionModelTask: ExecutionModel[Task] =
    new ExecutionModel[Task] {

      override def point[A](a: => A): Task[A] =
        ZIO.attempt(a)

      override def pure[A](a: A): Task[A] =
        ZIO.succeed(a)

      override def map[A, B](fa: Task[A])(f: A => B): Task[B] =
        fa map f

      override def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
        fa flatMap f

      override def tailrec[A, B](a: A)(rec: A => Task[Or[A, B]]): Task[B] = {
        def go(cur: A): Task[B] =
          ZIO.suspend(rec(cur)).flatMap {
            case Left(next) => go(next)
            case Right(b)   => ZIO.succeed(b)
          }
        go(a)
      }

      override def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => Task[A]): Task[A] = {
        def go(cur: A): Task[A] =
          ZIO.suspend(if (stop(cur)) ZIO.succeed(cur) else rec(cur).flatMap(go))
        go(a)
      }

      override def recover[E, A](f: => Task[Or[Failure[E], A]])(implicit attempt: ErrorHandler[E]): Task[Or[Failure[E], A]] =
        ZIO.suspendSucceed(attempt.recover(f, e => ZIO.succeed(Left(e))))
          .catchAll(t => ZIO.succeed(Left(attempt(t))))
          .catchAllDefect(t => ZIO.succeed(Left(attempt(t))))

      override val now: Task[Instant] =
        Clock.instant

      override def schedule[A](task: => Task[A], startAt: Instant): Task[A] =
        now.flatMap { n =>
          val d = JavaDuration.between(n, startAt)
          if (d.isZero || d.isNegative)
            ZIO.suspend(task)
          else
            ZIO.sleep(d) *> ZIO.suspend(task)
        }

      override def doFinally[A, B](main: => Task[A], last: => Task[B]): Task[A] =
        ZIO.uninterruptibleMask { restore =>
          restore(ZIO.suspend(main)).exit.flatMap { exit =>
            ZIO.suspend(last).exit.flatMap { lastExit =>
              (exit, lastExit) match {
                case (Exit.Success(_), Exit.Failure(c))   => ZIO.refailCause(c)
                case (Exit.Failure(c1), Exit.Failure(c2)) => ZIO.refailCause(c1 ++ c2)
                case _                                    => ZIO.suspendSucceed(exit)
              }
            }
          }
        }
    }

  final implicit def toZioExtDsl(self: Dsl.type): ExtZio.ZioExtDsl =
    new ExtZio.ZioExtDsl(self)
}

object ExtZio extends ExtZio {

  final class ZioExtDsl(private val self: Dsl.type) extends AnyVal {
    def task[R, O, S]: Dsl[Task, R, O, S, String] =
      self.full
  }
}
