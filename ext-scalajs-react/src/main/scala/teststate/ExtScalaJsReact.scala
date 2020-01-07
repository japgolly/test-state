package teststate

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import java.time.Instant
import teststate.data.{Failure, Or}
import teststate.typeclass.{ErrorHandler, ExecutionModel}

trait ExtScalaJsReact extends domzipper.Exports {
  import ExtScalaJsReact._

  final implicit def toReactExtHtmlScrubObject(a: HtmlScrub.type): ReactExtHtmlScrubObject =
    new ReactExtHtmlScrubObject(a)

  final implicit def toExtScalaJsReactCompExt(m: GenericComponent.MountedRaw): ExtScalaJsReactCompExt =
    new ExtScalaJsReactCompExt(m)

  implicit override val htmlScrub: HtmlScrub =
    HtmlScrub.default >> HtmlScrub.removeReactInternals

//  lazy val executionModalCallback: ExecutionModel[CallbackTo] =
//    new ExecutionModel[CallbackTo] {
//      override def point[A](a: => A): F[A] =
//        CallbackTo(a)
//
//      override def pure[A](a: A): F[A] =
//        CallbackTo.pure(a)
//
//      override def map[A, B](fa: F[A])(f: A => B): F[B] =
//        fa.map(f)
//
//      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
//        fa.flatMap(f)
//
//      override def tailrec[A, B](a: A)(rec: A => F[Or[A, B]]): F[B] =
//        CallbackTo.tailrec(a)(rec.andThen(_.map(_.toEither)))
//
//      override def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => F[A]): F[A] =
//        CallbackTo.tailrec(a)(a => if (stop(a)) CallbackTo.pure(Right(a)) else rec(a).map(Left(_)))
//
//      override def recover[E, A](f: => F[Or[Failure[E], A]])(implicit attempt: ErrorHandler[E]): F[Or[Failure[E], A]] =
//        ???
//
//      override val now: F[Instant] =
//        CallbackTo(Instant.now())
//
//      override def schedule[A](task: => F[A], startAt: Instant): F[A] =
//        ???
//
//      override def doFinally[A, B](main: => F[A], last: => F[B]): F[A] =
//        main.finallyRun(last)
//    }

  lazy val executionModalAsyncCallback: ExecutionModel[AsyncCallback] =
    new ExecutionModel[AsyncCallback] {
      override def point[A](a: => A): F[A] =
        AsyncCallback.point(a)

      override def pure[A](a: A): F[A] =
        AsyncCallback.pure(a)

      override def map[A, B](fa: F[A])(f: A => B): F[B] =
        fa.map(f)

      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
        fa.flatMap(f)

      override def tailrec[A, B](a: A)(rec: A => F[Or[A, B]]): F[B] =
        AsyncCallback.tailrec(a)(rec.andThen(_.map(_.toEither)))

      override def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => F[A]): F[A] =
        AsyncCallback.tailrec(a)(a => if (stop(a)) AsyncCallback.pure(Right(a)) else rec(a).map(Left(_)))

      override def recover[E, A](f: => F[Or[Failure[E], A]])(implicit attempt: ErrorHandler[E]): F[Or[Failure[E], A]] =
        f.map(o => attempt.recover(o, teststate.data.Left(_)))

      override val now: F[Instant] =
        AsyncCallback.point(Instant.now())

      override def schedule[A](task: => F[A], startAt: Instant): F[A] =
        now.flatMap { n =>
          val d = startAt.toEpochMilli - n.toEpochMilli
          task.delayMs(d)
        }

      override def doFinally[A, B](main: => F[A], last: => F[B]): F[A] =
        main.finallyRun(last)
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ExtScalaJsReact extends ExtScalaJsReact {

  final class ReactExtHtmlScrubObject(private val self: HtmlScrub.type) extends AnyVal {
    @deprecated("Use .removeReactInternals", "2.1.2") def removeReactDataAttr: HtmlScrub = removeReactInternals

    def removeReactInternals: HtmlScrub =
      HtmlScrub(ReactTestUtils.removeReactInternals)
  }

  final class ExtScalaJsReactCompExt(private val m: GenericComponent.MountedRaw) extends AnyVal {
    def domZipper(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJs =
      DomZipperJs(m.displayName, ReactDOM.findDOMNode(m.raw).get.asElement)($, scrub)
  }
}
