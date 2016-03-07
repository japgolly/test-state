package teststate.typeclass

import acyclic.file
import teststate.data._

final case class Recover[E](apply: Throwable => E) extends AnyVal {
  def recover[A](a: => A, ko: E => A): A =
    try a catch { case t: Throwable => ko(apply(t)) }

  def attempt[A](a: => A): E Or A =
    recover(Right(a), Left(_))

  def map[EE](f: E => EE): Recover[EE] =
    Recover(f compose apply)

  def name[A](f: NameFn[A], a: Some[A]): String =
    attempt(f(a).value) match {
      case Right(n) => n
      case Left(_) => attempt(f(None).value) match {
        case Right(n) => n
        case Left(e) => "Name exception: " + e.toString
      }
    }
}

object Recover {
  implicit val recoverToString: Recover[String] =
    Recover { t =>
      // TODO Yay? Nay?
      /*
      val o = System.err
      o.println()
      t.printStackTrace(o)
      o.println()
      */
      "Caught exception: " + t.toString
    }
}

