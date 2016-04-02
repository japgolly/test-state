package testate.typeclass

import acyclic.file
import testate.data._

final case class Recover[+E](toE: Throwable => E) extends AnyVal {
  def map[EE](f: E => EE): Recover[EE] =
    Recover(f compose toE)

  def apply(t: Throwable): Failure.WithCause[E] =
    Failure.WithCause(toE(t), t)

  def recover[A](a: => A, ko: Failure.WithCause[E] => A): A =
    try a catch { case t: Throwable => ko(apply(t)) }

  def attempt[A](a: => A): Failure.WithCause[E] Or A =
    recover(Right(a), Left(_))

  def name[A](f: NameFn[A], a: Some[A]): Name = {
    val name: String =
      attempt(f(a).value) match {
        case Right(n) => n
        case Left(_)  => attempt(f(None).value) match {
          case Right(n) => n
          case Left(e)  => "Name exception: " + e.toString
        }
      }
    Name now name
  }
}

object Recover {

  val id: Recover[Throwable] =
    Recover(identity)

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
