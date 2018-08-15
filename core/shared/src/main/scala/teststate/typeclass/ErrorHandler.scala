package teststate.typeclass

import acyclic.file
import java.io.{PrintStream, PrintWriter, StringWriter}
import java.util.regex.Pattern
import teststate.data._

final case class ErrorHandler[+E](toE: Throwable => E) extends AnyVal {
  def map[EE](f: E => EE): ErrorHandler[EE] =
    ErrorHandler(f compose toE)

  def apply(t: Throwable): Failure.WithCause[E] =
    Failure.WithCause(toE(t), t)

  def recover[A](a: => A, ko: Failure.WithCause[E] => A): A =
    try a catch { case t: Throwable => ko(apply(t)) }

  def attempt[A](a: => A): Failure.WithCause[E] Or A =
    recover(Right(a), Left(_))

  def name[A](f: NameFn[A], o: Option[A]): Name =
    o match {
      case Some(_) =>
        attempt(f(o).value) match {
          case Right(n) => Name now n
          case Left(_)  => name(f, None)
        }
      case None =>
        Name.now(
          attempt(f(None).value) match {
            case Right(n) => n
            case Left(e)  => "Name exception: " + e.toString
          })
    }
}

object ErrorHandler {

  val id: ErrorHandler[Throwable] =
    ErrorHandler(identity)

  val byToString: ErrorHandler[String] =
    ErrorHandler(_.toString)

  def toStringWithStackTrace: ErrorHandler[String] =
    toStringWithStackTrace(l => l)

  def toStringWithStackTrace(pattern: Pattern): ErrorHandler[String] =
    toStringWithStackTrace(_.filter(pattern.matcher(_).find()).map(_.trim))

  def toStringWithStackTrace(stackTraceMod: List[String] => List[String]): ErrorHandler[String] =
    ErrorHandler { err =>
      val st = stackTraceMod(stackTrace(err).split('\n').toList).mkString("\n").trim
      if (st.isEmpty)
        err.toString
      else
        s"$err\n$st"
    }

  def stackTrace(t: Throwable): String = {
    val sw = new StringWriter()
    try {
      val pw = new PrintWriter(sw)
      try {
        t.printStackTrace(pw)
        sw.toString
      } finally pw.close()
    } finally sw.close()
  }

  def printFirst[A](r: ErrorHandler[A], stream: PrintStream = System.err): ErrorHandler[A] =
    ErrorHandler { t =>
      stream.println()
      t.printStackTrace(stream)
      stream.println()
      r.toE(t)
    }
}
