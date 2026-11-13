package teststate.typeclass

import japgolly.microlibs.name_fn._
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
        try Name.now(f(o).value)
        catch { case _: Throwable => name(f, None) }
      case None =>
        Name.now(
          try f(None).value
          catch { case t: Throwable => "Name exception: " + toE(t) })
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
      val errStr = err.toString
      val raw = stackTrace(err)
      val traceOnly =
        if (raw.startsWith(errStr))
          raw.substring(errStr.length).dropWhile(c => c == '\r' || c == '\n')
        else
          raw
      val lines = if (traceOnly.isEmpty) Nil else traceOnly.split('\n').toList
      val st = stackTraceMod(lines).mkString("\n").trim
      if (st.isEmpty)
        errStr
      else
        s"$errStr\n$st"
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
