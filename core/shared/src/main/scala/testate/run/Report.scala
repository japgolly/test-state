package testate.run

// import acyclic.file
import scala.annotation.elidable
import scala.util.control.NonFatal
import testate.data.{Failure, Name}
import testate.typeclass.DisplayError
import Report._

case class Report[+E](name: Option[Name], history: History[Failure[E]], stats: Stats) {

  @inline def failure = history.failure
  @inline def failed = history.failed
  @inline def result = history.result

  def failureReason[e >: E](implicit displayError: DisplayError[e]): Option[Failure[String]] =
    failure.map(_ map (err =>
      (history.rootFailurePath.lastOption, Option(displayError display err).filter(_.nonEmpty)) match {
        case (Some(f), Some(e)) => s"${f.name.value} -- $e"
        case (Some(f), None   ) => f.name.value
        case (None   , Some(e)) => e
        case (None   , None   ) => "Failed without an error message."
      }
    ))

  @elidable(elidable.ASSERTION)
  def assert[EE >: E]()(implicit as: AssertionSettings, se: DisplayError[EE]): Unit =
    failureReason(se) match {

      case None =>
        as.onPass.print[EE](this)

      case Some(fe) =>
        as.onFail.print[EE](this)

        throw fe.cause match {
          case Some(NonFatal(e)) =>
            e

          case Some(e) =>
            // Because UndefinedBehaviourErrors (and presumably other fatal errors) freeze Scala.JS
            val x = new RuntimeException(e.getMessage)
            x.setStackTrace(e.getStackTrace)
            x

          case None =>
            new AssertionError(fe.failure)
        }
    }

  def format[EE >: E](implicit as: AssertionSettings, s: DisplayError[EE]): String =
    format[EE](if (failed) as.onFail else as.onPass)(s)

  def format[EE >: E](f: Format)(implicit s: DisplayError[EE]): String =
    f.format[EE](this)(s) getOrElse ""
}

object Report {

  // Help keep Exports small

  type History[+E] = testate.run.History[E]
  val  History     = testate.run.History

  type Format = testate.run.ReportFormat
  val  Format = testate.run.ReportFormat

  type Stats = testate.run.Stats
  val  Stats = testate.run.Stats


  case class AssertionSettings(onPass: Format, onFail: Format) {
    def silenceOnPass: AssertionSettings =
      copy(onPass = Format.quiet)
  }

  object AssertionSettings {
    def uniform(format: Format): AssertionSettings =
      AssertionSettings(format, format)

    def uncoloured = AssertionSettings(
      onPass = Format.Default.uncoloured.apply,
      onFail = Format.Default.uncoloured.alwaysShowChildren.apply)

    def coloured = AssertionSettings(
      onPass = Format.Default.coloured.apply,
      onFail = Format.Default.coloured.alwaysShowChildren.apply)
  }
}