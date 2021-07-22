package teststate.run

import scala.annotation.elidable
import scala.util.control.NonFatal
import teststate.data.{Failure, Name}
import teststate.typeclass.DisplayError
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

  @deprecated("Use .assert(true)", "2.2.0")
  @elidable(elidable.ASSERTION)
  def assertF[EE >: E]()(implicit as: AssertionSettings, se: DisplayError[EE]): Unit =
    assert[EE]()(as.withFailSettingsOnPass, se)

  @elidable(elidable.ASSERTION)
  def assert[EE >: E](useFailSettingsOnPass: Boolean = false)
                     (implicit as: AssertionSettings, de: DisplayError[EE]): Unit = {
    print[EE](useFailSettingsOnPass)
    exception[EE].foreach(throw _)
  }

  /** Print the report to stdout */
  def print[EE >: E](useFailSettingsOnPass: Boolean = false)
                    (implicit as: AssertionSettings, de: DisplayError[EE]): Unit = {
    val fmt = if (useFailSettingsOnPass || failed) as.onFail else as.onPass
    fmt.print[EE](this)
  }

  def exception[EE >: E](implicit de: DisplayError[EE]): Option[Throwable] =
    failureReason(de) map { fe =>
      fe.cause match {
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
    format[EE]()

  def format[EE >: E](useFailSettingsOnPass: Boolean = false)
                     (implicit as: AssertionSettings, de: DisplayError[EE]): String =
    format[EE](if (failed || useFailSettingsOnPass) as.onFail else as.onPass)(de)

  def format[EE >: E](f: Format)(implicit de: DisplayError[EE]): String =
    f.format[EE](this)(de) getOrElse ""

  @deprecated("Use .format(true)", "2.2.0")
  def formatF[EE >: E](implicit as: AssertionSettings, s: DisplayError[EE]): String =
    format(as.withFailSettingsOnPass, s)
}

object Report {

  // Help keep Exports small

  type History[+E] = teststate.run.History[E]
  val  History     = teststate.run.History

  type Format = teststate.run.ReportFormat
  val  Format = teststate.run.ReportFormat

  type Stats = teststate.run.Stats
  val  Stats = teststate.run.Stats


  final case class AssertionSettings(onPass: Format, onFail: Format) {
    def silenceOnPass: AssertionSettings =
      copy(onPass = Format.quiet)

    @deprecated("Use withFailSettingsOnPass", "2.2.0")
    def failSettingsOnPass: AssertionSettings =
      copy(onPass = onFail)

    def withFailSettingsOnPass: AssertionSettings =
      copy(onPass = onFail)
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

    def default = coloured
  }
}