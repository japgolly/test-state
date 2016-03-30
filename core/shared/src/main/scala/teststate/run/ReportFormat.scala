package teststate.run

import scala.Console._
import scala.concurrent.duration.FiniteDuration
import teststate.data.{Failure, Result}
import teststate.typeclass.ShowError
import History.{Step, Steps}
import Result.{Fail, Skip, Pass}

trait ReportFormat {
  def format[E](report: Report[E])(implicit se: ShowError[E]): Option[String]

  def print[E](report: Report[E])(implicit se: ShowError[E]): Unit =
    format(report) foreach println
}

object ReportFormat {

  val quiet: ReportFormat =
    new ReportFormat {
      override def format[E](report: Report[E])(implicit se: ShowError[E]) =
        None
    }

  class Default(s: Default.Settings) extends ReportFormat {
    override def format[E](report: Report[E])(implicit se: ShowError[E]) = {
      val sb = new StringBuilder

      def appendIndent(indent: Int): Unit = {
        var i = indent
        while (i > 0) {
          sb append s.indent
          i -= 1
        }
      }

      def appendResultFlag(r: Result[Any]): Unit = {
        sb append (r match {
          case Pass    => s.onPass
          case Skip    => s.onSkip
          case Fail(_) => s.onFail
        })
        ()
      }

      def showHistory(h: History[Failure[E]], indent: Int): Unit =
        showSteps(h.steps, indent)

      def showSteps(steps: Steps[Failure[E]], indent: Int): Unit =
        steps foreach (showStep(_, indent))

      def showStep(step: Step[Failure[E]], indent: Int): Unit = {
        val showChildren = step.children.steps.nonEmpty && s.showChildren(step.children)
        val error = if (showChildren) None else step.result.failure

        appendIndent(indent)
        appendResultFlag(step.result)
        sb append ' '
        sb append step.name.value
        for (err <- error) {
          val e = se.show(err.failure)
          if (e.nonEmpty) {
            sb append " -- "
            sb append e
          }
        }
        sb append s.eol

        if (showChildren)
          showHistory(step.children, indent + 1)
      }

      report.name match {
        case None =>
          showHistory(report.history, 0)
        case Some(n) =>
          appendResultFlag(report.result)
          sb append ' '
          sb append n.value
          sb append s.eol
          showHistory(report.history, 1)
      }

      if (report.history.result != Skip) {
//      if (report.history.nonEmpty) { TODO Stop adding summaries in Runner
//        sb append "  "
        s.stats.run(sb, report.stats)
        sb append s.eol
      }

      Some(sb.result())
    }
  }

  object Default {

    case class StatsFormat(run: (StringBuilder, Stats) => Unit) extends AnyVal {
      def before(f: (StringBuilder, Stats) => Any): StatsFormat =
        StatsFormat { (sb, s) =>
          f(sb, s)
          run(sb, s)
        }

      def after(f: (StringBuilder, Stats) => Any): StatsFormat =
        StatsFormat { (sb, s) =>
          run(sb, s)
          f(sb, s)
          ()
        }

      def before(f: StringBuilder => Any): StatsFormat =
        before((sb, _) => f(sb))

      def after(f: StringBuilder => Any): StatsFormat =
        after((sb, _) => f(sb))
    }

    object StatsFormat {

      def formatDuration(secPrec: Int): FiniteDuration => String = {
        val secfmt = "%." + secPrec + "f sec"
        d =>
          if (d.toMicros == 0)
            d.toNanos + " μs"
          else if (d.toSeconds == 0)
            d.toMillis + " ms"
          else
            secfmt.format(d.toMicros.toDouble / 1000000)
      }

      val sec1 = formatDuration(1)

      /*
      case class Settings(pass: Int => String,
                          skip: Int => String,
                          fail: Int => String,
                          time: FiniteDuration => String)

      val uncoloured = Settings(_.toString, _.toString, _.toString, formatDuration(1))

      val coloured = Settings(
        pass = BOLD + GREEN + _ + RESET + WHITE,
        skip = BOLD + YELLOW + _ + BLACK,
        fail = RED + _ + BOLD,
        time = uncoloured.time)
        */

      private def units(sb: StringBuilder, n: Int, u: String) = {
        sb append n
        sb append ' '
        sb append u
        if (n != 1)
          sb append 's'
        ()
      }

      val _withoutTime: StatsFormat =
        StatsFormat { (sb, s) =>
          sb append "Performed "
          units(sb, s.performedActions, "action")
          sb append ", "
          units(sb, s.performedChecks, "check")
          ()
        }

      val withoutTime: StatsFormat =
        _withoutTime.after(_ append '.')

      val onlyTime: StatsFormat =
        StatsFormat { (sb, s) =>
          sb append "Took "
          sb append sec1(s.totalTime)
          sb append '.'
          ()
        }

      val default: StatsFormat =
        StatsFormat { (sb, s) =>
          _withoutTime.run(sb, s)
          sb append " in "
          sb append sec1(s.totalTime)
          sb append '.'
          ()
        }
    }

    case class Settings(indent      : String,
                        onPass      : String,
                        onSkip      : String,
                        onFail      : String,
                        eol         : String,
                        showChildren: History[Any] => Boolean,
                        stats       : StatsFormat) {

      def alwaysShowChildren =
        copy(showChildren = _ => true)

      def onlyShowFailedChildren =
        copy(showChildren = _.failure.isDefined)

      def apply: ReportFormat =
        new Default(this)
    }

    implicit def settingsToReportFormat(s: Settings): ReportFormat =
      s.apply

    val uncoloured = Settings(
      indent       = "  ",
      onPass       = "✓", // ✓ ✔
      onSkip       = "-", // ⇣ ↶ ↷
      onFail       = "✘", // ✗ ✘
      eol          = "\n",
      showChildren = _.failure.isDefined,
      stats        = StatsFormat.default)

    val coloured = Settings(
      indent       = "  ",
      onPass       = BOLD + GREEN + uncoloured.onPass + RESET + WHITE,
      onSkip       = BOLD + YELLOW + uncoloured.onSkip + BLACK,
      onFail       = RED + uncoloured.onFail + BOLD,
      eol          = RESET + uncoloured.eol,
      showChildren = uncoloured.showChildren,
      stats        = uncoloured.stats.before(_ append WHITE))
  }
}
