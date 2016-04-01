package teststate.run

import acyclic.file
import scala.concurrent.duration._
import Stats._

case class Stats(performedActions: Int,
                 performedChecks: Int,
                 totalTime: FiniteDuration) {
  def +(that: Stats): Stats =
    Stats(
      this.performedActions + that.performedActions,
      this.performedChecks  + that.performedChecks,
      this.totalTime        + that.totalTime)
}

object Stats {

  def empty = Stats(0, 0, FiniteDuration(0, NANOSECONDS))

  class Mutable {
    var startTime, endTime = 0L
    def startTimer(): Unit = startTime = System.nanoTime()
    def stopTimer(): Unit = endTime = System.nanoTime()

    var actions, checks = 0

    def result() = Stats(
      actions,
      checks,
      FiniteDuration(endTime - startTime, NANOSECONDS))
  }

  /*
  case class Tally(passed: Int, skipped: Int, failed: Int) {
    def +(b: Tally): Tally =
      Tally(
        passed  + b.passed,
        skipped + b.skipped,
        failed  + b.failed)
  }

  class Mutable {
    var startTime, endTime = 0L
    def startTimer(): Unit = startTime = System.nanoTime()
    def stopTimer(): Unit = endTime = System.nanoTime()

    var actionTally, checkTally = new MutableTally

    def result() = Stats(
      actionTally.result(),
      checkTally.result(),
      FiniteDuration(endTime - startTime, NANOSECONDS))
  }
  */
}
