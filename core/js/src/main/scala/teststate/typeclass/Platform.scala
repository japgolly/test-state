package teststate.typeclass

import scala.annotation.nowarn

private[typeclass] object Platform {

  @nowarn("cat=unused")
  def threadSleep(timeMs: Long): Unit =
    System.err.print("Thread.sleep() unavailable on JavaScript.")

}