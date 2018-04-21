package teststate.typeclass

import scala.concurrent.blocking

private[typeclass] object Platform {

  def threadSleep(timeMs: Long): Unit =
    blocking(Thread.sleep(timeMs))

}