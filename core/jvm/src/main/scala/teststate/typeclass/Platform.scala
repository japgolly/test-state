package teststate.typeclass

private[typeclass] object Platform {

  def threadSleep(timeMs: Long): Unit =
    Thread.sleep(timeMs)

}