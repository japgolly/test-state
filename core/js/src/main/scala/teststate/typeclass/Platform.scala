package teststate.typeclass

private[typeclass] object Platform {

  def threadSleep(timeMs: Long): Unit =
    System.err.print("Thread.sleep() unavailable on JavaScript.")

}