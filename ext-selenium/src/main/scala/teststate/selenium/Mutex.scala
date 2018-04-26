package teststate.selenium

trait Mutex {
  def apply[A](a: => A): A
}

object Mutex {
  def apply(): Mutex = {
    val lock = new AnyRef
    new Mutex {
      override def apply[A](a: => A): A =
        lock.synchronized(a)
    }
  }

  def blocking(): Mutex = {
    val lock = new AnyRef
    new Mutex {
      override def apply[A](a: => A): A =
        scala.concurrent.blocking(lock.synchronized(a))
    }
  }
}