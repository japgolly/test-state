package teststate.selenium

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.duration.Duration
import teststate.typeclass.ExecutionModel

final case class Mutex(lock: ReentrantLock) extends AnyVal {

  def apply[A](a: => A): A = {
    lock.lockInterruptibly()
    try a finally lock.unlock()
  }

  def monadic[M[_], A](ma: => M[A], wait: Duration, retry: Duration)
                               (implicit EM: ExecutionModel[M]): M[A] = {

    def unlock: M[Unit] = EM.point(lock.unlock())

    def go: M[A] =
      EM.flatten(
        EM.point {
          if (lock.tryLock(wait.toMillis, TimeUnit.MILLISECONDS))
            EM.doFinally(ma, unlock)
          else
            EM.flatMap(EM.now)(now => EM.schedule(go, now.plusMillis(retry.toMillis)))
        }
      )

    go
  }
}

object Mutex {
  def apply(): Mutex =
    new Mutex(new ReentrantLock)
}