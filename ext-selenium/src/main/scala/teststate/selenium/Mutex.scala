package teststate.selenium

import java.util.concurrent.{Semaphore, TimeUnit}
import scala.concurrent.duration.Duration
import teststate.typeclass.ExecutionModel

final class Mutex {
  private val semaphore = new Semaphore(1)
  private var owner: Thread = null
  private var holds: Int = 0

  private def tryReenter(): Boolean = synchronized {
    val t = Thread.currentThread()
    if (owner eq t) {
      holds += 1
      true
    } else
      false
  }

  private def onAcquired(t: Thread): Unit = synchronized {
    owner = t
    holds = 1
  }

  def lock(): Unit = {
    if (!tryReenter()) {
      semaphore.acquire()
      onAcquired(Thread.currentThread())
    }
  }

  def tryLock(wait: Duration): Boolean =
    tryLock(wait, Thread.currentThread())

  private def tryLock(wait: Duration, ownerIfAcquired: Thread): Boolean =
    if (tryReenter())
      true
    else if (semaphore.tryAcquire(wait.toMillis, TimeUnit.MILLISECONDS)) {
      onAcquired(ownerIfAcquired)
      true
    } else
      false

  def unlock(): Unit = synchronized {
    holds match {
      case 0 => throw new IllegalMonitorStateException()
      case 1 =>
        owner = null
        holds = 0
        semaphore.release()
      case n =>
        holds = n - 1
    }
  }

  def apply[A](a: => A): A = {
    lock()
    try a finally unlock()
  }

  def monadic[M[_], A](ma: => M[A], wait: Duration, retry: Duration)
                      (implicit EM: ExecutionModel[M]): M[A] = {

    def unlockM: M[Unit] = EM.point(unlock())

    def go: M[A] =
      EM.flatten(
        EM.point {
          if (tryLock(wait, null))
            EM.doFinally(ma, unlockM)
          else
            EM.flatMap(EM.now)(now => EM.schedule(go, now.plusMillis(retry.toMillis)))
        }
      )

    go
  }
}

object Mutex {
  def apply(): Mutex =
    new Mutex
}
