package teststate.selenium

import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import teststate.data.Id
import teststate.typeclass.ExecutionModel

/** Access to a specific tab in a browser.
  *
  * Ensure that you call `.withSeleniumTab` on your TestState DSL when using this.
  */
trait Tab[+D <: WebDriver] { underlying =>

  /** Focus the tab and perform action within it */
  def use[A](f: D => A): A

  /** Monadic version of [[use()]].
    *
    * @param lockWait Maximum time to wait attempting to acquire the tab lock.
    * @param lockRetry If the lock cannot be acquired, how long to wait before trying again.
    */
  def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                   (implicit EM: ExecutionModel[M]): M[A]

  /** Close the tab.
    *
    * @return whether this call was effective.
    *         Specifically, true if this call closed the tab, false if the tab was already closed.
    */
  def closeTab(): Boolean

  def withAroundFirstUse(around: D => Tab.ProcMod): Tab[D]

  final def withBeforeFirstUse(f: D => Unit): Tab[D] =
    withAroundFirstUse(d => Tab.ProcMod.before(f(d)))

  final def withAfterFirstUse(f: D => Unit): Tab[D] =
    withAroundFirstUse(d => Tab.ProcMod.after(f(d)))

  final def withBeforeEachUse(f: D => Unit): Tab[D] =
    withAroundEachUse(d => Tab.ProcMod.before(f(d)))

  final def withAfterEachUse(f: D => Unit): Tab[D] =
    withAroundEachUse(d => Tab.ProcMod.after(f(d)))

  final def withAroundEachUse(around: D => Tab.ProcMod): Tab[D] =
    new Tab[D] {

      override def use[A](f: D => A): A =
        underlying.use { d =>
          val m = around(d)
          val g = m[Id, A](() => f(d))
          g()
        }

      override def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)(implicit EM: ExecutionModel[M]): M[A] = {
        val f2: D => M[A] = { d =>
          val m = around(d)
          val g = m(() => f(d))
          g()
        }
        underlying.useM(f2, lockWait, lockRetry)
      }

      override def withAroundFirstUse(around: D => Tab.ProcMod): Tab[D] =
        underlying.withAroundFirstUse(around)

      override def closeTab(): Boolean =
        underlying.closeTab()
    }

  final def withOnClose(callback: => Any): Tab[D] =
    new Tab[D] {

      override def use[A](f: D => A): A =
        underlying.use(f)

      override def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                                (implicit EM: ExecutionModel[M]): M[A] =
        underlying.useM(f, lockWait, lockRetry)(EM)

      override def withAroundFirstUse(around: D => Tab.ProcMod): Tab[D] =
        underlying.withAroundFirstUse(around)

      override def closeTab(): Boolean = {
        val wasEffective = underlying.closeTab()
        if (wasEffective)
          callback
        wasEffective
      }
    }
}

object Tab {

  def apply[D <: WebDriver](driver: D,
                            mutex: Mutex,
                            tabSupport: TabSupport[D],
                            onFirstUse: Option[D => ProcMod] = None)
                           (rootTab: tabSupport.TabHandle): Tab[D] =
    new Tab[D] {
      import tabSupport.TabHandle

      private var tab: Option[TabHandle] = None
      private var closed = false

      private def prepareWithoutLocking(): D = {
        implicit def d = driver

        if (closed)
          throw new TabAlreadyClosed()

        tab match {
          case Some(t) =>
            tabSupport.activate(t)
          case None =>
            val t = tabSupport.open(rootTab)
            tab = Some(t)
            tabSupport.activate(t)
            onFirstUse.foreach { f =>
              val m = f(d)
              val g = m[Id, Unit](doNothing)
              g()
            }
        }

        d
      }

      override def use[A](f: D => A): A =
        mutex(f(prepareWithoutLocking()))

      override def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                                (implicit EM: ExecutionModel[M]): M[A] = {
        def driver = EM.point(prepareWithoutLocking())
        mutex.monadic(EM.flatMap(driver)(f), lockWait, lockRetry)
      }

      override def withAroundFirstUse(around: D => Tab.ProcMod): Tab[D] = {
        val f: D => ProcMod = onFirstUse.fold(around)(o => d => o(d).andThen(around(d)))
        Tab(driver, mutex, tabSupport, Some(f))(rootTab)
      }

      override def closeTab(): Boolean =
        mutex {
          val affect = !closed
          if (affect) {
            implicit def d = driver
            for (t <- tab) {
              tabSupport.activate(t)
              tabSupport.closeActive()
            }
            closed = true
            tab = None
          }
          affect
        }
    }

  private[this] val doNothing = () => ()

  trait ProcMod { outer =>
    def apply[F[_], A](proc: () => F[A])(implicit EM: ExecutionModel[F]): () => F[A]

    final def andThen(inner: ProcMod): ProcMod =
      new ProcMod {
        override def apply[F[_], A](proc: () => F[A])(implicit EM: ExecutionModel[F]): () => F[A] =
          outer(inner(proc))
      }
  }

  object ProcMod {
    def before(f: => Unit): ProcMod =
      new ProcMod {
        override def apply[F[_], A](proc: () => F[A])(implicit EM: ExecutionModel[F]): () => F[A] = () => {
          def before = EM.point(f)
          EM.flatMap(before)(_ => proc())
        }
      }

    def after(f: => Unit): ProcMod =
      new ProcMod {
        override def apply[F[_], A](proc: () => F[A])(implicit EM: ExecutionModel[F]): () => F[A] = () => {
          def after = EM.point(f)
          EM.flatMap(proc())(a => EM.map(after)(_ => a))
        }
      }

  }
}

final class TabAlreadyClosed() extends RuntimeException("You cannot use a tab after you've closed it.")
