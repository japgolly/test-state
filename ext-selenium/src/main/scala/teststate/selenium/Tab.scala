package teststate.selenium

import org.openqa.selenium.WebDriver
import scala.concurrent.duration.Duration
import teststate.data.Id
import teststate.selenium.Internal._
import teststate.typeclass.ExecutionModel

/** Access to a specific tab in a browser.
  *
  * Ensure that you call `.withSeleniumTab` on your TestState DSL when using this.
  *
  * Note: This is mutable.
  */
trait Tab[+D <: WebDriver] {

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

  def aroundFirstUse(around: D => Tab.ProcMod): this.type
  def aroundEachUse (around: D => Tab.ProcMod): this.type
  def beforeClose   (callback: D => Unit): this.type
  def afterClose    (callback: D => Unit): this.type
  def afterClosed   (callback: => Unit): this.type

  final def beforeFirstUse(f: D => Unit): this.type = aroundFirstUse(d => Tab.ProcMod.before(f(d)))
  final def afterFirstUse (f: D => Unit): this.type = aroundFirstUse(d => Tab.ProcMod.after (f(d)))
  final def beforeEachUse (f: D => Unit): this.type = aroundEachUse (d => Tab.ProcMod.before(f(d)))
  final def afterEachUse  (f: D => Unit): this.type = aroundEachUse (d => Tab.ProcMod.after (f(d)))
}

object Tab {

  def apply[D <: WebDriver](driver: D,
                            mutex: Mutex,
                            tabSupport: TabSupport[D])
                           (rootTab: tabSupport.TabHandle): Tab[D] =
    new Tab[D] {
      import tabSupport.TabHandle

      private var tab         = Option.empty[TabHandle]
      private var closed      = false
      private var onFirstUse  = Option.empty[D => ProcMod]
      private var onEachUse   = Option.empty[D => ProcMod]
      private var beforeClose = (doNothing1: D => Unit)
      private var afterClose  = (doNothing1: D => Unit)
      private var afterClosed = doNothing0

      private def prepareWithoutLocking(): (D, Option[D => ProcMod]) = {
        implicit def d: D = driver

        if (closed)
          throw new TabAlreadyClosed()

        val firstMod: Option[D => ProcMod] =
          tab match {
            case Some(t) =>
              tabSupport.activate(t)
              None
            case None =>
              val t = tabSupport.open(rootTab)
              // No need to call tabSupport.activate(t) here because tabSupport.open() already activates the new tab
              tab = Some(t)
              onFirstUse
          }

        val mod: Option[D => ProcMod] =
          (firstMod, onEachUse) match {
            case (Some(f), Some(e)) => Some(d => f(d).andThen(e(d)))
            case (Some(_), None)    => firstMod
            case (None, Some(_))    => onEachUse
            case (None, None)       => None
          }

        (d, mod)
      }

      private def runWithMod[M[_], A](d: D, mod: Option[D => ProcMod], f: D => M[A])
                                     (implicit EM: ExecutionModel[M]): M[A] =
        mod match {
          case None =>
            f(d)
          case Some(around) =>
            val m = around(d)
            val g = m(() => f(d))
            g()
        }

      override def use[A](f: D => A): A =
        mutex {
          val (d, mod) = prepareWithoutLocking()
          runWithMod[Id, A](d, mod, f)
        }

      override def useM[M[_], A](f: D => M[A], lockWait: Duration, lockRetry: Duration)
                                (implicit EM: ExecutionModel[M]): M[A] = {
        def prepared = EM.point(prepareWithoutLocking())
        mutex.monadic(EM.flatMap(prepared) { case (d, mod) => runWithMod(d, mod, f) }, lockWait, lockRetry)
      }

      override def closeTab(): Boolean = {
        // Close the tab with lock held
        val callback = mutex {
          val affect = !closed
          if (affect) {
            implicit def d: D = driver
            for (t <- tab) {
              if (beforeClose ne doNothing1) {
                tabSupport.activate(t)
                beforeClose(d)
              }
              tabSupport.activate(t)
              tabSupport.closeActive()
            }
            closed = true
            tab = None
            afterClose(d)
            afterClosed
          } else
            null
        }

        // Run the afterClosed callback without the browser lock
        if (callback eq null)
          false
        else {
          if (callback ne doNothing0)
            callback()
          true
        }
      }

      override def aroundFirstUse(around: D => Tab.ProcMod): this.type =
        mutex {
          if (tab.isDefined)
            sys.error("Attempting to add a {before,after,around}FirstUse callback to a tab after its first use.")
          onFirstUse = Some(mergeProcMods(onFirstUse, around))
          this
        }

      override def aroundEachUse(around: D => Tab.ProcMod): this.type =
        mutex {
          onEachUse = Some(mergeProcMods(onEachUse, around))
          this
        }

      override def beforeClose(callback: D => Unit): this.type =
        mutex {
          beforeClose = beforeClose >> callback
          this
        }

      override def afterClose(callback: D => Unit): this.type =
        mutex {
          afterClose = afterClose >> callback
          this
        }

      override def afterClosed(callback: => Unit): this.type =
        mutex {
          afterClosed = afterClosed >> callback
          this
        }
  }

  // ===================================================================================================================

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
