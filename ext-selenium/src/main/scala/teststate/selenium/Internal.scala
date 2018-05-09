package teststate.selenium

import teststate.selenium.Tab.ProcMod

private[selenium] object Internal {

  val doNothing0 = () => ()

  val doNothing1 = (_: Any) => ()

  implicit class Fn0UnitExt[A](private val self: () => Unit) extends AnyVal {
    def >>(next: => Unit): () => Unit =
      () => {
        self()
        next
      }
  }

  implicit class AnyToUnitExt[A](private val self: A => Unit) extends AnyVal {
    def >>(next: A => Unit): A => Unit =
      a => {
        self(a)
        next(a)
      }
  }

  def mergeProcMods[A](o: Option[A => ProcMod], f: A => ProcMod): A => ProcMod =
    o.fold(f)(o => a => o(a).andThen(f(a)))

}
