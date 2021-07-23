package teststate

import japgolly.microlibs.name_fn._
import teststate.data.BeforeAfter

package object core {

  @inline implicit class TestStateCoreAnyExt[A](private val self: A) extends AnyVal {
    @inline def |>[B](f: A => B): B =
      f(self)
  }

  @inline implicit class TestStateNFEBA[A](private val f: NameFn[BeforeAfter[A]] => NameFn[BeforeAfter[A]]) extends AnyVal {
    def thruBefore: NameFn[A] => NameFn[A] =
      n => f(n.cmap(_.before)).cmap(BeforeAfter.same)

    def thruAfter: NameFn[A] => NameFn[A] =
      n => f(n.cmap(_.before)).cmap(BeforeAfter.same)
  }
}
