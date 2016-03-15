package teststate

package object core {

  @inline implicit class TestStateCoreAnyExt[A](private val self: A) extends AnyVal {
    @inline def |>[B](f: A => B): B =
      f(self)
  }

}
