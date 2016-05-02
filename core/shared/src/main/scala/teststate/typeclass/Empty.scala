package teststate.typeclass

import acyclic.file

case class Empty[+A](instance: A) extends AnyVal

object Empty {
  def instance[A](implicit e: Empty[A]): A =
    e.instance

  final class EmptyOptionOps[A](private val o: Option[A]) extends AnyVal {
    def orEmpty(implicit e: Empty[A]): A =
      o getOrElse e.instance
  }

  trait Ops {
    // Require Empty here to prevent all type of Options getting the op
    implicit def toEmptyOptionOps[A: Empty](o: Option[A]): EmptyOptionOps[A] =
      new EmptyOptionOps(o)
  }
}
