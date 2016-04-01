package teststate.typeclass

import acyclic.file

case class Empty[+A](instance: A) extends AnyVal

object Empty {
  def instance[A](implicit e: Empty[A]): A =
    e.instance

//  trait ? {
//    def empty[A: Empty]: A = Empty.instance
//    def ∅[A: Empty]: A = Empty.instance
//  }
}
