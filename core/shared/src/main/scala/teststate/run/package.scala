package teststate

import acyclic.file

package object run {
  @inline private[teststate] def vector1[A](a: A): Vector[A] =
    Vector.empty[A] :+ a
}
