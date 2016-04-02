package testate

import acyclic.file

package object run {
  @inline private[testate] def vector1[A](a: A): Vector[A] =
    Vector.empty[A] :+ a
}
