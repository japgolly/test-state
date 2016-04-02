package testate.typeclass

import acyclic.file

case class DisplayError[A](display: A => String) extends AnyVal

object DisplayError {
  implicit val displayErrorString: DisplayError[String] =
    DisplayError(identity)
}

