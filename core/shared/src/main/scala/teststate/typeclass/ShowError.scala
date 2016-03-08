package teststate.typeclass

import acyclic.file

case class ShowError[A](show: A => String) extends AnyVal

object ShowError {
  implicit val showErrorString: ShowError[String] =
    ShowError(identity)
}

