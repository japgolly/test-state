package testate.data

import acyclic.file

case class NamedError[+E](name: Name, error: E) {
  def map[F](f: E => F): NamedError[F] =
    NamedError(name, f(error))
}

