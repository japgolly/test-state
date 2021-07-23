package teststate.data

import japgolly.microlibs.name_fn._

case class NamedError[+E](name: Name, error: E) {
  def map[F](f: E => F): NamedError[F] =
    NamedError(name, f(error))
}

