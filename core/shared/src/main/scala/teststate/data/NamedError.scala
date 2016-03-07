package teststate.data

import acyclic.file

case class NamedError[+E](name: Name, error: E)