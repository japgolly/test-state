package teststate.data

import acyclic.file

sealed abstract class Result[+Err] extends Product with Serializable {
  def failure: Option[Err]
  def +[e >: Err](r: Result[e]): Result[e]
}

object Result {
  def empty[E]: Result[E] = Skip

  case object Pass extends Result[Nothing] {
    override def failure = None
    override def +[e](r: Result[e]): Result[e] = r match {
      case Pass | Skip => Pass
      case Fail(_)     => r
    }
  }

  case object Skip extends Result[Nothing] {
    override def failure = None
    override def +[e](r: Result[e]): Result[e] = r
  }

  final case class Fail[+Err](error: Err) extends Result[Err] {
    override def failure = Some(error)
    override def +[e >: Err](r: Result[e]): Result[e] = this
  }
}
