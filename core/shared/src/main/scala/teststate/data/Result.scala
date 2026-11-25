package teststate.data

import japgolly.univeq.UnivEq
import scala.annotation.nowarn

sealed abstract class Result[+Err] extends Product with Serializable {
  def failure: Option[Err]
  def failed: Boolean
  def +[e >: Err](r: Result[e]): Result[e]
}

object Result {
  def empty[E]: Result[E] = Skip

  case object Pass extends Result[Nothing] {
    override def failure = None
    override def failed = false
    override def +[e](r: Result[e]): Result[e] = r match {
      case Pass | Skip => Pass
      case Fail(_)     => r
    }
  }

  case object Skip extends Result[Nothing] {
    override def failure = None
    override def failed = false
    override def +[e](r: Result[e]): Result[e] = r
  }

  final case class Fail[+Err](error: Err) extends Result[Err] {
    override def failure = Some(error)
    override def failed = true
    override def +[e >: Err](r: Result[e]): Result[e] = this
  }

  @nowarn("cat=unused")
  implicit def univEq[E: UnivEq]: UnivEq[Result[E]] = UnivEq.derive
}
