package teststate

sealed trait Action[State, Obs, Err] {
  def nonCompositeActions: Vector[Action.NonComposite[State, Obs, Err]]

  final def >>(next: Action[State, Obs, Err]): Action.Composite[State, Obs, Err] =
    Action.Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen(next: Action[State, Obs, Err]) =
    this >> next
}

object Action {

  def empty[State, Obs, Err]: Action[State, Obs, Err] =
    Composite(Vector.empty)

  sealed trait NonComposite[State, Obs, Err] extends Action[State, Obs, Err] {
    def name: Option[(State, Obs)] => String

    final override def nonCompositeActions: Vector[NonComposite[State, Obs, Err]] =
      vector1(this)

    final def times(n: Int): Group[State, Obs, Err] =
      Group(i => s"${name(i)} ($n times)", Iterator.fill(n)(this).foldLeft(Action.empty[State, Obs, Err])(_ >> _))
  }

  case class Composite[State, Obs, Err](nonCompositeActions: Vector[NonComposite[State, Obs, Err]])
    extends Action[State, Obs, Err] {

    def group(name: String): Group[State, Obs, Err] =
      Group(_ => name, this)

    def times(n: Int, name: String) =
      group(name).times(n)
  }

  case class Group[State, Obs, Err](name: Option[(State, Obs)] => String,
                                    action: Action[State, Obs, Err])
    extends NonComposite[State, Obs, Err]

  case class Single[State, Obs, Err](name: Option[(State, Obs)] => String,
                                     run: (State, Obs) => Option[() => Either[Err, Obs => State]],
                                     checks: Checks[State, Obs, Err])
    extends NonComposite[State, Obs, Err]
}
