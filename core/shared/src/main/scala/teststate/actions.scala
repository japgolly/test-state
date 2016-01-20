package teststate

sealed trait Action[-S1, -O1, S2, -O2, +Err] {
  def nonCompositeActions: Vector[Action.NonComposite[S1, O1, S2, O2, Err]]

  final def >>[s1 <: S1, o1 <: O1, o2 <: O2, e >: Err](next: Action[s1, o1, S2, o2, e]): Action.Composite[s1, o1, S2, o2, e] =
    Action.Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen[s1 <: S1, o1 <: O1, o2 <: O2, e >: Err](next: Action[s1, o1, S2, o2, e]) =
    this >> next
}

object Action {

  def empty[S2] = Composite[Any, Any, S2, Any, Nothing](Vector.empty)

  sealed trait NonComposite[-S1, -O1, S2, -O2, +Err] extends Action[S1, O1, S2, O2, Err] {
    def name: Option[(S1, O1)] => String

    final override def nonCompositeActions: Vector[NonComposite[S1, O1, S2, O2, Err]] =
      vector1(this)

    final def times(n: Int): Group[S1, O1, S2, O2, Err] =
      Group(i => s"${name(i)} ($n times)", Iterator.fill(n)(this).foldLeft(empty: Action[S1, O1, S2, O2, Err])(_ >> _))
  }

  case class Composite[-S1, -O1, S2, -O2, +Err](nonCompositeActions: Vector[NonComposite[S1, O1, S2, O2, Err]])
    extends Action[S1, O1, S2, O2, Err] {

    def group(name: String): Group[S1, O1, S2, O2, Err] =
      Group(_ => name, this)

    def times(n: Int, name: String) =
      group(name).times(n)
  }

  case class Group[-S1, -O1, S2, -O2, +Err](name: Option[(S1, O1)] => String,
                                            action: Action[S1, O1, S2, O2, Err])
    extends NonComposite[S1, O1, S2, O2, Err]

  case class Single[-S1, -O1, S2, -O2, +Err](name: Option[(S1, O1)] => String,
                                             run: (S1, O1) => Option[() => Either[Err, O2 => S2]],
                                             checks: Checks[S1, O1, S2, O2, Err])
    extends NonComposite[S1, O1, S2, O2, Err]

}
