package teststate

sealed trait Action[-O1, -S1, -O2, S2, +Err] {
  def nonCompositeActions: Vector[Action.NonComposite[O1, S1, O2, S2, Err]]

  final def >>[o1 <: O1, s1 <: S1, o2 <: O2, e >: Err](next: Action[o1, s1, o2, S2, e]): Action.Composite[o1, s1, o2, S2, e] =
    Action.Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen[o1 <: O1, s1 <: S1, o2 <: O2, e >: Err](next: Action[o1, s1, o2, S2, e]) =
    this >> next
}

object Action {

  def empty[S2] = Composite[Any, Any, Any, S2, Nothing](Vector.empty)

  sealed trait NonComposite[-O1, -S1, -O2, S2, +Err] extends Action[O1, S1, O2, S2, Err] {
    def name: Option[(O1, S2)] => String

    final override def nonCompositeActions: Vector[NonComposite[O1, S1, O2, S2, Err]] =
      vector1(this)

    final def times(n: Int): Group[O1, S1, O2, S2, Err] =
      Group(i => s"${name(i)} ($n times)", Iterator.fill(n)(this).foldLeft(empty: Action[O1, S1, O2, S2, Err])(_ >> _))
  }

  case class Composite[-O1, -S1, -O2, S2, +Err](nonCompositeActions: Vector[NonComposite[O1, S1, O2, S2, Err]])
    extends Action[O1, S1, O2, S2, Err] {

    def group(name: String): Group[O1, S1, O2, S2, Err] =
      Group(_ => name, this)

    def times(n: Int, name: String) =
      group(name).times(n)
  }

  case class Group[-O1, -S1, -O2, S2, +Err](name: Option[(O1, S2)] => String,
                                            action: Action[O1, S1, O2, S2, Err])
    extends NonComposite[O1, S1, O2, S2, Err]

  case class Single[-O1, -S1, -O2, S2, +Err](name: Option[(O1, S2)] => String,
                                             run: (O1, S2) => Option[() => Either[Err, O2 => S2]],
                                             checks: Checks[O1, S1, O2, S2, Err])
    extends NonComposite[O1, S1, O2, S2, Err]

}
