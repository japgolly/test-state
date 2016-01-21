package teststate

sealed trait Action[-Ref, -O1, -S1, -O2, S2, +Err] {
  def nonCompositeActions: Vector[Action.NonComposite[Ref, O1, S1, O2, S2, Err]]

  final def >>[r <: Ref, o1 <: O1, s1 <: S1, o2 <: O2, e >: Err](next: Action[r, o1, s1, o2, S2, e]): Action.Composite[r, o1, s1, o2, S2, e] =
    Action.Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen[r <: Ref, o1 <: O1, s1 <: S1, o2 <: O2, e >: Err](next: Action[r, o1, s1, o2, S2, e]) =
    this >> next
}

object Action {

  def empty[S2] = Composite[Any, Any, Any, Any, S2, Nothing](Vector.empty)

  sealed trait NonComposite[-Ref, -O1, -S1, -O2, S2, +Err] extends Action[Ref, O1, S1, O2, S2, Err] {
    def name: Option[(O1, S1)] => String

    def nameMod(f: (=> String) => String): NonComposite[Ref, O1, S1, O2, S2, Err]

    final override def nonCompositeActions: Vector[NonComposite[Ref, O1, S1, O2, S2, Err]] =
      vector1(this)

    final def times(n: Int): Group[Ref, O1, S1, O2, S2, Err] =
      Group(i => s"${name(i)} ($n times)",
        (1 to n).iterator
          .map(i => nameMod(s => s"[$i/$n] $s"))
          .foldLeft(empty: Action[Ref, O1, S1, O2, S2, Err])(_ >> _))
  }

  case class Composite[-Ref, -O1, -S1, -O2, S2, +Err](nonCompositeActions: Vector[NonComposite[Ref, O1, S1, O2, S2, Err]])
    extends Action[Ref, O1, S1, O2, S2, Err] {

    def group(name: String): Group[Ref, O1, S1, O2, S2, Err] =
      Group(_ => name, this)

    def times(n: Int, name: String) =
      group(name).times(n)
  }

  case class Group[-Ref, -O1, -S1, -O2, S2, +Err](name: Option[(O1, S1)] => String,
                                                  action: Action[Ref, O1, S1, O2, S2, Err])
    extends NonComposite[Ref, O1, S1, O2, S2, Err] {

    override def nameMod(f: (=> String) => String) =
      copy(name = o => f(name(o)))
  }

  case class Single[-Ref, -O1, -S1, -O2, S2, +Err](name: Option[(O1, S1)] => String,
                                                   run: (Ref, O1, S1) => Option[() => Either[Err, O2 => S2]],
                                                   checks: Checks[O1, S1, O2, S2, Err])
    extends NonComposite[Ref, O1, S1, O2, S2, Err] {

    override def nameMod(f: (=> String) => String) =
      copy(name = o => f(name(o)))
  }


  // ===================================================================================================================

}

class ActionBuilder[R, O, S1, S2, E] {

  case class ROS

  def action(name: String) = new HasName(_ => name)

  class HasName(name: Option[(O, S1)] => String) {
//    def act(f: R => Unit)
    /*
    def tmp(act: R => Unit, alterState: S1 => S2) =
      Action.Single[R, O, S1, O, S2, E](name, (r, o, s) =>
        Some(() => {
          act(r)
          Right(_ => alterState(s))
        }), Checks.empty)
*/
  }

  /*
  class Loop(name: Option[(O, S1)] => String, act: (R, O, S1) => Unit) {
    def act(f: R => Unit)

  }
  */

  /*
    def action(name: String)

    def act(R|O|S => Unit)
    def act(R|O|S => Option[E])

    def expect(S1|O2 => S2)
    def expect(S1 => E ∨ S2)
    def expect(S1 => E ∨ (O2→S2))

    def when|unless(R|O|S => Boolean)

    def check(c)

   */
}
