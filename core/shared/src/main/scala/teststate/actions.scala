package teststate

sealed trait Action[-Ref, -O, S, +Err] {
  def nonCompositeActions: Vector[Action.NonComposite[Ref, O, S, Err]]

  final def >>[r <: Ref, o <: O, e >: Err](next: Action[r, o, S, e]): Action.Composite[r, o, S, e] =
    Action.Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen[r <: Ref, o <: O, e >: Err](next: Action[r, o, S, e]) =
    this >> next
}

object Action {

  def empty[S] = Composite[Any, Any, S, Nothing](Vector.empty)

  sealed trait NonComposite[-Ref, -O, S, +Err] extends Action[Ref, O, S, Err] {
    def name: Option[(O, S)] => String

    def nameMod(f: (=> String) => String): NonComposite[Ref, O, S, Err]

    final override def nonCompositeActions: Vector[NonComposite[Ref, O, S, Err]] =
      vector1(this)

    final def times(n: Int): Group[Ref, O, S, Err] =
      Group(i => s"${name(i)} ($n times)",
        (1 to n).iterator
          .map(i => nameMod(s => s"[$i/$n] $s"))
          .foldLeft(empty: Action[Ref, O, S, Err])(_ >> _))
  }

  case class Composite[-Ref, -O, S, +Err](nonCompositeActions: Vector[NonComposite[Ref, O, S, Err]])
    extends Action[Ref, O, S, Err] {

    def group(name: String): Group[Ref, O, S, Err] =
      Group(_ => name, this)

    def times(n: Int, name: String) =
      group(name).times(n)
  }

  case class Group[-Ref, -O, S, +Err](name: Option[(O, S)] => String,
                                      action: Action[Ref, O, S, Err])
    extends NonComposite[Ref, O, S, Err] {

    override def nameMod(f: (=> String) => String) =
      copy(name = o => f(name(o)))
  }

  case class Single[-Ref, -O, S, +Err](name: Option[(O, S)] => String,
                                       run: (Ref, O, S) => Option[() => Either[Err, O => S]],
                                       checks: Checks[O, S, Err])
    extends NonComposite[Ref, O, S, Err] {

    override def nameMod(f: (=> String) => String) =
      copy(name = o => f(name(o)))
  }


  // ===================================================================================================================

}

class ActionBuilder[R, O, S, E] {


  def action(name: String) = new HasName(_ => name)

  class HasName(name: Option[(O, S)] => String) {
//    def act(f: R => Unit)
    /*
    def tmp(act: R => Unit, alterState: S => S) =
      Action.Single[R, O, S, E](name, (r, o, s) =>
        Some(() => {
          act(r)
          Right(_ => alterState(s))
        }), Checks.empty)
*/
  }

  /*
  class Loop(name: Option[(O, S)] => String, act: (R, O, S) => Unit) {
    def act(f: R => Unit)

  }
  */

  /*
    def action(name: String)

    def act(R|O|S => Unit)
    def act(R|O|S => Option[E])

    def expect(S|O => S)
    def expect(S => E ∨ S)
    def expect(S => E ∨ (O→S))

    def when|unless(R|O|S => Boolean)

    def check(c)

   */
}
