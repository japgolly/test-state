package teststate

sealed trait Action[-Ref, -O, S, +Err] {
  def nonCompositeActions: Vector[Action.NonComposite[Ref, O, S, Err]]

  final def >>[r <: Ref, o <: O, e >: Err](next: Action[r, o, S, e]): Action.Composite[r, o, S, e] =
    Action.Composite(nonCompositeActions ++ next.nonCompositeActions)

  final def andThen[r <: Ref, o <: O, e >: Err](next: Action[r, o, S, e]) =
    this >> next
}

case class ROS[+Ref, +Obs, +State](ref: Ref, obs: Obs, state: State) {
  val sos: Some[(Obs, State)] =
    Some((obs, state))
}

object Action {

  def empty[S] = Composite[Any, Any, S, Nothing](Vector.empty)

  sealed trait NonComposite[-Ref, -O, S, +Err] extends Action[Ref, O, S, Err] {

    def name: Option[(O, S)] => String

    def nameMod(f: (=> String) => String): NonComposite[Ref, O, S, Err]

    def when[r <: Ref, o <: O](f: ROS[r, o, S] => Boolean): NonComposite[r, o, S, Err] = this

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
                                      action: Action[Ref, O, S, Err]) extends NonComposite[Ref, O, S, Err] {

    override def nameMod(f: (=> String) => String): Group[Ref, O, S, Err] =
      copy(name = o => f(name(o)))
  }

  case class Single[-Ref, -O, S, +Err](name: Option[(O, S)] => String,
                                       run: ROS[Ref, O, S] => Option[() => Either[Err, O => S]],
                                       checks: Checks[O, S, Err]) extends NonComposite[Ref, O, S, Err] {

    override def nameMod(f: (=> String) => String): Single[Ref, O, S, Err] =
      copy(name = o => f(name(o)))
  }


  // ===================================================================================================================

}


class ActionBuilder[R, O, S, E] {

  type Name = Option[(O, S)] => String
  type ROS = teststate.ROS[R, O, S]
  type ActionRun = ROS => Option[() => Either[E, O => S]]

  def action(name: String) = new B1(_ => name)

  class B1(name: Name) {

    def act(f: ROS => Unit) =
      actTry(f.andThen(_ => None))

    def actTry(f: ROS => Option[E]) =
      new B2(name, Some(f))

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

  class B2(name: Name, act: Option[ROS => Option[E]]) {
    def expect(nextState: S => O => S) =
      new B3(name, i => Some(() =>
        act.flatMap(_ apply i)
          .leftOr(nextState(i.state))
      ))

    def expect2(f: S => Either[E, S]) =
      new B3(name, i => Some(() =>
        act.flatMap(_ apply i)
          .leftBind(f(i.state).map(Function.const))
      ))

  }

  class B3(name: Name, act: ActionRun) {
    def build = Action.Single(name, act, Checks.empty)
  }

    /*
    class Loop(name: Name, act: (R, O, S) => Unit) {
      def act(f: R => Unit)
    }
    */

  /*
    def expect(S|O => S)
    def expect(S => E ∨ S)
    def expect(S => E ∨ (O→S))

    def when|unless(R|O|S => Boolean)

    def check(c)
   */
}