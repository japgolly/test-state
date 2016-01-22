package teststate

object Dsl {
  @inline def apply[R, O, S, E] = new Dsl[R, O, S, E]
}

class Dsl[R, O, S, E] {
  private type Name = Option[(O, S)] => String
  private type ROS = teststate.ROS[R, O, S]
  private type ActionRun = ROS => Option[() => Either[E, O => S]]

  // ===================================================================================================================

  def action(name: String) = new A1(_ => name)

  class A1(name: Name) {

    def act(f: ROS => Unit) =
      actTry(f.andThen(_ => None))

    def actTry(f: ROS => Option[E]) =
      new A2(name, Some(f))

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

  class A2(name: Name, act: Option[ROS => Option[E]]) {
    def updateState(nextState: S => S) =
      updateStateO(s => _ => nextState(s))

    def updateStateO(nextState: S => O => S) =
      build(i => Some(() =>
        act.flatMap(_ apply i)
          .leftOr(nextState(i.state))
      ))

    def updateState2(f: S => Either[E, S]) =
      build(i => Some(() =>
        act.flatMap(_ apply i)
          .leftOrF(f(i.state).map(Function.const))
      ))

    def noStateUpdate =
      updateStateO(s => _ => s)

    private def build(act: ActionRun) =
      Action.Single(name, act, Check.Around.empty)
  }

//  class A3(name: Name, act: ActionRun) {
//    def build = Action.Single(name, act, Check.Around.empty)
//  }

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

  // ===================================================================================================================

  //def checkPoint(name: String) = new P1(_ => name)

/*
  compare: (A, A) => Boolean
  assertChange
  assertNoChange
  assert(before, after)

  assertChangeBy

  // a.focus(locked_?) .assertBefore(true).assertAfter(false)
}
 */
}

// =====================================================================================================================
// =====================================================================================================================
// =====================================================================================================================

object DslNoS {
  @inline def apply[R, O, E] = new DslNoS[R, O, E]
}

class DslNoS[R, O, E] {
  private type S = Unit
  private type Name = Option[(O, S)] => String
  private type ROS = teststate.ROS[R, O, S]
  private type ActionRun = ROS => Option[() => Either[E, O => S]]

  // ===================================================================================================================

  def action(name: String) = new A1(_ => name)

  class A1(name: Name) {
    def act(f: ROS => Unit) =
      actTry(f.andThen(_ => None))

    def actTry(f: ROS => Option[E]) =
      Action.Single[R, O, Unit, E](name, i => Some(() => f(i).leftOr(_ => ())), Check.Around.empty)

  }
}