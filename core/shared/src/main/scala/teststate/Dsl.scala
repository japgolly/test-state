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

  AROUND
  ======

  1. (O, S) => Either[E, A] | A
  2. (A, A) => Option[E]
  3.
   */

  def check(name: String) = new FocusDsl[O, S, E](name)
}

case class Equal[A](equal: (A, A) => Boolean)
object Equal {

  def byUnivEq[A]: Equal[A] = Equal(_ == _)

  implicit val equalString: Equal[String] = byUnivEq
  implicit val equalInt: Equal[Int] = byUnivEq

}

trait SomethingFailures[-AA, +E] {
  def expectedEqual      [A <: AA](expected: A, actual: A)         (implicit s: Show[A]): E
  def expectedToChange   [A <: AA](a: A)                           (implicit s: Show[A]): E
  def expectedChange     [A <: AA](from: A, expected: A, actual: A)(implicit s: Show[A]): E

  final def expectEqual[A <: AA](expected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      None
    else
      Some(expectedEqual(expected = expected, actual = actual))

  final def expectNotEqual[A <: AA](expected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      Some(expectedToChange(actual))
    else
      None

  final def expectChange[A <: AA](from: A, expected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(expected, actual))
      None
    else if (e.equal(from, actual))
      Some(expectedToChange(actual))
    else
      Some(expectedChange(from = from, expected = expected, actual = actual))
}
object SomethingFailures {
  implicit object ToString extends SomethingFailures[Any, String] {
    def expectedEqual      [A](expected: A, actual: A)         (implicit s: Show[A]) = s"Expected ${s(expected)}, not ${s(actual)}."
    def expectedToChange   [A](a: A)                           (implicit s: Show[A]) = s"Expected ${s(a)} to change, but it didn't."
    def expectedChange     [A](from: A, expected: A, actual: A)(implicit s: Show[A]) = s"Expected ${s(from)} to change into ${s(expected)}, not ${s(actual)}."
  }
}

class FocusDsl[O, S, E](focusName: String) {
  def obs  (implicit s: Show[O], e: Equal[O]) = apply((o, _) => o)
  def state(implicit s: Show[S], e: Equal[S]) = apply((_, s) => s)

  def   obsTo[A: Show: Equal](f: O => A) = apply((o, _) => f(o))
  def stateTo[A: Show: Equal](f: S => A) = apply((_, s) => f(s))

  def apply[A: Show: Equal](f: (O, S) => A) =
    new B1((o, s) => Right(f(o, s)))

  abstract class AroundOps[A, Out <: AroundOps[A, Out]](extract: (O, S) => Either[E, A])(implicit sa: Show[A], eq: Equal[A]) {

    protected def add(in: Check.Around.Single[O, S, E]): Out

    final def build(name: Option[(O, S)] => String, t: (A, A) => Option[E]): Out =
      add(Check.Around.Single(
        name,
        extract,
        (o, s, a: A) => extract(o, s) match {
          case Right(b) => t(a, b)
          case Left(e) => Some(e)
        }))

    final def assertBefore(expect: A)(implicit f: SomethingFailures[A, E]): Out =
      build(
        _ => focusName + " should start as " + sa.show(expect),
        (before, _) => f.expectEqual(expected = expect, before))

    final def assertAfter(expect: A)(implicit f: SomethingFailures[A, E]): Out =
      build(
        _ => focusName + " should end as " + sa.show(expect),
        (_, after) => f.expectEqual(expected = expect, after))

    final def assert(before: A, after: A)(implicit f: SomethingFailures[A, E]): Out =
      assertBefore(before).assertAfter(after)

    final def assertChange(desc: String, expect: A => A)(implicit f: SomethingFailures[A, E]): Out =
      build(
        _ => focusName + " change: " + desc,
        (before, after) => f.expectChange(from = before, expected = expect(before), actual = after))

    final def assertChanges(implicit f: SomethingFailures[A, E]): Out =
      build(
        _ => focusName + " should change",
        f.expectNotEqual(_, _))

    final def assertDoesntChange(implicit f: SomethingFailures[A, E]): Out =
      build(
        _ => focusName + " shouldn't change",
        f.expectEqual(_, _))
  }

  class B1[A](extract: (O, S) => Either[E, A])(implicit sa: Show[A], eq: Equal[A]) extends AroundOps[A, B2[A]](extract) {
    protected def add(c: Check.Around.Single[O, S, E]) =
      new B2(c, extract)
  }

  class B2[A](val check: Check.Around[O, S, E], extract: (O, S) => Either[E, A])(implicit sa: Show[A], eq: Equal[A]) extends AroundOps[A, B2[A]](extract) {
    protected def add(c: Check.Around.Single[O, S, E]) =
      new B2(check & c, extract)
  }
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