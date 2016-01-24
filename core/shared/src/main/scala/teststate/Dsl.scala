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

  def focus(name: String) = new FocusDsl[O, S, E](name)

  def bifocus[A: Show: Equal](name: String, fo: O => A, fs: S => A) =
    new BiFocusDsl[O, S, E, A](name, fo, fs)

//  def compareStateAndObs[A](name: String, fo: O => A, fs: S => A)(implicit ) =
//    new FocusDsl[O, S, E](name)

  def point(name: Option[(O, S)] => String, test: (O, S) => Option[E]) =
    Check.Point.Single(name, test)

  def around[A](name: Option[(O, S)] => String, before: (O, S) => Either[E, A])(test: (O, S, A) => Option[E]) =
    Check.Around.Single(name, before, test)
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

  final def expectNotEqual[A <: AA](unexpected: A, actual: A)(implicit s: Show[A], e: Equal[A]): Option[E] =
    if (e.equal(unexpected, actual))
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

class BiFocusDsl[O, S, E, A](focusName: String, fo: O => A, fs: S => A)(implicit sa: Show[A], eq: Equal[A]) {

  private def point(name: Option[(O, S)] => String, t: (A, A) => Option[E]) =
    Check.Point.Single[O, S, E](name, (o, s) =>
      t(fo(o), fs(s)))
      //fo(o).toOptionLeft(oa => t(oa, fs(s))))

  def obs   = new FocusDsl[O, S, E](focusName).obsTo(fo)
  def state = new FocusDsl[O, S, E](focusName).stateTo(fs)

  def assertEqual(implicit f: SomethingFailures[A, E]) =
    point(
      focusName + " = " + _.fold("<state>")(os => sa(fs(os._2))),
      (o, s) => f.expectEqual(expected = s, actual = o))

//  def test(desc: String => String, t: A => Boolean)(implicit ev: String =:= E): OutP =
//    test(desc, t, ev compose sa.show)

  // TODO Create case class for (obs,state) - use instead of tuple in ros.sos - use here too
//  def test(desc: String => String, t: (A, A) => Boolean, error: (A, A) => E) =
//    point(
//      Function const desc(focusName),
//      (o, s) => if (t(o, s)) None else Some(error(o, s)))
}


class FocusDsl[O, S, E](focusName: String) {
  def obs  (implicit s: Show[O], e: Equal[O]) = apply((o, _) => o)
  def state(implicit s: Show[S], e: Equal[S]) = apply((_, s) => s)

  def   obsTo[A: Show: Equal](f: O => A) = apply((o, _) => f(o))
  def stateTo[A: Show: Equal](f: S => A) = apply((_, s) => f(s))

  def apply[A: Show: Equal](f: (O, S) => A) =
    new A1((o, s) => Right(f(o, s)))

  def coll[A](f: (O, S) => Traversable[A])(implicit sa: Show[A]) =
    new {
      // TODO Problem: BaseOps requires Show and Equal which doesn't make sense for the collection asserts
      def assertDistinct(implicit ea: Equal[A], ev: CollAssert.FailedDistinct[A] => E) =
        Check.Point.Single[O, S, E](
          Function.const(focusName + " is distinct"),
          (o, s) => CollAssert.distinct(f(o, s)).map(ev))
    }


//  def collthing[A](f: (O, S) => Traversable[A])(implicit s: Show[A], e: Equal[A]) =

  trait BaseOps[A, OutA <: BaseOps[A, OutA, _], OutP <: BaseOps[A, _, OutP]] {
    protected val extract: (O, S) => Either[E, A]
    protected implicit val sa: Show[A]
    protected implicit val eq: Equal[A]

    protected def addA(c: Check.Around[O, S, E]): OutA
    protected def addP(c: Check.Point[O, S, E]): OutP

    protected final def around(name: String, t: (A, A) => Option[E]): OutA =
      aroundN(_ => name, t)

    protected final def aroundN(name: Option[(O, S)] => String, t: (A, A) => Option[E]): OutA =
      addA(Check.Around.Single(
        name,
        extract,
        (o, s, a: A) => extract(o, s).toOptionLeft(t(a, _))))

    final def assertBefore(expect: A)(implicit f: SomethingFailures[A, E]): OutA =
      around(
        focusName + " should start as " + sa.show(expect),
        (before, _) => f.expectEqual(expected = expect, before))

    final def assertAfter(expect: A)(implicit f: SomethingFailures[A, E]): OutA =
      around(
        focusName + " should end as " + sa.show(expect),
        (_, after) => f.expectEqual(expected = expect, after))

    final def assert(before: A, after: A)(implicit f: SomethingFailures[A, E]): OutA =
      assertBefore(before).assertAfter(after)

    final def assertChange(desc: String, expect: A => A)(implicit f: SomethingFailures[A, E]): OutA =
      around(
        focusName + " change: " + desc,
        (before, after) => f.expectChange(from = before, expected = expect(before), actual = after))

    final def assertChanges(implicit f: SomethingFailures[A, E]): OutA =
      around(
        focusName + " should change",
        f.expectNotEqual(_, _))

    final def assertDoesntChange(implicit f: SomethingFailures[A, E]): OutA =
      around(
        focusName + " shouldn't change",
        (before, after) => f.expectEqual(expected = before, actual = after))

    protected final def point(name: String, test: A => Option[E]): OutP =
      pointN(_ => name, test)

    protected final def pointN(name: Option[(O, S)] => String, test: A => Option[E]): OutP =
      addP(Check.Point.Single(
        name,
        (o, s) => extract(o, s).toOptionLeft(test)))

    def assertEqual(expect: A)(implicit f: SomethingFailures[A, E]): OutP =
      point(
        focusName + " = " + sa.show(expect),
        a => f.expectEqual(expected = expect, actual = a))

    def assertNotEqual(unexpect: A)(implicit f: SomethingFailures[A, E]): OutP =
      point(
        focusName + " ≠ " + sa.show(unexpect),
        a => f.expectNotEqual(unexpect, actual = a))

    def test(desc: String => String, t: A => Boolean)(implicit ev: String =:= E): OutP =
      test(desc, t, ev compose sa.show)

    def test(desc: String => String, t: A => Boolean, error: A => E): OutP =
      point(
        desc(focusName),
        a => if (t(a)) None else Some(error(a)))
  }

  class A1[A](protected val extract: (O, S) => Either[E, A])(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, A2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new A2(c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(c, extract)
  }

  class A2[A](val check: Check.Around[O, S, E], protected val extract: (O, S) => Either[E, A])(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, A2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new A2(check & c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(check & c, extract)
  }

  class I1[A](protected val extract: (O, S) => Either[E, A])(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, I2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new I2(c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(c, extract)
  }

  class I2[A](val check: Check[O, S, E], protected val extract: (O, S) => Either[E, A])(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, I2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new I2(check & c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(check & c, extract)
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