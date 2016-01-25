package teststate

object Dsl {
  @inline def apply[R, O, S, E] = new Dsl[R, O, S, E]
}

class Dsl[R, O, S, E] {
  type OS = teststate.OS[O, S]
  type ROS = teststate.ROS[R, O, S]
  type Name = Option[OS] => String
  private type ActionRun = ROS => Option[() => Either[E, O => S]]

  type Action = teststate.Action[R, O, S, E]
  type Check = teststate.Check[O, S, E]
  type CheckAround = teststate.Check.Around[O, S, E]

  // ===================================================================================================================

  def action(name: String) = new A1(_ => name)

  class A1(name: Name) {

    def act(f: ROS => Unit) =
      actTry(f.andThen(_ => None))

    def actTry(f: ROS => Option[E]) =
      new A2(name, Some(f))
  }

  class A2(name: Name, act: Option[ROS => Option[E]]) {
    def updateState(nextState: S => S) =
      updateStateO(s => _ => nextState(s))

    def updateStateO(nextState: S => O => S): Action.Single[R, O, S, E] =
      build(i => Some(() =>
        act.flatMap(_ apply i)
          .leftOr(nextState(i.state))
      ))

    def updateState2(f: S => Either[E, S]) =
      build(i => Some(() =>
        act.flatMap(_ apply i)
          .leftOrF(f(i.state).map(Function.const))
      ))

    def noStateUpdate: Action.Single[R, O, S, E] =
      updateStateO(s => _ => s)

    private def build(act: ActionRun): Action.Single[R, O, S, E] =
      Action.Single(name, act, Check.Around.empty)
  }

  // ===================================================================================================================

  def focus(name: String) = new FocusDsl[O, S, E](name)

  def bifocus[A: Show: Equal](name: String, fo: O => A, fs: S => A) =
    new BiFocusDsl[O, S, E, A](name, fo, fs)

//  def compareStateAndObs[A](name: String, fo: O => A, fs: S => A)(implicit ) =
//    new FocusDsl[O, S, E](name)

  def point (name: Option[OS] => String, test: OS => Option[E]) = Check.Point.Single(name, test)
  def before(name: Option[OS] => String, test: OS => Option[E]) = point(name, test).before
  def after (name: Option[OS] => String, test: OS => Option[E]) = point(name, test).after

  def assertEqual[A: Equal: Show](name: Option[OS] => String, expect: OS => A, actual: OS => A)(implicit f: SomethingFailures[A, E]) =
    point(name, i => f.expectEqual(expected = expect(i), actual = actual(i)))

  def around[A](name: Option[OS] => String, before: OS => A)(test: (OS, A) => Option[E]) =
    Check.Around.Dunno(name, before, test)
}

case class Equal[A](equal: (A, A) => Boolean)
object Equal {

  def byUnivEq[A]: Equal[A] = Equal(_ == _)

  implicit val equalString: Equal[String] = byUnivEq
  implicit val equalInt: Equal[Int] = byUnivEq
  implicit val equalBoolean: Equal[Boolean] = byUnivEq
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

  private def point(name: Option[OS[O, S]] => String, t: OS[A, A] => Option[E]) =
    Check.Point.Single[O, S, E](name, os =>
      t(OS(obs = fo(os.obs), state = fs(os.state))))
      //fo(o).toOptionLeft(oa => t(oa, fs(s))))

  def obs   = new FocusDsl[O, S, E](focusName).value(os => fo(os.obs))
  def state = new FocusDsl[O, S, E](focusName).value(os => fs(os.state))

  def assertEqual(implicit f: SomethingFailures[A, E]) =
    point(
      focusName + " = " + _.fold("<state>")(os => sa(fs(os.state))),
      os => f.expectEqual(expected = os.state, actual = os.obs))

  // TODO if assertEqual is here, all the other ops should be too
}

class FocusDsl[O, S, E](focusName: String) {
  type OS = teststate.OS[O, S]

  def value[A: Show: Equal](f: OS => A) = new A1(f)
  def collection[A](f: OS => TraversableOnce[A]) = new C0(f)

  trait CollOps[A, Out <: Check[O, S, E]] {
//    type Out <: Check[O, S, E]

    protected val as: OS => TraversableOnce[A]
    protected def build(name: Option[OS] => String, test: OS => Option[E]): Out

//    def assertEqual[C[X] <: TraversableOnce[X]](name: String => String, expect: OS => TraversableOnce[A])(implicit f: SomethingFailures[A, E]) =
//      build(
//        Function const name(focusName),
//        i => f.expectEqual(expected = expect(i), actual = actual(i)))

    def assertDistinct(implicit sa: Show[A], ev: CollAssert.FailedDistinct[A] => E) =
      build(
        Function.const(focusName + " are distinct."),
        os => CollAssert.distinct(as(os)).map(ev))

    def assertExistence(containsWhat: String,
                        expect: OS => Boolean, expected: OS => Set[A])
                       (implicit sa: Show[A],
                        ev1: CollAssert.FailedContainsAll[A] => E,
                        ev2: CollAssert.FailedContainsNone[A] => E) =
      build(
        _.fold(s"$focusName: Existence of $containsWhat."){os =>
          val e = expect(os)
          val c = if (e) "contain" else "don't contain"
          s"$focusName $c $containsWhat."
        },
        os => CollAssert.existence(expect(os), expected(os), as(os))
          .map(_.fold(ev2, ev1)))

    def assertEqualIgnoringOrder(name: String => String, expect: OS => TraversableOnce[A])(implicit sa: Show[A], ev: CollAssert.FailedEqualIgnoringOrder[A] => E) =
      build(
        Function.const(name(focusName)),
        os => CollAssert.equalIgnoringOrder(expect = expect(os), actual = as(os)).map(ev))

    // TODO Look, all the same

    def assertContainsAll[B <: A](name: String => String, required: OS => Set[B])(implicit sb: Show[B], ev: CollAssert.FailedContainsAll[B] => E) =
      build(
        Function.const(name(focusName)), // focusName + " contains all " + allWhat + "."),
        os => CollAssert.containsAll(required(os), as(os)).map(ev))

    def assertContainsOnly[B >: A](name: String => String, whitelist: OS => Set[B])(implicit sa: Show[A], ev: CollAssert.FailedContainsOnly[A] => E) =
      build(
        Function.const(name(focusName)),
        os => CollAssert.containsOnly(whitelist(os), as(os)).map(ev))

    def assertContainsNone[B >: A](name: String => String, blacklist: OS => Set[B])(implicit sa: Show[A], ev: CollAssert.FailedContainsNone[A] => E) =
      build(
        Function.const(name(focusName)),
        os => CollAssert.containsNone(blacklist(os), as(os)).map(ev))
  }

  class C0[A](as: OS => TraversableOnce[A]) {
    def point = new CP(as)

    // TODO ↓ Better just to call .check(After|Before) right?
    def before = new CA1(as)
    def after = new CA2(as)

//    def assert(before: A, after: A)(implicit f: SomethingFailures[A, E]) =
//      this.before.assertEqual(before) &
//      this.after.assertEqual(after)
      // TODO assertBefore(before).assertAfter(after)
  }

  class CA1[A](protected val as: OS => TraversableOnce[A]) extends CollOps[A, Check.Around.Single[O, S, E]] {
    // TODO Name should carry:         focusName + " should start as " + sa.show(expect),
    protected def build(name: Option[OS] => String, test: OS => Option[E]) =
      Check.Point.Single(name, test).before
  }
  class CA2[A](protected val as: OS => TraversableOnce[A]) extends CollOps[A, Check.Around.Single[O, S, E]] {
    protected def build(name: Option[OS] => String, test: OS => Option[E]) =
      Check.Point.Single(name, test).after
  }

  class CP[A](protected val as: OS => TraversableOnce[A]) extends CollOps[A, Check.Point.Single[O, S, E]] {
    protected def build(name: Option[OS] => String, test: OS => Option[E]) =
      Check.Point.Single(name, test)
  }

  trait BaseOps[A, OutA <: BaseOps[A, OutA, _], OutP <: BaseOps[A, _, OutP]] {
    protected val extract: OS => A
    protected implicit val sa: Show[A]
    protected implicit val eq: Equal[A]

    protected def addA(c: Check.Around[O, S, E]): OutA
    protected def addP(c: Check.Point[O, S, E]): OutP

    protected final def around(name: String, t: (A, A) => Option[E]): OutA =
      aroundN(_ => name, t)

    protected final def aroundN(name: Option[OS] => String, t: (A, A) => Option[E]): OutA =
      addA(Check.Around.Dunno(
        name,
        extract,
        (os, a: A) => t(a, extract(os))))

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

    protected final def pointN(name: Option[OS] => String, test: A => Option[E]): OutP =
      addP(Check.Point.Single(
        name,
        os => test(extract(os))))

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

  class A1[A](protected val extract: OS => A)(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, A2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new A2(c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(c, extract)
  }

  class A2[A](val check: Check.Around[O, S, E], protected val extract: OS => A)(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, A2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new A2(check & c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(check & c, extract)
  }

  class I1[A](protected val extract: OS => A)(implicit protected val sa: Show[A], protected val eq: Equal[A])
    extends BaseOps[A, I2[A], I2[A]] {
    protected def addA(c: Check.Around[O, S, E]) = new I2(c, extract)
    protected def addP(c: Check.Point [O, S, E]) = new I2(c, extract)
  }

  class I2[A](val check: Check[O, S, E], protected val extract: OS => A)(implicit protected val sa: Show[A], protected val eq: Equal[A])
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
  private type Name = Option[OS[O, S]] => String
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