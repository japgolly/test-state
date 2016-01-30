package teststate

//import teststate.{Action, Equal, Show, Check, ROS, OS, ExecutionModel, Id, SomethingFailures, TestStateExtMethodsForEither, TestStateExtMethodsForOption}

import Dsl.{Types, ActionB}

object Dsl {
  def apply[F[_]: ExecutionModel, R, O, S, E] =
    new Dsl[F, R, O, S, E]

  @inline def sync[R, O, S, E] =
    apply[Id, R, O, S, E]

  import scala.concurrent._
  @inline def future[R, O, S, E](implicit ec: ExecutionContext) =
    apply[Future, R, O, S, E]

  trait Types[F[_], R, O, S, E] {
    final type OS          = teststate.OS[O, S]
    final type ROS         = teststate.ROS[R, O, S]
    final type Action      = teststate.Action[F, R, O, S, E]
    final type Check       = teststate.Check[O, S, E]
    final type CheckAround = teststate.Check.Around[O, S, E]
    final type Point1      = teststate.Check.Point.Single[O, S, E]
    final type Around1     = teststate.Check.Around.Dunno[O, S, E]
    final type Action1     = teststate.Action.Single[F, R, O, S, E]
    final type Name        = Option[OS] => String
    final type ActionFn    = ROS => Option[() => F[Either[E, O => S]]]
  }

  final class ActionB[F[_], R, O, S, E](actionName: String)(implicit EM: ExecutionModel[F]) extends Types[F, R, O, S, E] {

    private def build(fn: (ROS => F[Option[E]]) => ActionFn): ActionB2[F, R, O, S, E] =
      new ActionB2(act => Action.Single[F, R, O, S, E](
        _ => actionName,
        fn(act), Check.Around.empty))

    def updateState(nextState: S => S) =
      updateStateO(s => _ => nextState(s))

    def updateStateO(nextState: S => O => S) =
      build(act => i => Some(() =>
        EM.map(act(i))(_.leftOr(nextState(i.state)))
      ))

    def updateState2(f: S => Either[E, S]) =
      build(act => i => Some(() =>
        EM.map(act(i))(_.leftOrF(f(i.state).map(Function.const)))
      ))

    def noStateUpdate =
      updateStateO(s => _ => s)
  }

  final class ActionB2[F[_], R, O, S, E](build: (ROS[R, O, S] => F[Option[E]]) => Action.Single[F, R, O, S, E])(implicit EM: ExecutionModel[F]) {

    def act[U](f: ROS[R, O, S] => F[U]) =
      build(f.andThen(EM.map(_)(_ => None)))

    def actTry(f: ROS[R, O, S] => F[Option[E]]) =
      build(f)
  }
}

final class Dsl[F[_], R, O, S, E](implicit EM: ExecutionModel[F]) extends Types[F, R, O, S, E] {

  def point(name: Option[OS] => String, test: OS => Option[E]): Point1 =
    Check.Point.Single(name, test)

  def around[A](name: Option[OS] => String, before: OS => A)(test: (OS, A) => Option[E]): Around1 =
    Check.Around.Dunno(name, before, test)

  // TODO Delete
//  def point(name: String, test: OS => Option[E]): Point1 = Check.Point.Single(_ => name, test)
//  def around[A](name: String, before: OS => A)(test: (OS, A) => Option[E]): Around1 = Check.Around.Dunno(_ => name, before, test)

  //  val point = Check.Point.Single.apply[O, S, E] _
//  def around[A] = Check.Around.Dunno.apply[O, S, E] _

  private def strErrorFn(implicit ev: String =:= E): Any => E = _ => ""
  private def strErrorFn2(implicit ev: String =:= E): (Any, Any) => E = (_,_) => ""


  def test(name: Name, testFn: OS => Boolean)(implicit ev: String =:= E): Point1 =
    test(name, testFn, strErrorFn)

  def test(name: Name, testFn: OS => Boolean, error: OS => E): Point1 =
    point(name, os => if (testFn(os)) None else Some(error(os)))

  def testAround(name: Name, testFn: (OS, OS) => Boolean)(implicit ev: String =:= E): Around1 =
    testAround(name, testFn, strErrorFn2)

  def testAround(name: Name, testFn: (OS, OS) => Boolean, error: (OS, OS) => E): Around1 =
    around(name, identity)((x, y) => if (testFn(x, y)) None else Some(error(x, y)))






  def focus(focusName: String) =
    new Focus(focusName)

  final class Focus(focusName: String) {
    // TODO Make implicit
    def value[A: Show](f: OS => A) =
      new FocusValue(focusName, f)

    def collection[A: Show](f: OS => TraversableOnce[A]) =
      new FocusColl(focusName, f)

    def obsAndState[A: Show](name: String, fo: O => A, fs: S => A) =
      new BiFocus[A](name, fo, fs)
  }

  // ===================================================================================================================

  def action(actionName: String) =
    new ActionB[F, R, O, S, E](actionName)

  // ===================================================================================================================

  final class FocusValue[A](focusName: String, focusFn: OS => A)(implicit showA: Show[A]) {

    def map[B: Show](f: A => B): FocusValue[B] =
      new FocusValue(focusName, f compose focusFn)

    private def suffix(desc: String): String => String =
      _ + " " + desc

    def test(descSuffix: String, testFn: A => Boolean)(implicit ev: String =:= E): Point1 =
      test(suffix(descSuffix), testFn)

    def test(descSuffix: String, testFn: A => Boolean, error: A => E): Point1 =
      test(suffix(descSuffix), testFn, error)

    def test(desc: String => String, testFn: A => Boolean)(implicit ev: String =:= E): Point1 =
      test(desc, testFn, strErrorFn)

    def test(desc: String => String, testFn: A => Boolean, error: A => E): Point1 =
      Dsl.this.test(
        Function const desc(focusName),
        testFn compose focusFn,
        error compose focusFn)

    def testAround(descSuffix: String, testFn: (A, A) => Boolean)(implicit ev: String =:= E): Around1 =
      testAround(suffix(descSuffix), testFn)

    def testAround(descSuffix: String, testFn: (A, A) => Boolean, error: (A, A) => E): Around1 =
      testAround(suffix(descSuffix), testFn, error)

    def testAround(desc: String => String, testFn: (A, A) => Boolean)(implicit ev: String =:= E): Around1 =
      testAround(desc, testFn, strErrorFn2)

    def testAround(desc: String => String, testFn: (A, A) => Boolean, error: (A, A) => E): Around1 =
      around(Function const desc(focusName), focusFn)((os, a1) => {
        val a2 = focusFn(os)
        if (testFn(a1, a2)) None else Some(error(a1, a2))
      })

    def assert: AssertOps = new AssertOps(true)
    def assert(positive: Boolean): AssertOps = new AssertOps(positive)

    final class AssertOps(positive: Boolean) {
      private val should: String =
        if (positive) "should" else "should not"

      def not = new AssertOps(!positive)

      def equal(expect: A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Point1 =
        Check.Point.Single(
          Function const s"$focusName $should be ${showA(expect)}.",
          i => f.expectMaybeEqual(positive, ex = expect, actual = focusFn(i)))

      def equalF(expect: OS => A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Point1 =
        Check.Point.Single(
          {
            case None => s"$focusName $should be <?>."
            case Some(i) => s"$focusName $should be ${showA(expect(i))}."
          },
          i => f.expectMaybeEqual(positive, ex = expect(i), actual = focusFn(i)))

      def beforeAndAfter(before: A, after: A)(implicit e: Equal[A], f: SomethingFailures[A, E]) =
        equal(before).before & equal(after).after

      def beforeAndAfterF(before: OS => A, after: OS => A)(implicit e: Equal[A], f: SomethingFailures[A, E]) =
        equalF(before).before & equalF(after).after

      def changesTo(expect: A => A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Around1 =
        Check.Around.Dunno(
          {
            case None => s"$focusName $should be <?>."
            case Some(i) => s"$focusName $should be ${showA(expect(focusFn(i)))}."
          },
          focusFn,
          (os, a1: A) => {
            val a2 = focusFn(os)
            f.expectMaybeEqual(positive, ex = expect(a1), actual = a2)
          })

      def changeOccurs(implicit e: Equal[A], f: SomethingFailures[A, E]) =
        not.changesTo(identity)
    }
  } // FocusValue

  // ===================================================================================================================

  final class FocusColl[A](focusName: String, focusFn: OS => TraversableOnce[A])(implicit showA: Show[A]) {

    /*
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

     */
  }


  // ===================================================================================================================

  final class BiFocus[A](focusName: String, fo: O => A, fs: S => A)(implicit showA: Show[A]) {

    def map[B: Show](f: A => B): BiFocus[B] =
      new BiFocus(focusName, f compose fo, f compose fs)

    def obs   = new FocusValue(focusName, os => fo(os.obs))
    def state = new FocusValue(focusName, os => fs(os.state))

    def assert: AssertOps = new AssertOps(true)
    def assert(positive: Boolean): AssertOps = new AssertOps(positive)

    final class AssertOps(positive: Boolean) {
      private val should: String =
        if (positive) "should" else "should not"

      def not = new AssertOps(!positive)

      def equal(implicit e: Equal[A], f: SomethingFailures[A, E]): Point1 =
        Check.Point.Single(
          {
            case None => s"$focusName $should be <?>."
            case Some(i) => s"$focusName $should be ${showA(fs(i.state))}."
          },
          os => f.expectMaybeEqual(positive, ex = fs(os.state), actual = fo(os.obs)))
    }
  }

}

// TODO Runner should print state & obs on failure, each assertion needn't. It should print S and/or S' depending on the type of check (pre and/or post) that failed.
// TODO Give Option[OS] => String a better type and add implicits from a plain String
// TODO History should handle empty error strings
/*
object Exampe {
  implicit def sa[A]: Show[A] = ???
  implicit def ea[A]: Equal[A] = ???
  val * = Dsl.sync[Unit, Unit, Unit, String]

  *.focus("stuff").value(_ => 0).assert.changeOccurs

  *.focus("stuff").value(_ => 0).assert.equal(3).before
  *.focus("stuff").value(_ => 0).assert.beforeAndAfter(1, 2)
  *.focus("stuff").value(_ => 0).assert.changesTo(_ + 1)

  *.focus("stuff").value(_ => 0).assert.not.equal(3).before
  *.focus("stuff").value(_ => 0).assert.not.beforeAndAfter(1, 2)
  *.focus("stuff").value(_ => 0).assert.not.changesTo(_ + 1)
}
*/