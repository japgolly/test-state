package teststate.dsl

import japgolly.univeq.UnivEq
import scala.collection.compat._
import teststate.core._
import teststate.core.Types.SackE
import teststate.data._
import teststate.run.Observer
import teststate.typeclass._
import CoreExports._

object Dsl {
  def full[F[_]: ExecutionModel, R, O, S, E] =
    new Dsl[F, R, O, S, E](identity)

  def apply[R, O, S] =
    full[Id, R, O, S, String]

  import scala.concurrent._
  def future[R, O, S](implicit ec: ExecutionContext) =
    full[Future, R, O, S, String]

  trait Types[F[_], R, O, S, E] extends Any {
    final type OS                   = teststate.data.OS[O, S]
    final type ROS                  = teststate.data.ROS[R, O, S]
    final type ActionName           = teststate.data.NameFn[ROS]
    final type AssertionName        = teststate.data.NameFn[OS]
    final type AroundName           = teststate.data.NameFn[BeforeAfter[OS]]
    final type Points               = CoreExports.Points[O, S, E]
    final type Arounds              = CoreExports.Arounds[O, S, E]
    final type Invariants           = CoreExports.Invariants[O, S, E]
    final type Actions              = CoreExports.Actions[F, R, O, S, E]
    final type PlanWithInitialState = teststate.run.PlanWithInitialState[F, R, O, S, E]
    final type TestWithInitialState = teststate.run.TestWithInitialState[F, R, O, S, E]
    final type Plan                 = teststate.run.Plan[F, R, O, S, E]
    final type Test                 = teststate.run.Test[F, R, O, S, E]
  }

  def Types[F[_], R, O, S, E]: Types[F, R, O, S, E] =
    new Types[F, R, O, S, E]{}
}


final class Dsl[F[_], R, O, S, E](actionMod: Action.Single[F, R, O, S, E] => Action.Single[F, R, O, S, E])
                                 (implicit EM: ExecutionModel[F]) extends Dsl.Types[F, R, O, S, E] {

  // Allows: import dsl.Types._
  val Types = Dsl.Types[F, R, O, S, E]

  def emptyAction: Actions =
    Empty.instance

  def emptyAround: Arounds =
    Empty.instance

  def emptyInvariant: Invariants =
    Empty.instance

  def emptyPlan: Plan =
    Empty.instance

  def emptyTest(observer: Observer[R, O, E])(implicit a: ErrorHandler[E]): Test =
    emptyPlan.test(observer)(a)

  val transformer: Transformer[F, R, O, S, E, F, R, O, S, E] =
    Transformer.id[F, R, O, S, E]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private def sack1[A](a: A) =
    Sack.Value(Right(a))

  private def sackE(ne: NamedError[Failure[E]]) =
    Sack.Value(Left(ne))

  private def strErrorFn(implicit ev: String =:= E): Any => E =
    _ => ""

  private def strErrorFn2(implicit ev: String =:= E): (Any, Any) => E =
    (_,_) => ""

  def point(name: AssertionName)(test: OS => Option[E]): Points =
    sack1(Point(name, Tri failedOption test(_)))

  def around[A](name: AroundName)(before: OS => A)(test: (OS, A) => Option[E]): Arounds =
    sack1(Around.Delta(Around.DeltaA(name, os => Passed(before(os)), test)))

  def test(name: AssertionName)(testFn: OS => Boolean)(implicit ev: String =:= E): Points =
    test(name, strErrorFn)(testFn)

  def test(name: AssertionName, error: OS => E)(testFn: OS => Boolean): Points =
    point(name)(os => if (testFn(os)) None else Some(error(os)))

  def testAround(name: AroundName)(testFn: (OS, OS) => Boolean)(implicit ev: String =:= E): Arounds =
    testAround(name, strErrorFn2)(testFn)

  def testAround(name: AroundName, error: (OS, OS) => E)(testFn: (OS, OS) => Boolean): Arounds =
    around(name)(identity)((x, y) => if (testFn(x, y)) None else Some(error(x, y)))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def chooseInvariant       (n: AssertionName)(f: OS => Invariants)     : Invariants = choose(n, f)
  def chooseInvariantAttempt(n: AssertionName)(f: OS => E Or Invariants): Invariants = chooseAttempt(n, f)

  def choosePoint       (n: AssertionName)(f: OS => Points)     : Points = choose(n, f)
  def choosePointAttempt(n: AssertionName)(f: OS => E Or Points): Points = chooseAttempt(n, f)

  def chooseAround       (n: AssertionName)(f: OS => Arounds)     : Arounds = choose(n, f)
  def chooseAroundAttempt(n: AssertionName)(f: OS => E Or Arounds): Arounds = chooseAttempt(n, f)

  def chooseAction       (n: ActionName)(f: ROS => Actions)     : Actions = choose(n, f)
  def chooseActionAttempt(n: ActionName)(f: ROS => E Or Actions): Actions = chooseAttempt(n, f)

  private def choose[A, B](name: NameFn[A], f: A => Sack[A, B]): Sack[A, B] =
    Sack.CoProduct(name, f)

  private def chooseAttempt[A, B](name: NameFn[A], f: A => E Or SackE[A, B, E]): SackE[A, B, E] =
    Sack.CoProduct(name, f(_).recover(e => sackE(NamedError(name(None), Failure NoCause e))))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def action(actionName: ActionName) =
    new ActionB(actionName)

  final class ActionB(actionName: ActionName) {
    def apply[U](f: ROS => F[U]): Actions =
      attempt(f.andThen(EM.map(_)(_ => None)))

    def attempt(f: ROS => F[Option[E]]): Actions =
      full(i => EM.map(f(i))(oe => Or.liftLeft(oe, _ => Right(i.state))))

    def update(f: ROS => F[S]): Actions =
      updateBy(i => EM.map(f(i))(Function.const))

    def updateBy(f: ROS => F[O => S]): Actions =
      full(i => EM.map(f(i))(os => Right(os.andThen(Right(_)))))

    def full[U](f: ROS => F[E Or (O => E Or S)]): Actions = {
      val a1 = Action.Single[F, R, O, S, E](i => Some(() => f(i)))
      val a2 = actionMod(a1)
      Action.liftInner(a2)(actionName)
    }
  }

  def print(f: ROS => Any): Actions =
    print("Print <?>.", f)

  def print(name: => ActionName, f: ROS => Any): Actions =
    action(name)(i => EM.point {
      println(f(i))
    })

  /** Multiple calls to this result in the earlier-specified functions running inside the later-specified ones.
    */
  def withActionMod(f: Action.Single[F, R, O , S, E] => Action.Single[F, R, O , S, E]): Dsl[F, R, O, S, E] =
    new Dsl(f compose actionMod)

  /** Execute some kind of arbitrary assertion just before action execution. */
  def withPreActionAssertion(assert: ROS => F[Unit]): Dsl[F, R, O, S, E] =
    withActionMod(_.modAction((ros, actionFn) => EM.flatMap(assert(ros))(_ => actionFn())))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def focus(focusName: => String) =
    new Focus(focusName)

  final class Focus(focusName: => String) {
    def value[A: Display](f: OS => A) =
      new FocusValue(focusName, f)

    def option[A](f: OS => Option[A]) =
      new FocusOption(focusName, f)

    def collection[T[x] <: IterableOnce[x], A](f: OS => T[A]) =
      new FocusColl(focusName, f)

    def obsAndState[A: Display](fo: O => A, fs: S => A) =
      new ObsAndState[A](focusName, fo, fs)

    def compare[A: Display](actual: OS => A, expect: OS => A) =
      new BiFocus[A](focusName, fa = actual, fe = expect)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class FocusValue[A](focusName: => String, focusFn: OS => A)(implicit displayA: Display[A]) {

    def rename(n: => String) = new FocusValue[A](n, focusFn)
    def run = focusFn

    def map[B: Display](f: A => B): FocusValue[B] =
      new FocusValue(focusName, f compose focusFn)

    private def suffix(desc: String): String => String =
      _ + " " + desc

    def test(descSuffix: String)(testFn: A => Boolean)(implicit ev: String =:= E): Points =
      test(suffix(descSuffix))(testFn)

    def test(descSuffix: String, error: A => E)(testFn: A => Boolean): Points =
      test(suffix(descSuffix), error)(testFn)

    def test(desc: String => String)(testFn: A => Boolean)(implicit ev: String =:= E): Points =
      test(desc, strErrorFn)(testFn)

    def test(desc: String => String, error: A => E)(testFn: A => Boolean): Points =
      Dsl.this.test(desc(focusName), error compose focusFn)(testFn compose focusFn)

    def testAround(descSuffix: String)(testFn: (A, A) => Boolean)(implicit ev: String =:= E): Arounds =
      testAround(suffix(descSuffix))(testFn)

    def testAround(descSuffix: String, error: (A, A) => E)(testFn: (A, A) => Boolean): Arounds =
      testAround(suffix(descSuffix), error)(testFn)

    def testAround(desc: String => String)(testFn: (A, A) => Boolean)(implicit ev: String =:= E): Arounds =
      testAround(desc, strErrorFn2)(testFn)

    def testAround(desc: String => String, error: (A, A) => E)(testFn: (A, A) => Boolean): Arounds =
      around(desc(focusName))(focusFn)((os, a1) => {
        val a2 = focusFn(os)
        if (testFn(a1, a2)) None else Some(error(a1, a2))
      })

    def assertB(positive: Boolean): AssertOps =
      new AssertOps(positive)

    def assert: AssertOps =
      new AssertOps(true)

    @inline def assert(expect: A)(implicit e: Equal[A], f: DisplayFailure[A, E]): Points =
      assert.equal(expect)(e, f)

    final class AssertOps(positive: Boolean) {
      def not = new AssertOps(!positive)

      def equal(expect: A)(implicit e: Equal[A], f: DisplayFailure[A, E]): Points =
        point(NameUtils.equal(focusName, positive, expect))(
          i => f.expectMaybeEqual(positive, ex = expect, actual = focusFn(i)))

      def equalBy(expect: OS => A)(implicit e: Equal[A], f: DisplayFailure[A, E]): Points =
        point(NameFn(NameUtils.equalFn(focusName, positive, expect)))(
          i => f.expectMaybeEqual(positive, ex = expect(i), actual = focusFn(i)))

      def equalWhenDefined(expect: Option[A])(implicit e: Equal[A], f: DisplayFailure[A, E]): Points =
        expect match {
          case Some(a) => equal(a)
          case None    => point(NameUtils.equal(focusName, positive, expect))(_ => None).skip
        }

      def equalByWhenDefined(expect: OS => Option[A])(implicit e: Equal[A], f: DisplayFailure[A, E]): Points =
        point(NameFn(NameUtils.equalOptionFn(focusName, positive, expect)))(os =>
          expect(os) match {
            case Some(a) => f.expectMaybeEqual(positive, ex = a, actual = focusFn(os))
            case None    => None
          }
        )

      def beforeAndAfter(before: A, after: A)(implicit e: Equal[A], f: DisplayFailure[A, E]): Arounds =
        equal(before).before & equal(after).after

      def beforeAndAfterBy(before: OS => A, after: OS => A)(implicit e: Equal[A], f: DisplayFailure[A, E]): Arounds =
        equalBy(before).before & equalBy(after).after

      private def mkAround(name: AroundName, f: (A, A) => Option[E]) =
        around(name)(focusFn)((os, a) => f(a, focusFn(os)))

      def changeTo(expect: A => A)(implicit e: Equal[A], f: DisplayFailure[A, E]): Arounds =
        mkAround(
          NameFn(NameUtils.equalFn(focusName, positive, i => expect(focusFn(i.before)))),
          (a1, a2) => f.expectMaybeEqual(positive, ex = expect(a1), actual = a2))

      def change(implicit e: Equal[A], f: DisplayFailure[A, E]): Arounds =
        not.changeTo(identity)
          .rename(NameUtils.subjectShouldVerb(focusName, positive, "change"))

      def noChange(implicit e: Equal[A], f: DisplayFailure[A, E]): Arounds =
        not.change

      def increaseBy(a: A)(implicit n: Numeric[A], q: Equal[A], f: DisplayFailure[A, E], s: Display[A]): Arounds =
        changeTo(n.plus(_, a))(q, f)
        .rename(NameUtils.subjectShouldVerb(focusName, positive, "increase by " + s(a)))

      def decreaseBy(a: A)(implicit n: Numeric[A], q: Equal[A], f: DisplayFailure[A, E], s: Display[A]): Arounds =
        changeTo(n.minus(_, a))(q, f)
          .rename(NameUtils.subjectShouldVerb(focusName, positive, "decrease by " + s(a)))

      def increment(implicit n: Numeric[A], q: Equal[A], f: DisplayFailure[A, E], s: Display[A]): Arounds =
        increaseBy(n.one)(n, q, f, s)

      def decrement(implicit n: Numeric[A], q: Equal[A], f: DisplayFailure[A, E], s: Display[A]): Arounds =
        decreaseBy(n.one)(n, q, f, s)
    }
  } // FocusValue

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class FocusOption[A](focusName: => String, focusFn: OS => Option[A]) {

    def rename(n: => String) = new FocusOption[A](n, focusFn)
    def run = focusFn

    def filter(f: A => Boolean): FocusOption[A] =
      mapOption(_ filter f)

    def map[B](f: A => B): FocusOption[B] =
      mapOption(_ map f)

    def mapOption[B](f: Option[A] => Option[B]): FocusOption[B] =
      new FocusOption(focusName, f compose focusFn)

    def value(implicit s: Display[Option[A]]) =
      new FocusValue[Option[A]](focusName, focusFn)

    def valueBy[B](f: Option[A] => B)(implicit s: Display[B]) =
      new FocusValue[B](focusName, f compose focusFn)

    def isEmpty: FocusValue[Boolean] =
      valueBy(_.isEmpty).rename(focusName + " is empty")

    def isDefined: FocusValue[Boolean] =
      valueBy(_.isDefined).rename(focusName + " is defined")

    def assertB(positive: Boolean): AssertOps =
      new AssertOps(positive)

    def assert: AssertOps =
      new AssertOps(true)

    @inline def assert(expect: Option[A])(implicit eq: Equal[Option[A]], f: DisplayFailure[Option[A], E]): Points =
      assert.equal(expect)(eq, f)

    final class AssertOps(positive: Boolean) {
      def not = new AssertOps(!positive)

      import OptionAssertions._

//      private def wrapExp1[I](f: I => Boolean): I => Boolean =
//        if (positive) f else f.andThen(!_)

      def empty(implicit eq: Equal[Boolean], f: DisplayFailure[Boolean, E]): Points =
        isEmpty.assert(positive)

      def defined(implicit eq: Equal[Boolean], f: DisplayFailure[Boolean, E]): Points =
        isDefined.assert(positive)

      def equal(expect: Option[A])(implicit eq: Equal[Option[A]], f: DisplayFailure[Option[A], E]): Points =
        value.assertB(positive).equal(expect)

      def equalBy(expect: OS => Option[A])(implicit eq: Equal[Option[A]], f: DisplayFailure[Option[A], E]): Points =
        value.assertB(positive).equalBy(expect)

      def contains[B >: A](query: B)(implicit sa: Display[B], ea: Equal[B], ev: Contains.Failure[B] => E): Points =
        point(Contains(positive).name(focusName, sa(query)))(
          os => Contains(positive)(focusFn(os), query).map(ev))

//      def existenceOf(query: A)(expect: OS => Boolean)
//                     (implicit sa: Display[A], ea: Equal[A], ev: Contains.Failure[A] => E): Points = {
//        val e = wrapExp1(expect)
//        point(Contains.nameFn(e, focusName, sa(query)))(
//          os => Contains(e(os))(focusFn(os), query).map(ev))
//      }

      def forall(criteriaDesc: => String, criteria: A => Boolean)(implicit d: Display[A], ev: Forall.Failure[A] => E): Points = {
        val q = Forall(positive)
        point(q.name(focusName, criteriaDesc))(os =>
          q(focusFn(os))(criteria).map(ev))
      }

      def exists(criteriaDesc: => String, criteria: A => Boolean)(implicit d: Display[A], ev: Exists.Failure[A] => E): Points = {
        val q = Exists(positive)
        point(q.name(focusName, criteriaDesc))(os =>
          q(focusFn(os))(criteria).map(ev))
      }

    }
  } // FocusOption

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class FocusColl[C[X] <: IterableOnce[X], A](focusName: => String, focusFn: OS => C[A]) {

    def rename(n: => String) = new FocusColl[C, A](n, focusFn)
    def run = focusFn

    def map[B](f: A => B): FocusColl[Iterator, B] =
      mapColl(_.iterator map f)

    def filter(f: A => Boolean): FocusColl[Iterator, A] =
      mapColl(_.iterator filter f)

    def mapColl[D[X] <: IterableOnce[X], B](f: C[A] => D[B]): FocusColl[D, B] =
      new FocusColl(focusName, f compose focusFn)

    def value(implicit s: Display[C[A]]) =
      new FocusValue[C[A]](focusName, focusFn)

    def valueBy[B](f: C[A] => B)(implicit s: Display[B]) =
      new FocusValue[B](focusName, f compose focusFn)

    def size = valueBy(_.iterator.size).rename(focusName + " size")

    def assertB(positive: Boolean): AssertOps =
      new AssertOps(positive)

    def assert: AssertOps =
      new AssertOps(true)

    @inline def assert(expect: A*)(implicit eq: Equal[A], sa: Display[A], ev: CollectionAssertions.EqualIncludingOrder.Failure[A] => E): Points =
      assert.equal(expect: _*)(eq, sa, ev)

    final class AssertOps(positive: Boolean) {
      def not = new AssertOps(!positive)

      //def size = valueBy(_.size).assert(positive)

      private def wrapExp1[I](f: I => Boolean): I => Boolean =
        if (positive) f else f.andThen(!_)

      import CollectionAssertions._

      def distinct(implicit sa: Display[A], ev: Distinct.Failure[A] => E): Points = {
        val d = Distinct(positive)
        point(d.name(focusName))(
          os => d(focusFn(os)).map(ev))
      }

      def containsAll(queryNames: => String)(query: OS => Set[A])(implicit sb: Display[A], ua: UnivEq[A], ev: ContainsAll.Failure[A] => E): Points = {
        val d = ContainsAll(positive)
        point(d.name(focusName, queryNames))(
          os => d(focusFn(os), query(os)).map(ev))
      }

      def containsAny(queryNames: => String)(query: OS => Set[A])(implicit sb: Display[A], ev: ContainsAny.Failure[A] => E): Points = {
        val d = ContainsAny(positive)
        point(d.name(focusName, queryNames))(
          os => d(focusFn(os), query(os)).map(ev))
      }

      def containsNone(queryNames: => String)(query: OS => Set[A])(implicit sb: Display[A], ev: ContainsAny.Failure[A] => E): Points =
        not.containsAny(queryNames)(query)

      def containsOnly(queryNames: => String)(query: OS => Set[A])(implicit sa: Display[A], ua: UnivEq[A], ev: ContainsOnly.Failure[A] => E): Points = {
        val d = ContainsOnly(positive)
        point(d.name(focusName, queryNames))(
          os => d(focusFn(os), query(os)).map(ev))
      }

      def contains[B >: A](query: B)(implicit sa: Display[B], ea: Equal[B], ev: Contains.Failure[B] => E): Points =
        point(Contains(positive).name(focusName, sa(query)))(
          os => Contains(positive)(focusFn(os), query).map(ev))

      def existenceOf(query: A)(expect: OS => Boolean)
                     (implicit sa: Display[A], ea: Equal[A], ev: Contains.Failure[A] => E): Points = {
        val e = wrapExp1(expect)
        point(Contains.nameFn(e, focusName, sa(query)))(
          os => Contains(e(os))(focusFn(os), query).map(ev))
      }


      def existenceOfAll(allName: => String)(all: A*)(expect: OS => Boolean)
                        (implicit sa: Display[A], ua: UnivEq[A],
                         ev1: ContainsAny.FoundSome[A] => E,
                         ev2: ContainsAll.Missing[A] => E): Points =
        existenceOfAllBy(allName)(Function const UnivEq.toSet(all))(expect)

      def existenceOfAllBy(allName: => String)(all: OS => Set[A])(expect: OS => Boolean)
                          (implicit sa: Display[A], ua: UnivEq[A],
                           ev1: ContainsAny.FoundSome[A] => E,
                           ev2: ContainsAll.Missing[A] => E): Points = {
        val e = wrapExp1(expect)
        point(ExistenceOfAll.nameFn(e, focusName, allName))(
          os => ExistenceOfAll(e(os), focusFn(os), all(os)).map(_.fold(ev1, ev2)))
      }


      def equal(expect: A*)(implicit eq: Equal[A], sa: Display[A], ev: EqualIncludingOrder.Failure[A] => E): Points =
        equalBy(Function const expect)(eq, sa, ev)

      def equalBy(expect: OS => IterableOnce[A])(implicit eq: Equal[A], sa: Display[A], ev: EqualIncludingOrder.Failure[A] => E): Points = {
        val d = EqualIncludingOrder(positive)
        point(NameFn(NameUtils.equalFn(focusName, positive, expect)(sa.coll[IterableOnce])))(
          os => d(source = focusFn(os), expect = expect(os)).map(ev))
      }


      def equalIgnoringOrder(expect: A*)(implicit sa: Display[A], ev: EqualIgnoringOrder.Failure[A] => E): Points =
        equalIgnoringOrderBy(Function const expect)(sa, ev)

      def equalIgnoringOrderBy(expect: OS => IterableOnce[A])(implicit sa: Display[A], ev: EqualIgnoringOrder.Failure[A] => E): Points = {
        val d = EqualIgnoringOrder(positive)
        point(
          NameFn(NameUtils.equalFn(focusName, positive, expect)(sa.coll[IterableOnce])
            .andThen(_.map(_ + " (ignoring order)"))))(
          os => d(source = focusFn(os), expect = expect(os)).map(ev))
      }


      def elemChanges(del: A*)(add: A*)(implicit sa: Display[A], ev: ElemChanges.Failure[A] => E): Arounds =
        elemChangesBy(Function const del, Function const add)(sa, ev)

      def elemChangesBy(del: OS => IterableOnce[A], add: OS => IterableOnce[A])(implicit sa: Display[A], ev: ElemChanges.Failure[A] => E): Arounds = {
        val d = ElemChanges(positive)
        around(
          NameFn(NameUtils.collChangeFn(focusName, positive, "change", del, add)))(
          focusFn)(
          (os, b) =>
            d(ElemChanges.Args(
              before    = b,
              after     = focusFn(os),
              expectDel = del(os),
              expectAdd = add(os)
            )).map(ev))
      }

      def forall(criteriaDesc: => String, criteria: A => Boolean)(implicit d: Display[A], ev: Forall.Failure[A] => E): Points = {
        val q = Forall(positive)
        point(q.name(focusName, criteriaDesc))(os =>
          q(focusFn(os))(criteria).map(ev))
      }

      def exists(criteriaDesc: => String, criteria: A => Boolean)(implicit d: Display[A], ev: Exists.Failure[A] => E): Points = {
        val q = Exists(positive)
        point(q.name(focusName, criteriaDesc))(os =>
          q(focusFn(os))(criteria).map(ev))
      }

    }
  } // FocusColl

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed class BiFocus[A](focusName: => String, fa: OS => A, fe: OS => A)(implicit displayA: Display[A]) {

    def map[B: Display](f: A => B): BiFocus[B] =
      new BiFocus(focusName, f compose fa, f compose fe)

    def assertB(positive: Boolean): AssertOps =
      new AssertOps(positive)

    def assert: AssertOps =
      new AssertOps(true)

    final class AssertOps(positive: Boolean) {
      def not = new AssertOps(!positive)

      def equal(implicit e: Equal[A], f: DisplayFailure[A, E]): Points =
        point(NameFn(NameUtils.equalFn(focusName, positive, i => fe(i))))(
          os => f.expectMaybeEqual(positive, ex = fe(os), actual = fa(os)))
    }
  }

  final class ObsAndState[A](focusName: => String, fo: O => A, fs: S => A)(implicit displayA: Display[A])
      extends BiFocus(focusName, fa = i => fo(i.obs), fe = i => fs(i.state)) {

    def obs   = new FocusValue(focusName, os => fo(os.obs))
    def state = new FocusValue(focusName, os => fs(os.state))

    override def map[B: Display](f: A => B): ObsAndState[B] =
      new ObsAndState(focusName, f compose fo, f compose fs)
  }
}

// TODO Runner should print state & obs on failure, each assertion needn't. It should print S and/or S' depending on the type of check (pre and/or post) that failed.
