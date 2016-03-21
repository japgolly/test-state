package teststate.dsl

import acyclic.file
import teststate.core._
import teststate.data._
import teststate.run.Test
import teststate.typeclass._
import CoreExports._
import Dsl.Types
import Types.SackE

object Dsl {
  def apply[F[_]: ExecutionModel, R, O, S, E] =
    new Dsl[F, R, O, S, E]

  @inline def sync[R, O, S, E] =
    apply[Id, R, O, S, E]

  import scala.concurrent._
  @inline def future[R, O, S, E](implicit ec: ExecutionContext) =
    apply[Future, R, O, S, E]

  // TODO Rename DSL types like {Point,Around}{,s}, etc
  trait Types[F[_], R, O, S, E] {
    final type OS          = teststate.data.OS[O, S]
    final type ROS         = teststate.data.ROS[R, O, S]
    final type ANameFn     = teststate.data.NameFn[ROS]
    final type CNameFn     = teststate.data.NameFn[OS]
    final type ArNameFn    = teststate.data.NameFn[BeforeAfter[OS]]
    final type Point       = Points[O, S, E]
    final type Around      = Arounds[O, S, E]
    final type Invariant   = Invariants[O, S, E]
    final type Action      = Actions[F, R, O, S, E]
    final type TestContent = teststate.run.TestContent[F, R, O, S, E]
    final type Test        = teststate.run.Test[F, R, O, S, E]
  }
}

final class Dsl[F[_], R, O, S, E](implicit EM: ExecutionModel[F]) extends Types[F, R, O, S, E] {

  private def sack1[A](a: A) =
    Sack.Value(Right(a))

  private def sackE(ne: NamedError[E]) =
    Sack.Value(Left(ne))

  def point(name: CNameFn, test: OS => Option[E]): Point =
    sack1(Point(name, Tri failedOption test(_)))

  def around[A](name: ArNameFn, before: OS => A)(test: (OS, A) => Option[E]): Around =
    sack1(Around.Delta(Around.DeltaA(name, os => Passed(before(os)), test)))

  private def strErrorFn(implicit ev: String =:= E): Any => E = _ => ""
  private def strErrorFn2(implicit ev: String =:= E): (Any, Any) => E = (_,_) => ""


  def test(name: CNameFn, testFn: OS => Boolean)(implicit ev: String =:= E): Point =
    test(name, testFn, strErrorFn)

  def test(name: CNameFn, testFn: OS => Boolean, error: OS => E): Point =
    point(name, os => if (testFn(os)) None else Some(error(os)))

  def testAround(name: ArNameFn, testFn: (OS, OS) => Boolean)(implicit ev: String =:= E): Around =
    testAround(name, testFn, strErrorFn2)

  def testAround(name: ArNameFn, testFn: (OS, OS) => Boolean, error: (OS, OS) => E): Around =
    around(name, identity)((x, y) => if (testFn(x, y)) None else Some(error(x, y)))


  def chooseInvariant   (n: Name, f: OS => Invariant)     : Invariant = choose(n, f)
  def tryChooseInvariant(n: Name, f: OS => E Or Invariant): Invariant = tryChoose(n, f)

  def choosePoint   (n: Name, f: OS => Point)     : Point = choose(n, f)
  def tryChoosePoint(n: Name, f: OS => E Or Point): Point = tryChoose(n, f)

  def chooseAround   (n: Name, f: OS => Around)     : Around = choose(n, f)
  def tryChooseAround(n: Name, f: OS => E Or Around): Around = tryChoose(n, f)

  def chooseAction   (n: Name, f: ROS => Action)     : Action = choose(n, f)
  def tryChooseAction(n: Name, f: ROS => E Or Action): Action = tryChoose(n, f)

  private def choose[A, B](name: Name, f: A => Sack[A, B]): Sack[A, B] =
    Sack.CoProduct(name, f)

  private def tryChoose[A, B](name: Name, f: A => E Or SackE[A, B, E]): SackE[A, B, E] =
    Sack.CoProduct(name, f(_).recover(e => sackE(NamedError(name, e))))


  def focus(focusName: => String) =
    new Focus(focusName)

  final class Focus(focusName: => String) {
    // TODO Make implicit
    def value[A: Show](f: OS => A) =
      new FocusValue(focusName, f)

    def collection[T[x] <: TraversableOnce[x], A](f: OS => T[A]) =
      new FocusColl(focusName, f)

    def obsAndState[A: Show](fo: O => A, fs: S => A) =
      new ObsAndState[A](focusName, fo, fs)

    def compare[A: Show](actual: OS => A, expect: OS => A) =
      new BiFocus[A](focusName, fa = actual, fe = expect)
  }

  // ===================================================================================================================

  val transformer: Transformer[F, R, O, S, E, F, R, O, S, E] =
    Transformer.id[F, R, O, S, E]

  def emptyAction: Action =
    Sack.empty

  def emptyAround: Around =
    Sack.empty

  def emptyInvariant: Invariant =
    Sack.empty

  def emptyTest(implicit r: Recover[E]): TestContent =
    Test(emptyAction)(EM, r)

  // ===================================================================================================================

  def action(actionName: ANameFn) =
    new ActionB(actionName)

  final class ActionB(actionName: ANameFn) {
    def act[U](f: ROS => F[U]): Action =
      actTry(f.andThen(EM.map(_)(_ => None)))

    def actTry(f: ROS => F[Option[E]]): Action =
      full(i => EM.map(f(i))(oe => Or.liftLeft(oe, _ => Right(i.state))))

    def update(f: ROS => F[S]): Action =
      updateBy(i => EM.map(f(i))(Function.const))

    def updateBy(f: ROS => F[O => S]): Action =
      full(i => EM.map(f(i))(os => Right(os.andThen(Right(_)))))

    def full[U](f: ROS => F[E Or (O => E Or S)]): Action = {
      val a = Action.Single[F, R, O, S, E](i => Some(() => f(i)))
      Action.liftInner(a)(actionName)
    }
  }

  // ===================================================================================================================

  final class FocusValue[A](focusName: => String, focusFn: OS => A)(implicit showA: Show[A]) {

    def rename(n: => String) = new FocusValue[A](n, focusFn)
    def run = focusFn

    def map[B: Show](f: A => B): FocusValue[B] =
      new FocusValue(focusName, f compose focusFn)

    private def suffix(desc: String): String => String =
      _ + " " + desc

    def test(descSuffix: String, testFn: A => Boolean)(implicit ev: String =:= E): Point =
      test(suffix(descSuffix), testFn)

    def test(descSuffix: String, testFn: A => Boolean, error: A => E): Point =
      test(suffix(descSuffix), testFn, error)

    def test(desc: String => String, testFn: A => Boolean)(implicit ev: String =:= E): Point =
      test(desc, testFn, strErrorFn)

    def test(desc: String => String, testFn: A => Boolean, error: A => E): Point =
      Dsl.this.test(
        desc(focusName),
        testFn compose focusFn,
        error compose focusFn)

    def testAround(descSuffix: String, testFn: (A, A) => Boolean)(implicit ev: String =:= E): Around =
      testAround(suffix(descSuffix), testFn)

    def testAround(descSuffix: String, testFn: (A, A) => Boolean, error: (A, A) => E): Around =
      testAround(suffix(descSuffix), testFn, error)

    def testAround(desc: String => String, testFn: (A, A) => Boolean)(implicit ev: String =:= E): Around =
      testAround(desc, testFn, strErrorFn2)

    def testAround(desc: String => String, testFn: (A, A) => Boolean, error: (A, A) => E): Around =
      around(desc(focusName), focusFn)((os, a1) => {
        val a2 = focusFn(os)
        if (testFn(a1, a2)) None else Some(error(a1, a2))
      })

    def assert: AssertOps = new AssertOps(true)
    def assert(positive: Boolean): AssertOps = new AssertOps(positive)

    final class AssertOps(positive: Boolean) {

      def not = new AssertOps(!positive)

      def equal(expect: A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Point =
        point(
          NameUtils.equal(focusName, positive, expect),
          i => f.expectMaybeEqual(positive, ex = expect, actual = focusFn(i)))

      def equalBy(expect: OS => A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Point =
        point(
          NameFn(NameUtils.equalFn(focusName, positive, expect)),
          i => f.expectMaybeEqual(positive, ex = expect(i), actual = focusFn(i)))

      def beforeAndAfter(before: A, after: A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Around =
        equal(before).before & equal(after).after

      def beforeAndAfterBy(before: OS => A, after: OS => A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Around =
        equalBy(before).before & equalBy(after).after

      private def mkAround(name: ArNameFn, f: (A, A) => Option[E]) =
        around(name, focusFn)((os, a) => f(a, focusFn(os)))

      def changeTo(expect: A => A)(implicit e: Equal[A], f: SomethingFailures[A, E]): Around =
        mkAround(
          NameFn(NameUtils.equalFn(focusName, positive, i => expect(focusFn(i.before)))),
          (a1, a2) => f.expectMaybeEqual(positive, ex = expect(a1), actual = a2))

      def change(implicit e: Equal[A], f: SomethingFailures[A, E]) =
        not.changeTo(identity)
          .renameContextFree(NameUtils.subjectShouldVerb(focusName, positive, "change"))

      def noChange(implicit e: Equal[A], f: SomethingFailures[A, E]) =
        not.change

      def increaseBy(a: A)(implicit n: Numeric[A], q: Equal[A], f: SomethingFailures[A, E], s: Show[A]) =
        changeTo(n.plus(_, a))(q, f)
          .renameContextFree(NameUtils.subjectShouldVerb(focusName, positive, "increase by " + s(a)))

      def decreaseBy(a: A)(implicit n: Numeric[A], q: Equal[A], f: SomethingFailures[A, E], s: Show[A]) =
        changeTo(n.minus(_, a))(q, f)
          .renameContextFree(NameUtils.subjectShouldVerb(focusName, positive, "decrease by " + s(a)))

      def increment(a: A)(implicit n: Numeric[A], q: Equal[A], f: SomethingFailures[A, E], s: Show[A]) =
        increaseBy(n.one)(n, q, f, s)

      def decrement(a: A)(implicit n: Numeric[A], q: Equal[A], f: SomethingFailures[A, E], s: Show[A]) =
        decreaseBy(n.one)(n, q, f, s)
    }
  } // FocusValue

  // ===================================================================================================================

  final class FocusColl[C[X] <: TraversableOnce[X], A](focusName: => String, focusFn: OS => C[A]) {

    def rename(n: => String) = new FocusColl[C, A](n, focusFn)
    def run = focusFn

    def map[D[X] <: TraversableOnce[X], B: Show](f: C[A] => D[B]): FocusColl[D, B] =
      new FocusColl(focusName, f compose focusFn)

    def value(implicit s: Show[C[A]]) =
      new FocusValue[C[A]](focusName, focusFn)

    def valueBy[B](f: C[A] => B)(implicit s: Show[B]) =
      new FocusValue[B](focusName, f compose focusFn)

    def size = valueBy(_.size)

    def assert: AssertOps = new AssertOps(true)
    def assert(positive: Boolean): AssertOps = new AssertOps(positive)

    final class AssertOps(positive: Boolean) {
      def not = new AssertOps(!positive)

      //def size = valueBy(_.size).assert(positive)

      private def wrapExp1[I](f: I => Boolean): I => Boolean =
        if (positive) f else f.andThen(!_)

      import CollectionAssertions._

      def distinct(implicit sa: Show[A], ev: Distinct.Failure[A] => E) = {
        val d = Distinct(positive)
        point(
          d.name(focusName),
          os => d(focusFn(os)).map(ev))
      }

      def containsAll[B <: A](queryNames: => String, query: OS => Set[B])(implicit sb: Show[B], ev: ContainsAll.Failure[B] => E) = {
        val d = ContainsAll(positive)
        point(
          d.name(focusName, queryNames),
          os => d(focusFn(os), query(os)).map(ev))
      }

      def containsAny[B >: A](queryNames: => String, query: OS => Set[B])(implicit sb: Show[B], ev: ContainsAny.Failure[B] => E) = {
        val d = ContainsAny(positive)
        point(
          d.name(focusName, queryNames),
          os => d(focusFn(os), query(os)).map(ev))
      }

      def containsNone[B >: A](queryNames: => String, query: OS => Set[B])(implicit sb: Show[B], ev: ContainsAny.Failure[B] => E) =
        not.containsAny(queryNames, query)

      def containsOnly[B >: A](queryNames: => String, query: OS => Set[B])(implicit sa: Show[A], ev: ContainsOnly.Failure[A] => E) = {
        val d = ContainsOnly(positive)
        point(
          d.name(focusName, queryNames),
          os => d(focusFn(os), query(os)).map(ev))
      }

      // TODO inconsistency with blah{,By} and types

      def contains[B >: A](query: B)(implicit sa: Show[B], ea: Equal[B], ev: Exists.Failure[B] => E) =
        point(
          Exists(positive).name(focusName, sa(query)),
          os => Exists(positive)(focusFn(os), query).map(ev))

      def existenceOf(query: A)(expect: OS => Boolean)
                     (implicit sa: Show[A], ea: Equal[A], ev: Exists.Failure[A] => E) = {
        val e = wrapExp1(expect)
        point(
          Exists.nameFn(e, focusName, sa(query)),
          os => Exists(e(os))(focusFn(os), query).map(ev))
      }


      def existenceOfAll(allName: => String)(all: A*)(expect: OS => Boolean)
                        (implicit sa: Show[A], ev1: ContainsAny.FoundSome[A] => E, ev2: ContainsAll.Missing[A] => E) =
        existenceOfAllBy(allName, Function const all.toSet)(expect)

      def existenceOfAllBy(allName: => String, all: OS => Set[A])(expect: OS => Boolean)
                          (implicit sa: Show[A], ev1: ContainsAny.FoundSome[A] => E, ev2: ContainsAll.Missing[A] => E) = {
        val e = wrapExp1(expect)
        point(
          ExistenceOfAll.nameFn(e, focusName, allName),
          os => ExistenceOfAll(e(os), focusFn(os), all(os)).map(_.fold(ev1, ev2)))
      }


      def equal(expect: A*)(implicit eq: Equal[A], sa: Show[A], ev: EqualIncludingOrder.Failure[A] => E) =
        equalBy(Function const expect)(eq, sa, ev)

      def equalBy(expect: OS => TraversableOnce[A])(implicit eq: Equal[A], sa: Show[A], ev: EqualIncludingOrder.Failure[A] => E) = {
        val d = EqualIncludingOrder(positive)
        point(
          NameFn(NameUtils.equalFn(focusName, positive, expect)(sa.coll[TraversableOnce])),
          os => d(source = focusFn(os), expect = expect(os)).map(ev))
      }


      def equalIgnoringOrder(expect: A*)(implicit sa: Show[A], ev: EqualIgnoringOrder.Failure[A] => E) =
        equalIgnoringOrderBy(Function const expect)(sa, ev)

      def equalIgnoringOrderBy(expect: OS => TraversableOnce[A])(implicit sa: Show[A], ev: EqualIgnoringOrder.Failure[A] => E) = {
        val d = EqualIgnoringOrder(positive)
        point(
          NameFn(NameUtils.equalFn(focusName, positive, expect)(sa.coll[TraversableOnce])
            .andThen(_.map(_ + " (ignoring order)"))),
          os => d(source = focusFn(os), expect = expect(os)).map(ev))
      }


      def elemChanges(del: A*)(add: A*)(implicit sa: Show[A], ev: ElemChanges.Failure[A] => E) =
        elemChangesBy(Function const del, Function const add)(sa, ev)

      def elemChangesBy(del: OS => TraversableOnce[A], add: OS => TraversableOnce[A])(implicit sa: Show[A], ev: ElemChanges.Failure[A] => E) = {
        val d = ElemChanges(positive)
        around(
          NameFn(NameUtils.collChangeFn(focusName, positive, "change", del, add)),
          focusFn)(
          (os, b) =>
            d(ElemChanges.Args(
              before    = b,
              after     = focusFn(os),
              expectDel = del(os),
              expectAdd = add(os)
            )).map(ev))
      }

    }
  }


  // ===================================================================================================================

  sealed class BiFocus[A](focusName: => String, fa: OS => A, fe: OS => A)(implicit showA: Show[A]) {

    def map[B: Show](f: A => B): BiFocus[B] =
      new BiFocus(focusName, f compose fa, f compose fe)

    def assert: AssertOps = new AssertOps(true)
    def assert(positive: Boolean): AssertOps = new AssertOps(positive)

    final class AssertOps(positive: Boolean) {

      def not = new AssertOps(!positive)

      def equal(implicit e: Equal[A], f: SomethingFailures[A, E]): Point =
        point(
          NameFn(NameUtils.equalFn(focusName, positive, i => fe(i))),
          os => f.expectMaybeEqual(positive, ex = fe(os), actual = fa(os)))
    }
  }

  final class ObsAndState[A](focusName: => String, fo: O => A, fs: S => A)(implicit showA: Show[A])
      extends BiFocus(focusName, fa = i => fo(i.obs), fe = i => fs(i.state)) {

    def obs   = new FocusValue(focusName, os => fo(os.obs))
    def state = new FocusValue(focusName, os => fs(os.state))

    override def map[B: Show](f: A => B): ObsAndState[B] =
      new ObsAndState(focusName, f compose fo, f compose fs)
  }
}

// TODO Runner should print state & obs on failure, each assertion needn't. It should print S and/or S' depending on the type of check (pre and/or post) that failed.

/*
object Exampe {
  implicit def sa[A]: Show[A] = ???
  implicit def ea[A]: Equal[A] = ???
  val * = Dsl.sync[Unit, Unit, Unit, String]

  *.focus("stuff").value(_ => 0).assert.change

  *.focus("stuff").value(_ => 0).assert.equal(3).before
  *.focus("stuff").value(_ => 0).assert.beforeAndAfter(1, 2)
  *.focus("stuff").value(_ => 0).assert.changeTo(_ + 1)

  *.focus("stuff").value(_ => 0).assert.not.equal(3).before
  *.focus("stuff").value(_ => 0).assert.not.beforeAndAfter(1, 2)
  *.focus("stuff").value(_ => 0).assert.not.changeTo(_ + 1)

//  *.focus("stuff").collection(_ => List(1)).
}
*/