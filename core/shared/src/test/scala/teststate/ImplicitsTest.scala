package teststate.external_package

import language.reflectiveCalls
import scala.annotation.implicitNotFound

abstract class AbstractTest {
  trait A
  trait F[_]
  trait R
  trait O {
    def bool: Boolean
  }
  trait O2
  trait S
  trait S2
  trait E
  trait E2
  type OS1 = teststate.data.OS[O, S]
  def o12: O => O2
  def o21: O2 => O
  def s12: S => S2
  def s21: S2 => S
  def e12: E => E2
  def e21: E2 => E

  implicit def fem: teststate.typeclass.ExecutionModel[F]

  @implicitNotFound(msg = "\n\nExpected: ${From}\n  Actual: ${To}\n\n")
  sealed abstract class =:=[From, To] extends (From => To) with Serializable
  private[this] final val singleton_=:= = new =:=[Any,Any] { def apply(x: Any): Any = x }
  object =:= {
     implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
  }

  def test[A] = new {
    def apply[R](f: A => R) = new {
      def expect[E] (implicit ev: R =:= E) = ()
      def expectSelf(implicit ev: R =:= A) = ()
    }
  }

  def testAA[A] = test2[A, A]

  def test2[A, B] = new {
    def apply[R](f: (A, B) => R) = new {
      def expect[E](implicit ev: E =:= R) = ()
      def expectA  (implicit ev: A =:= R) = ()
      def expectB  (implicit ev: B =:= R) = ()
    }
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

abstract class ImplicitsTest extends AbstractTest {
  import teststate.Exports._

  // ===================================================================================================================
  // Checks
  // ===================================================================================================================

  // mapO
  test[Points    [O, S, E]](_ mapO o21).expect[Points    [O2, S, E]]
  test[Arounds   [O, S, E]](_ mapO o21).expect[Arounds   [O2, S, E]]
  test[Invariants[O, S, E]](_ mapO o21).expect[Invariants[O2, S, E]]

  // when
  test[Points    [O, S, E]](_.when(_.obs.bool)).expect[Points    [O, S, E]]
  test[Arounds   [O, S, E]](_.when(_.obs.bool)).expect[Arounds   [O, S, E]]
  test[Invariants[O, S, E]](_.when(_.obs.bool)).expect[Invariants[O, S, E]]

  // compose (mono)
  testAA[Points    [O, S, E]](_ & _).expect[Points    [O, S, E]]
  testAA[Arounds   [O, S, E]](_ & _).expect[Arounds   [O, S, E]]
  testAA[Invariants[O, S, E]](_ & _).expect[Invariants[O, S, E]]

  // compose (poly)
  test2[Points    [O, S, E], Arounds   [O, S, E]](_ & _).expect[Invariants[O, S, E]]
  test2[Arounds   [O, S, E], Points    [O, S, E]](_ & _).expect[Invariants[O, S, E]]
  test2[Invariants[O, S, E], Arounds   [O, S, E]](_ & _).expect[Invariants[O, S, E]]
  test2[Invariants[O, S, E], Points    [O, S, E]](_ & _).expect[Invariants[O, S, E]]
  test2[Points    [O, S, E], Invariants[O, S, E]](_ & _).expect[Invariants[O, S, E]]
  test2[Arounds   [O, S, E], Invariants[O, S, E]](_ & _).expect[Invariants[O, S, E]]

  // fake subtyping
  test[Points [O, S, E]](c => c: Invariants[O, S, E])
  test[Arounds[O, S, E]](c => c: Invariants[O, S, E])

  // show
  test[Points    [O, S, E]](_.show).expect[String]
  test[Arounds   [O, S, E]](_.show).expect[String]
  test[Invariants[O, S, E]](_.show).expect[String]

  // ===================================================================================================================
  // Actions
  // ===================================================================================================================

  // Conditional
  test[Actions[F, R, O, S, E]](_.when(_.obs.bool)).expect[Actions[F, R, O, S, E]]

  // ActionOps1
  test[Actions[F, R, O, S, E]](_ mapO o21).expect[Actions[F, R, O2, S, E]]

  // ActionOps2
  test[Actions[F, R, O, S, E]](_ times 3).expect[Actions[F, R, O, S, E]]

  // ActionOps3
  testAA[Actions[F, R, O, S, E]](_ >> _).expect[Actions[F, R, O, S, E]]
}

