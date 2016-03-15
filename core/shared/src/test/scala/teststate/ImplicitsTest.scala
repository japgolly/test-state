package teststate.external_package

import language.reflectiveCalls
import scala.annotation.implicitNotFound
import utest.compileError

abstract class AbstractTest {
  trait A
  trait F[_]
  trait F2[_]
  trait R
  trait R2
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

  implicit val transformer: teststate.core.Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]

  @implicitNotFound(msg = "\n\nExpected: ${Expect}\n  Actual: ${Src}\n  .")
  sealed abstract class Became[Src, Expect] extends (Src => Expect) with Serializable
  private[this] final val singleton_Became = new Became[Any,Any] { def apply(x: Any): Any = x }
  object Became {
     implicit def tpEquals[A]: A Became A = singleton_Became.asInstanceOf[A Became A]
  }

  def test[A] = new {
    def apply[R](f: A => R) = new {
      def expect[E] (implicit ev: R Became E) = ()
      def expectSelf(implicit ev: R Became A) = ()
    }
  }

  def testAA[A] = test2[A, A]

  def test2[A, B] = new {
    def apply[R](f: (A, B) => R) = new {
      def expect[E](implicit ev: R Became E) = ()
      def expectA  (implicit ev: R Became A) = ()
      def expectB  (implicit ev: R Became B) = ()
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

  // ===================================================================================================================
  // Transformers
  // ===================================================================================================================

  test[Points          [O, S, E]](_.lift).expect[Points            [O2, S2, E2]]
  test[Arounds         [O, S, E]](_.lift).expect[Arounds           [O2, S2, E2]]
  test[Invariants      [O, S, E]](_.lift).expect[Invariants        [O2, S2, E2]]
  test[Actions   [F, R, O, S, E]](_.lift).expect[Actions   [F2, R2, O2, S2, E2]]

  compileError("(x: Actions   [F, R, O, S, E]) => x: Actions   [F2, R2, O2, S2, E2]")
  compileError("(x: Points          [O, S, E]) => x: Points            [O2, S2, E2]")
  compileError("(x: Arounds         [O, S, E]) => x: Arounds           [O2, S2, E2]")
  compileError("(x: Invariants      [O, S, E]) => x: Invariants        [O2, S2, E2]")
  import transformer.Auto._
                (x: Actions   [F, R, O, S, E]) => x: Actions   [F2, R2, O2, S2, E2]
                (x: Points          [O, S, E]) => x: Points            [O2, S2, E2]
                (x: Arounds         [O, S, E]) => x: Arounds           [O2, S2, E2]
                (x: Invariants      [O, S, E]) => x: Invariants        [O2, S2, E2]
}

