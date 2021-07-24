package teststate.external

import japgolly.microlibs.testutil.TestUtil._
import scala.annotation.nowarn
import utest._

abstract class AbstractTest {
  trait A
  trait F[X]
  trait R
  trait S
  trait E
  trait O { def bool: Boolean; def int: Int}
  trait F2[X]
  trait R2
  trait O2
  trait S2
  trait E2
  trait F3[X]
  trait R3
  trait O3
  trait S3
  trait E3
  type OS1 = teststate.data.OS[O, S]
  def o12: O => O2
  def o21: O2 => O
  def s12: S => S2
  def s21: S2 => S
  def e12: E => E2
  def e21: E2 => E

  implicit def fem: teststate.typeclass.ExecutionModel[F]

  def test2[A, B] = new Test2[A, B]
  def testAA[A] = test2[A, A]

  final class Test2[A, B] {
    @nowarn("cat=unused")
    def apply[R](f: (A, B) => R) = assertType[R]
  }

  val dsl: teststate.dsl.Dsl[F, R, O, S, E]
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

abstract class ImplicitsTest1 extends AbstractTest {
  import teststate.Exports._

  // ===================================================================================================================
  // Checks
  // ===================================================================================================================

  // mapO
  assertType[Points    [O, S, E]].map(_ mapO o21).is[Points    [O2, S, E]]
  assertType[Arounds   [O, S, E]].map(_ mapO o21).is[Arounds   [O2, S, E]]
  assertType[Invariants[O, S, E]].map(_ mapO o21).is[Invariants[O2, S, E]]

  // when
  assertType[Points    [O, S, E]].map(_.when(_.obs.bool)).is[Points    [O, S, E]]
  assertType[Arounds   [O, S, E]].map(_.when(_.obs.bool)).is[Arounds   [O, S, E]]
  assertType[Invariants[O, S, E]].map(_.when(_.obs.bool)).is[Invariants[O, S, E]]

  // compose (mono)
  testAA[Points    [O, S, E]](_ & _).is[Points    [O, S, E]]
  testAA[Arounds   [O, S, E]](_ & _).is[Arounds   [O, S, E]]
  testAA[Invariants[O, S, E]](_ & _).is[Invariants[O, S, E]]

  // compose (poly)
  test2[Points    [O, S, E], Arounds   [O, S, E]](_ & _).is[Invariants[O, S, E]]
  test2[Arounds   [O, S, E], Points    [O, S, E]](_ & _).is[Invariants[O, S, E]]
  test2[Invariants[O, S, E], Arounds   [O, S, E]](_ & _).is[Invariants[O, S, E]]
  test2[Invariants[O, S, E], Points    [O, S, E]](_ & _).is[Invariants[O, S, E]]
  test2[Points    [O, S, E], Invariants[O, S, E]](_ & _).is[Invariants[O, S, E]]
  test2[Arounds   [O, S, E], Invariants[O, S, E]](_ & _).is[Invariants[O, S, E]]

  // combine
  assertType[List[Points    [O, S, E]]].map(_.combine).is[Points    [O, S, E]]
  assertType[List[Arounds   [O, S, E]]].map(_.combine).is[Arounds   [O, S, E]]
  assertType[List[Invariants[O, S, E]]].map(_.combine).is[Invariants[O, S, E]]

  // fake subtyping
  assertType[Points [O, S, E]].map(c => c: Invariants[O, S, E])
  assertType[Arounds[O, S, E]].map(c => c: Invariants[O, S, E])

  // display
  assertType[Points    [O, S, E]].map(_.display).is[String]
  assertType[Arounds   [O, S, E]].map(_.display).is[String]
  assertType[Invariants[O, S, E]].map(_.display).is[String]

  // NamedOps
  assertType[Points    [O, S, E]].map(_ rename "").is[Points    [O, S, E]]
  assertType[Arounds   [O, S, E]].map(_ rename "").is[Arounds   [O, S, E]]
  assertType[Invariants[O, S, E]].map(_ rename "").is[Invariants[O, S, E]]

  // orEmpty
  assertType[Option[Points    [O, S, E]]].map(_.orEmpty).is[Points    [O, S, E]]
  assertType[Option[Arounds   [O, S, E]]].map(_.orEmpty).is[Arounds   [O, S, E]]
  assertType[Option[Invariants[O, S, E]]].map(_.orEmpty).is[Invariants[O, S, E]]

  // ===================================================================================================================
  // Actions
  // ===================================================================================================================

  // Conditional
  assertType[Actions[F, R, O, S, E]].map(_.when(_.obs.bool)).is[Actions[F, R, O, S, E]]

  // NamedOps
  assertType[Actions[F, R, O, S, E]].map(_ rename "").is[Actions[F, R, O, S, E]]

  // ActionOps1
  assertType[Actions[F, R, O, S, E]].map(_ mapO o21).is[Actions[F, R, O2, S, E]]

  // ActionOps2
  assertType[Actions[F, R, O, S, E]].map(_ times 3).is[Actions[F, R, O, S, E]]

  // ActionOps3
  assertType[Actions[F, R, O, S, E]].map(_.group("yay")).is[Actions[F, R, O, S, E]]

  // compose (mono)
  testAA[Actions[F, R, O, S, E]](_ >> _).is[Actions[F, R, O, S, E]]
  compileError("testAA[Actions[F, R, O, S, E]](_ +> _)")

  // combine
  assertType[List[Actions[F, R, O, S, E]]].map(_.combine).is[Actions[F, R, O, S, E]]

  // >>
  compileError("test2[Actions[F, R, O, S, E], Points       [O, S, E]](_ >> _)")
  compileError("test2[Actions[F, R, O, S, E], Arounds      [O, S, E]](_ >> _)")
  compileError("test2[Actions[F, R, O, S, E], Invariants   [O, S, E]](_ >> _)")
  compileError("test2[Points       [O, S, E], Actions[F, R, O, S, E]](_ >> _)")
  compileError("test2[Arounds      [O, S, E], Actions[F, R, O, S, E]](_ >> _)")
  compileError("test2[Invariants   [O, S, E], Actions[F, R, O, S, E]](_ >> _)")

  // orEmpty
  assertType[Option[Actions[F, R, O, S, E]]].map(_.orEmpty).is[Actions[F, R, O, S, E]]

  // ===================================================================================================================
  // +>
  // ===================================================================================================================

  // point* +> action +> (point | around)*

                testAA[Points    [      O, S, E]](_ +> _).is[Points[O, S, E]]
  compileError("testAA[Arounds   [      O, S, E]](_ +> _)")
  compileError("testAA[Invariants[      O, S, E]](_ +> _)")
  compileError("testAA[Actions   [F, R, O, S, E]](_ +> _)")

  compileError("test2[Points    [O, S, E], Arounds   [O, S, E]](_ +> _)")
  compileError("test2[Points    [O, S, E], Invariants[O, S, E]](_ +> _)")
  compileError("test2[Arounds   [O, S, E], Points    [O, S, E]](_ +> _)")
  compileError("test2[Arounds   [O, S, E], Invariants[O, S, E]](_ +> _)")
  compileError("test2[Invariants[O, S, E], Arounds   [O, S, E]](_ +> _)")
  compileError("test2[Invariants[O, S, E], Points    [O, S, E]](_ +> _)")

  test2[Points       [O, S, E], Actions[F, R, O, S, E]](_ +> _).is[Actions[F, R, O, S, E]]
  test2[Actions[F, R, O, S, E], Points       [O, S, E]](_ +> _).is[Actions[F, R, O, S, E]]
  test2[Actions[F, R, O, S, E], Arounds      [O, S, E]](_ +> _).is[Actions[F, R, O, S, E]]

  compileError("test2[Actions[F, R, O, S, E], Invariants   [O, S, E]](_ +> _)")
  compileError("test2[Arounds      [O, S, E], Actions[F, R, O, S, E]](_ +> _)")
  compileError("test2[Invariants   [O, S, E], Actions[F, R, O, S, E]](_ +> _)")

  test2[Points[O, S, E], Actions[F, R, O, S, E]]((p, action) =>
    p +> p +> action +> p +> p +> p.after +> p.after +> p
  ).is[Actions[F, R, O, S, E]]


  // ===================================================================================================================
  // Transformers
  // ===================================================================================================================

  abstract class Transformers1 {
    implicit val transformer: teststate.core.Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]

    assertType[Points          [O, S, E]].map(_.lift).is[Points            [O2, S2, E2]]
    assertType[Arounds         [O, S, E]].map(_.lift).is[Arounds           [O2, S2, E2]]
    assertType[Invariants      [O, S, E]].map(_.lift).is[Invariants        [O2, S2, E2]]
    assertType[Actions   [F, R, O, S, E]].map(_.lift).is[Actions   [F2, R2, O2, S2, E2]]

//    compileError("(x: Actions   [F, R, O, S, E]) => x: Actions   [F2, R2, O2, S2, E2]")
//    compileError("(x: Points          [O, S, E]) => x: Points            [O2, S2, E2]")
//    compileError("(x: Arounds         [O, S, E]) => x: Arounds           [O2, S2, E2]")
//    compileError("(x: Invariants      [O, S, E]) => x: Invariants        [O2, S2, E2]")
//    import transformer.Auto._
//                  (x: Actions   [F, R, O, S, E]) => x: Actions   [F2, R2, O2, S2, E2]
//                  (x: Points          [O, S, E]) => x: Points            [O2, S2, E2]
//                  (x: Arounds         [O, S, E]) => x: Arounds           [O2, S2, E2]
//                  (x: Invariants      [O, S, E]) => x: Invariants        [O2, S2, E2]
  }

  abstract class Transformers2 {
    implicit val t2: teststate.core.Transformer[F2, R2, O2, S2, E2, F, R, O, S, E]
    implicit val t3: teststate.core.Transformer[F3, R3, O3, S3, E3, F, R, O, S, E]
    val i2: Invariants[O2, S2, E2]
    val i3: Invariants[O3, S3, E3]

    val i = dsl.chooseInvariant("blah")(_.obs.int match {
      case 2 => i2.lift
      case 3 => i3.lift
      case _ => dsl.emptyInvariant
    })

    assertTypeOf(i).is[Invariants[O, S, E]]
  }

}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ImplicitsTest2 extends TestSuite {
  import teststate.Exports._

  implicit def displayX2: Display[X2] =
    Display("X2=" + _.i)

  override def tests = Tests {

    // Doesn't affect correctness. Is just an optional nicety so don't ram it down users' throats.
    // Use toString by default and if someone wants to see something different, add their own instance.
    // There is no cost to incorrectness here.
    // This really just that I've laid things out in such a way that implicits resolve as expected.
    "display" - {
      def test[A](a: A)(expect: String)(implicit s: Display[A]): Unit = {
        val actual = s(a)
        assert(actual == expect)
      }
      "undef"     - test(X1(9))("X1(9)")
      "defDirect" - test(X2(9))("X2=9")
      "defInCO"   - test(X3(9))("X3=9")
    }

  }
}
