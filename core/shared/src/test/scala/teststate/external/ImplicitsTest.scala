package teststate.external

import language.reflectiveCalls
import scala.annotation.implicitNotFound
import utest.asserts.compileError
import utest._

abstract class AbstractTest {
  trait A
  trait F[_]
  trait R
  trait S
  trait E
  trait O { def bool: Boolean; def int: Int}
  trait F2[_]
  trait R2
  trait O2
  trait S2
  trait E2
  trait F3[_]
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

  def testExpr[A](a: A) = test[A](identity[A])

  val dsl: teststate.dsl.Dsl[F, R, O, S, E]
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

abstract class ImplicitsTest1 extends AbstractTest {
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

  // combine
  test[List[Points    [O, S, E]]](_.combine).expect[Points    [O, S, E]]
  test[List[Arounds   [O, S, E]]](_.combine).expect[Arounds   [O, S, E]]
  test[List[Invariants[O, S, E]]](_.combine).expect[Invariants[O, S, E]]

  // fake subtyping
  test[Points [O, S, E]](c => c: Invariants[O, S, E])
  test[Arounds[O, S, E]](c => c: Invariants[O, S, E])

  // show
  test[Points    [O, S, E]](_.show).expect[String]
  test[Arounds   [O, S, E]](_.show).expect[String]
  test[Invariants[O, S, E]](_.show).expect[String]

  // NamedOps
  test[Points    [O, S, E]](_ rename "").expectSelf
  test[Arounds   [O, S, E]](_ rename "").expectSelf
  test[Invariants[O, S, E]](_ rename "").expectSelf

  // orEmpty
  test[Option[Points    [O, S, E]]](_.orEmpty).expect[Points    [O, S, E]]
  test[Option[Arounds   [O, S, E]]](_.orEmpty).expect[Arounds   [O, S, E]]
  test[Option[Invariants[O, S, E]]](_.orEmpty).expect[Invariants[O, S, E]]

  // ===================================================================================================================
  // Actions
  // ===================================================================================================================

  // Conditional
  test[Actions[F, R, O, S, E]](_.when(_.obs.bool)).expectSelf

  // NamedOps
  test[Actions[F, R, O, S, E]](_ rename "").expectSelf

  // ActionOps1
  test[Actions[F, R, O, S, E]](_ mapO o21).expect[Actions[F, R, O2, S, E]]

  // ActionOps2
  test[Actions[F, R, O, S, E]](_ times 3).expectSelf

  // ActionOps3
  test[Actions[F, R, O, S, E]](_.group("yay")).expectSelf

  // compose (mono)
  testAA[Actions[F, R, O, S, E]](_ >> _).expect[Actions[F, R, O, S, E]]
  compileError("testAA[Actions[F, R, O, S, E]](_ +> _)")

  // combine
  test[List[Actions[F, R, O, S, E]]](_.combine).expect[Actions[F, R, O, S, E]]

  // >>
  compileError("test2[Actions[F, R, O, S, E], Points       [O, S, E]](_ >> _)")
  compileError("test2[Actions[F, R, O, S, E], Arounds      [O, S, E]](_ >> _)")
  compileError("test2[Actions[F, R, O, S, E], Invariants   [O, S, E]](_ >> _)")
  compileError("test2[Points       [O, S, E], Actions[F, R, O, S, E]](_ >> _)")
  compileError("test2[Arounds      [O, S, E], Actions[F, R, O, S, E]](_ >> _)")
  compileError("test2[Invariants   [O, S, E], Actions[F, R, O, S, E]](_ >> _)")

  // +>
                test2[Points       [O, S, E], Actions[F, R, O, S, E]](_ +> _).expect[Actions[F, R, O, S, E]]
                test2[Actions[F, R, O, S, E], Points       [O, S, E]](_ +> _).expect[Actions[F, R, O, S, E]]
                test2[Actions[F, R, O, S, E], Arounds      [O, S, E]](_ +> _).expect[Actions[F, R, O, S, E]]
  compileError("test2[Actions[F, R, O, S, E], Invariants   [O, S, E]](_ +> _)")
  compileError("test2[Arounds      [O, S, E], Actions[F, R, O, S, E]](_ +> _)")
  compileError("test2[Invariants   [O, S, E], Actions[F, R, O, S, E]](_ +> _)")

  // orEmpty
  test[Option[Actions[F, R, O, S, E]]](_.orEmpty).expect[Actions[F, R, O, S, E]]

  // ===================================================================================================================
  // Transformers
  // ===================================================================================================================

  abstract class Transformers1 {
    implicit val transformer: teststate.core.Transformer[F, R, O, S, E, F2, R2, O2, S2, E2]

    test[Points          [O, S, E]](_.lift).expect[Points            [O2, S2, E2]]
    test[Arounds         [O, S, E]](_.lift).expect[Arounds           [O2, S2, E2]]
    test[Invariants      [O, S, E]](_.lift).expect[Invariants        [O2, S2, E2]]
    test[Actions   [F, R, O, S, E]](_.lift).expect[Actions   [F2, R2, O2, S2, E2]]

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

    testExpr(i).expect[Invariants[O, S, E]]
  }

}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ImplicitsTest2 extends TestSuite {
  import teststate.Exports._

  implicit def showX2: Show[X2] =
    Show("X2=" + _.i)

  override def tests = TestSuite {

    // Doesn't affect correctness. Is just an optional nicety so don't ram it down users' throats.
    // Use toString by default and if someone wants to see something different, add their own instance.
    // There is no cost to incorrectness here.
    // This really just that I've laid things out in such a way that implicits resolve as expected.
    'show {
      def test[A](a: A)(expect: String)(implicit s: Show[A]): Unit = {
        val actual = s(a)
        assert(actual == expect)
      }
      'undef     - test(X1(9))("X1(9)")
      'defDirect - test(X2(9))("X2=9")
      'defInCO   - test(X3(9))("X3=9")
      'default   - test("a \n \u0001 \\ \" ok")("\"a \\n \\u0001 \\\\ \\\" ok\"")
    }

  }
}
