package teststate

import utest._
import teststate.data._
import teststate.core._
import teststate.run._
import teststate.typeclass._
import teststate.TestUtil._
import CoreExports._

object RunnerTest extends TestSuite {
  implicit def euqlA[A]: Equal[A] = Equal.byUnivEq

  class RecordVar(var s: Record) {
    def +=(n: String): Unit =
      s = s.copy(actions = s.actions :+ n)
  }
  case class Record(actions: Vector[String])

  val * = Dsl.sync[RecordVar, Record, Unit, String]

  val f = *.focus("Actions").value(_.obs.actions)

  def expectAt(n: Int) =
    (1 to n).map("A" + _).toVector

  def a(n: Int) = *.action("A" + n).act(_.ref += "A" + n)
    .addCheck(f.assert.equal(expectAt(n - 1)).before)
    .addCheck(f.assert.equal(expectAt(n)).after)

//  implicit class ActionExt(private val a: *.Action) extends AnyVal {
//    def assertAfter(n: String*): *.Action =
//      a.addCheck(c assertAfter n.toVector)
//  }

  val nop = *.action("NOP").act(_ => ())

  implicit val recoverToString: Recover[String] =
    Recover("Caught exception: " + _.toString)

  val test = Test(
    a(1)
    >> a(2)
    >> (a(3) >> a(4)).group("A34").addCheck(f.assert.equal(expectAt(4)).after)
  ).observe(_.s)

  def newState = new RecordVar(Record(Vector.empty))

  def testHistory(h: History[String], expect: String, normalise: String => String = identity): Unit = {
    val actual = normalise(h.format(History.Options.uncolored).trim)
    assertEq(actual = actual, normalise(expect.trim))
  }

  def delayCrash[A, B](successfulInvocations: Int)(f: A => B): A => B = {
    var i = 0
    a => {
      i += 1
      if (i <= successfulInvocations)
        f(a)
      else
        sys error "NO MORE!"
    }
  }

  override def tests = TestSuite {
    'pass {
      val v = newState
      testHistory(test.run((), v),
        """
          |✓ A1
          |✓ A2
          |✓ A34
          |✓ All pass.
        """.stripMargin)
      assertEq(actual = v.s, Record(Vector("A1", "A2", "A3", "A4")))
    }

    'catch {

      def badPoint = *.point("OMG", _ => sys error "Crash!")

      'action {
        val test = Test(*.action("A").act(_ => sys error "Crash!")).observe(_.s)
        testHistory(test.run((), newState),
          """
            |✘ A -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      'before {
        val test = Test(nop, badPoint.before).observe(_.s)
        testHistory(test.run((), newState),
          """
            |✘ Initial state.
            |  ✘ OMG -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      // Point-invariants don't distinguish between before/after.
      // All point-invariants are run both before and after actions.
      'after {
        val test = Test(nop, badPoint.after).observe(_.s)
        testHistory(test.run((), newState),
          """
            |✘ Initial state.
            |  ✘ OMG -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      'around {
        val test = Test(nop, *.focus("").value(_ => 0).testAround(_ => "what?", (_: Any, _: Any) => sys error "Crashhh!")).observe(_.s)
        testHistory(test.run((), newState),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ what? -- Caught exception: java.lang.RuntimeException: Crashhh!
          """.stripMargin)
      }

      'invariants {
        val test = Test(nop, badPoint).observe(_.s)
        testHistory(test.run((), newState),
          """
            |✘ Initial state.
            |  ✘ OMG -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      'obs1 {
        val test = Test(nop).observe(_ => sys error "NO!")
        testHistory(test.run((), newState),
          """
            |✘ Initial state.
            |  ✘ Observation -- Caught exception: java.lang.RuntimeException: NO!
          """.stripMargin)
      }

      'obs2 {
        val test = Test(nop).observe(delayCrash(1)(_.s))
        testHistory(test.run((), newState),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Observation -- Caught exception: java.lang.RuntimeException: NO MORE!
          """.stripMargin)
      }

      'obsAround {
        class Yar {
          lazy val b: Boolean = ().asInstanceOf[Boolean]
        }
        val * = Dsl.sync[Unit, Yar, Unit, String]
        val a = *.action("NOP").act(_ => ())
          .addCheck(*.focus("Blah").value(_.obs.b).assert.changeOccurs)
        val test = Test(a).observe(_ => new Yar)
        val error = "<EXPECTED>"
        def fixExpectedException(s: String): String =
          List(
            "java.lang.ClassCastException: scala.runtime.BoxedUnit cannot be cast to java.lang.Boolean",
            "java.lang.ClassCastException",
            "scala.runtime.BoxedUnit cannot be cast to java.lang.Boolean",
            "scala.scalajs.runtime.UndefinedBehaviorError: An undefined behavior was detected: undefined is not an instance of java.lang.Boolean"
          ).foldLeft(s)(_.replace(_, error))

        testHistory(test.run((), ()),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Blah shouldn't be <?>. -- Caught exception: java.lang.ClassCastException
          """.stripMargin,
          fixExpectedException)
      }

      'nextState {
        val test = Test(*.action("Merf").updateState(_ => sys error "BERF").act(_ => ())).observe(_.s)
        testHistory(test.run((), newState),
          """
            |✘ Merf
            |  ✓ Action
            |  ✘ Update expected state -- Caught exception: java.lang.RuntimeException: BERF
          """.stripMargin)
      }
    }

    'refByName {
      var i = 3
      val * = Dsl.sync[Int, Unit, Unit, String]
      val inc = *.action("inc").act(x => i = x.ref + 1)
      val h = Test(inc.times(4)).observe(_ => ()).run((), i)
      assertEq(h.failure, None)
      assertEq(i, 7)
    }

    'modS {
      var i = 9
      val * = Dsl.sync[Unit, Int, Int, String]
      val inc = *.action("inc").updateState(_ + 8).act(_ => i = i + 1).modS(_ - 3).modS(_ - 4)
      val h = Test(inc.times(3)).observe(_ => i).run(i, ())
      assertEq(h.failure, None)
      assertEq(i, 12)
    }

    'skip {
      'action {
        var i = 0
        val a = *.action("A").act(_ => i += 1).skip
        val test = Test(a: *.Action /* TODO What? */).observe(_.s)
        test.run((), newState)
        assertEq(i, 0)
      }

      'invariant {
        var i = 0
        val c = *.point("X", _ => {i += 1; None}).skip
        val test = Test(a(1), c).observe(_.s)
        test.run((), newState)
        assertEq(i, 0)
      }

      'action {
        var i = 0
        val c = *.point("X", _ => {i += 1; None}).skip
        val d = *.around("Y", _ => {i += 1; i})((_, _) => {i += 1; None}).skip
        val test = Test(a(1) addCheck c.beforeAndAfter addCheck d).observe(_.s)
        test.run((), newState)
        assertEq(i, 0)
      }
    }
  }
}
