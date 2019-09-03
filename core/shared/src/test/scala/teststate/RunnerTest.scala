package teststate

import utest._
import teststate.data.Failure
import teststate.Exports.{testStateAssertionSettings => _, testStateEqualByUnivEq => _, _}
import teststate.TestUtil._

object RunnerTest extends TestSuite {
  implicit def equal[A]: Equal[A] = Equal.by_==

  class RecordVar(var s: Record) {
    def +=(n: String): Unit =
      s = s.copy(actions = s.actions :+ n)
  }
  case class Record(actions: Vector[String])

  val * = Dsl[RecordVar, Record, Unit]

  val f = *.focus("Actions").value(_.obs.actions)

  def expectAt(n: Int) =
    (1 to n).map("A" + _).toVector

  def a(n: Int) = *.action("A" + n)(_.ref += "A" + n)
    .addCheck(f.assert.equal(expectAt(n - 1)).before)
    .addCheck(f.assert.equal(expectAt(n)).after)

//  implicit class ActionExt(private val a: *.Action) extends AnyVal {
//    def assertAfter(n: String*): *.Action =
//      a.addCheck(c assertAfter n.toVector)
//  }

  val nop = *.action("NOP")(_ => ())

  val test = Plan.action(
    a(1)
    >> a(2)
    >> (a(3) >> a(4)).group("A34").addCheck(f.assert.beforeAndAfter(expectAt(2), expectAt(4)))
  ).test(Observer(_.s)).stateless

  val test2 = Plan.action(
    f.assert(expectAt(0)) +> (*.action("A1")(_.ref += "A1") >> a(2)).group("A12") +> f.assert(expectAt(2))
  ).test(Observer(_.s)).stateless

  def newState = new RecordVar(Record(Vector.empty))

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

  override def tests = Tests {
    "pass1" - {
      val v = newState
      assertRun(test.withRef(v).run(),
        """
          |✓ A1
          |✓ A2
          |✓ A34
          |✓ All pass.
          |Performed 4 actions, 10 checks.
        """.stripMargin, showChildren = false)
      assertEq(actual = v.s, Record(Vector("A1", "A2", "A3", "A4")))
    }

    "pass2" - {
      val v = newState
      assertRun(test2.withRef(v).run(),
        """
          |✓ A12
          |  ✓ Pre-conditions
          |    ✓ Actions should be .
          |  ✓ A1
          |  ✓ A2
          |    ✓ Pre-conditions
          |      ✓ Actions should be "A1".
          |    ✓ Action
          |    ✓ Post-conditions
          |      ✓ Actions should be "A1", "A2".
          |  ✓ Post-conditions
          |    ✓ Actions should be "A1", "A2".
          |✓ All pass.
          |Performed 2 actions, 4 checks.
        """.stripMargin, showChildren = true)
      assertEq(actual = v.s, Record(Vector("A1", "A2")))
    }

    "failureReason" - {
      "action" - {
        val e = new RuntimeException("hurr")
        val test = Plan.action(*.action("A")(_ => throw e)).test(Observer(_.s)).stateless
        val r = test.withRef(newState).run()
        r.failureReason match {
          case Some(Failure.WithCause(_, f)) => assert(e eq f)
          case x => fail("Got: " + x)
        }
      }
      "after" - {
        val e = new RuntimeException("hurr")
        val test = Plan.action(*.action("A")(_ => ()) +> *.test("x")(_ => throw e)).test(Observer(_.s)).stateless
        val r = test.withRef(newState).run()
        r.failureReason match {
          case Some(Failure.WithCause(_, f)) => assert(e eq f)
          case x => fail("Got: " + x)
        }
      }
    }


    "catch" - {

      def badPoint = *.point("OMG")(_ => sys error "Crash!")

      "action" - {
        val test = Plan.action(*.action("A")(_ => sys error "Crash!")).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ A -- java.lang.RuntimeException: Crash!
            |Performed 1 action, 0 checks.
          """.stripMargin)
      }

      "before" - {
        val test = Plan(nop, badPoint.before).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ Initial state.
            |  ✘ OMG -- java.lang.RuntimeException: Crash!
            |Performed 0 actions, 1 check.
          """.stripMargin)
      }

      // Point-invariants don't distinguish between before/after.
      // All point-invariants are run both before and after actions.
      "after" - {
        val test = Plan(nop, badPoint.after).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ Initial state.
            |  ✘ OMG -- java.lang.RuntimeException: Crash!
            |Performed 0 actions, 1 check.
          """.stripMargin)
      }

      "around" - {
        val test = Plan(nop, *.focus("").value(_ => 0).testAround(_ => "what?")((_: Any, _: Any) => sys error "Crashhh!")).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ what? -- java.lang.RuntimeException: Crashhh!
            |Performed 1 action, 1 check.
          """.stripMargin)
      }

      "invariants" - {
        val test = Plan(nop, badPoint).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ Initial state.
            |  ✘ OMG -- java.lang.RuntimeException: Crash!
            |Performed 0 actions, 1 check.
          """.stripMargin)
      }

      "coproduct" - {
        val test = Plan(nop, *.chooseInvariant("Who knows?!")(_ => sys error "NO!")).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
        """
          |✘ Initial state.
          |  ✘ Who knows?! -- java.lang.RuntimeException: NO!
          |Performed 0 actions, 1 check.
        """.stripMargin)
      }

      "obs1" - {
        val test = Plan.action(nop).test(Observer watch (sys error "NO!")).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ Initial state.
            |  ✘ Observation -- java.lang.RuntimeException: NO!
            |Performed 0 actions, 0 checks.
          """.stripMargin)
      }

      "obs2" - {
        val test = Plan.action(nop).test(Observer(delayCrash(1)(_.s))).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Observation -- java.lang.RuntimeException: NO MORE!
            |Performed 1 action, 0 checks.
          """.stripMargin)
      }

      "obsAround" - {
        class Yar {
          lazy val b: Boolean = ().asInstanceOf[Boolean]
        }
        val * = Dsl[Unit, Yar, Unit]
        val a = *.action("NOP")(_ => ())
          .addCheck(*.focus("Blah").value(_.obs.b).assert.change)
        val test = Plan.action(a).test(Observer watch new Yar).stateless
        val error = "<EXPECTED>"
        def fixExpectedException(s: String): String =
          List(
            "java.lang.ClassCastException: scala.runtime.BoxedUnit cannot be cast to java.lang.Boolean",
            "java.lang.ClassCastException",
            "scala.runtime.BoxedUnit cannot be cast to java.lang.Boolean",
            "scala.scalajs.runtime.UndefinedBehaviorError: An undefined behavior was detected: undefined is not an instance of java.lang.Boolean"
          ).foldLeft(s)(_.replace(_, error))

        assertRun(test.runU,
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Blah should change. -- java.lang.ClassCastException
            |Performed 1 action, 1 check.
          """.stripMargin,
          normalise = fixExpectedException)
      }

      "nextState" - {
        val a = *.action("Merf")(_ => ()).updateStateBy(_ => sys error "BERF")
        val test = Plan.action(a).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ Merf
            |  ✓ Action
            |  ✘ Update expected state -- java.lang.RuntimeException: BERF
            |Performed 1 action, 0 checks.
          """.stripMargin)
      }
    }

    "refByName" - {
      var i = 3
      val * = Dsl[Int, Unit, Unit]
      val inc = *.action("inc")(x => i = x.ref + 1)
      val h = Plan.action(inc.times(4)).testU.stateless.withRefByName(i).run()
      assertEq(h.failure, None)
      assertEq(i, 7)
    }

    "modS" - {
      var i = 9
      val * = Dsl[Unit, Int, Int]
      val inc = *.action("inc")(_ => i = i + 1)
        .updateState(_ + 8)
        .updateStateBy(_.state - 3)
        .updateState(_ - 4)
      val h = Plan.action(inc.times(3)).test(Observer watch i).withInitialState(i).runU()
      assertEq(h.failure, None)
      assertEq(i, 12)
    }

    "skip" - {
      "action" - {
        var i = 0
        val a = *.action("A")(_ => i += 1).skip
        val test = Plan.action(a).test(Observer(_.s)).stateless
        test.withRef(newState).run()
        assertEq(i, 0)
      }

      "invariant" - {
        var i = 0
        val c = *.point("X")(_ => {i += 1; None}).skip
        val test = Plan(a(1), c).test(Observer(_.s)).stateless
        test.withRef(newState).run()
        assertEq(i, 0)
      }

      "action" - {
        var i = 0
        val c = *.point("X")(_ => {i += 1; None}).skip
        val d = *.around("Y")(_ => {i += 1; i})((_, _) => {i += 1; None}).skip
        val test = Plan.action(a(1) addCheck c.beforeAndAfter addCheck d).test(Observer(_.s)).stateless
        test.withRef(newState).run()
        assertEq(i, 0)
      }
    }

    "choice" - {
      "invariant" - {
        var v = true
        val * = Dsl[Unit, Boolean, Boolean]
        val a = *.action("A")(_ => v = !v).updateState(!_)
        val i00 = *.test("IFF")(_ => true)
        val i11 = *.test("ITT")(_ => true)
        val i01 = *.test("IFT")(_ => false)
        val i10 = *.test("ITF")(_ => false)
        val i = *.chooseInvariant("I")(x => (x.obs, x.state) match {
          case (true,  true ) => i11
          case (false, false) => i00
          case (true , false) => i10
          case (false, true ) => i01
        })
        val t = Plan(a >> a, i).test(Observer watch v)

        v = true
        assertRun(t.runU(v),
          """
            |✓ Initial state.
            |  ✓ ITT
            |✓ A
            |  ✓ Action
            |  ✓ Invariants
            |    ✓ IFF
            |✓ A
            |  ✓ Action
            |  ✓ Invariants
            |    ✓ ITT
            |✓ All pass.
            |Performed 2 actions, 3 checks.
          """.stripMargin)

        v = false
        assertRun(t.runU(v),
          """
            |✓ Initial state.
            |  ✓ IFF
            |✓ A
            |  ✓ Action
            |  ✓ Invariants
            |    ✓ ITT
            |✓ A
            |  ✓ Action
            |  ✓ Invariants
            |    ✓ IFF
            |✓ All pass.
            |Performed 2 actions, 3 checks.
          """.stripMargin)
      }
    }

    "realOrder" - {
      var i = 0
      val * = Dsl[Unit, Unit, Unit]
      val plan = Plan.action(
        *.test("Before")(_ => i == 0)
          +> *.action("Inc")(_ => i += 1)
          +> *.test("After")(_ => i == 1))

      val test = plan.stateless.testU
      assertRun(test.runU,
        """
          |✓ Inc
          |  ✓ Pre-conditions
          |    ✓ Before
          |  ✓ Action
          |  ✓ Post-conditions
          |    ✓ After
          |✓ All pass.
          |Performed 1 action, 2 checks.
        """.stripMargin)
    }

  }
}
