package teststate

import teststate.Exports.{testStateAssertionSettings => _, testStateEqualByUnivEq => _, _}
import teststate.TestUtil._
import teststate.data.Failure
import utest._

object RunnerTest extends TestSuite {
  implicit def equal[A]: Equal[A] = Equal.by_==

  class RecordVar(var s: Record) {
    def +=(n: String): Unit =
      s = s.copy(actions = s.actions :+ n)
  }
  case class Record(actions: Vector[String])

  val dsl = Dsl[RecordVar, Record, Unit]

  val f = dsl.focus("Actions").value(_.obs.actions)

  def expectAt(n: Int) =
    (1 to n).map("A" + _).toVector

  def a(n: Int) = dsl.action("A" + n)(_.ref += "A" + n)
    .addCheck(f.assert.equal(expectAt(n - 1)).before)
    .addCheck(f.assert.equal(expectAt(n)).after)

//  implicit class ActionExt(private val a: dsl.Action) extends AnyVal {
//    def assertAfter(n: String*): dsl.Action =
//      a.addCheck(c assertAfter n.toVector)
//  }

  val nop = dsl.action("NOP")(_ => ())

  val test = Plan.action(
    a(1)
    >> a(2)
    >> (a(3) >> a(4)).group("A34").addCheck(f.assert.beforeAndAfter(expectAt(2), expectAt(4)))
  ).test(Observer(_.s)).stateless

  val test2 = Plan.action(
    f.assert(expectAt(0)) +> (dsl.action("A1")(_.ref += "A1") >> a(2)).group("A12") +> f.assert(expectAt(2))
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
        val test = Plan.action(dsl.action("A")(_ => throw e)).test(Observer(_.s)).stateless
        val r = test.withRef(newState).run()
        r.failureReason match {
          case Some(Failure.WithCause(_, f)) => assert(e eq f)
          case x => fail("Got: " + x)
        }
      }
      "after" - {
        val e = new RuntimeException("hurr")
        val test = Plan.action(dsl.action("A")(_ => ()) +> dsl.test("x")(_ => throw e)).test(Observer(_.s)).stateless
        val r = test.withRef(newState).run()
        r.failureReason match {
          case Some(Failure.WithCause(_, f)) => assert(e eq f)
          case x => fail("Got: " + x)
        }
      }
    }


    "catch" - {

      def badPoint = dsl.point("OMG")(_ => sys error "Crash!")

      "action" - {
        val test = Plan.action(dsl.action("A")(_ => sys error "Crash!")).test(Observer(_.s)).stateless
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
        val test = Plan(nop, dsl.focus("").value(_ => 0).testAround(_ => "what?")((_: Any, _: Any) => sys error "Crashhh!")).test(Observer(_.s)).stateless
        assertRun(test.withRef(newState).run(),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Invariants
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
        val test = Plan(nop, dsl.chooseInvariant("Who knows?!")(_ => sys error "NO!")).test(Observer(_.s)).stateless
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
          lazy val b: Boolean = throw new RuntimeException("aaaahhhh!")
        }
        val dsl = Dsl[Unit, Yar, Unit]
        val a = dsl.action("NOP")(_ => ())
          .addCheck(dsl.focus("Blah").value(_.obs.b).assert.change)
        val test = Plan.action(a).test(Observer watch new Yar).stateless

        assertRun(test.runU(),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ Blah should change. -- java.lang.RuntimeException: aaaahhhh!
            |Performed 1 action, 1 check.
          """.stripMargin)
      }

      "nextState" - {
        val a = dsl.action("Merf")(_ => ()).updateStateBy(_ => sys error "BERF")
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
      val dsl = Dsl[Int, Unit, Unit]
      val inc = dsl.action("inc")(x => i = x.ref + 1)
      val h = Plan.action(inc.times(4)).testU.stateless.withRefByName(i).run()
      assertEq(h.failure, None)
      assertEq(i, 7)
    }

    "modS" - {
      "single" - {
        var i = 9
        val dsl = Dsl[Unit, Int, Int]
        val inc = dsl.action("inc")(_ => i = i + 1)
          .updateState(_ + 8)
          .updateStateBy(_.state - 3)
          .updateState(_ - 4)
        val h = Plan.action(inc.times(3)).test(Observer watch i).withInitialState(i).runU()
        assertEq(h.failure, None)
        assertEq(i, 12)
      }

      "groupCondition" - {
        var ran1 = false
        var ran2 = false
        var stateAfter = -1
        val dsl = Dsl[Unit, Unit, Int]
        val a1 = dsl.action("A1")(_ => ran1 = true)
        val a2 = dsl.action("A2")(_ => ran2 = true)
        val a3 = dsl.action("A3")(ros => stateAfter = ros.state)
        val g = (a1 >> a2).group("G").when(_.state == 0).updateState(_ + 1)
        val h = Plan.action(g >> a3).testU.withInitialState(0).runU()
        assertEq(h.failure, None)
        assertEq(ran1, true)
        assertEq(ran2, true)
        assertEq(stateAfter, 1)
        assertEq(h.history.result, Result.Pass)
      }

      "groupConditionShortCircuit" - {
        var ran1 = false
        var ran2 = false
        var nextCondEvaluated = false
        val dsl = Dsl[Unit, Unit, Int]
        val a1 = dsl.action("A1")(_ => ran1 = true)
        val a2 = dsl.action("A2")(_ => ran2 = true)
        val g = (a1 >> a2).group("G")
          .when(_ => false)
          .when(_ => {
            nextCondEvaluated = true
            true
          })
        val h = Plan.action(g).testU.withInitialState(0).runU()
        assertEq(h.failure, None)
        assertEq(ran1, false)
        assertEq(ran2, false)
        assertEq(nextCondEvaluated, false)
        assertEq(h.history.result, Result.Skip)
      }

      "groupPostObservation" - {
        var obs = "before"
        var ran = false
        var stateAfter = -1
        val dsl = Dsl[Unit, String, Int]
        val a1 = dsl.action("A1")(_ => {
          ran = true
          obs = "after"
        })
        val a2 = dsl.action("A2")(_ => ())
        val a3 = dsl.action("A3")(ros => stateAfter = ros.state)
        val g = (a1 >> a2).group("G").updateStateAttemptBy(os =>
          if (os.obs == "after") Right(os.state + 1)
          else Left(s"Expected post-obs 'after', got '${os.obs}'")
        )
        val h = Plan.action(g >> a3).test(Observer watch obs).withInitialState(0).runU()
        assertEq(h.failure, None)
        assertEq(ran, true)
        assertEq(stateAfter, 1)
        assertEq(h.history.result, Result.Pass)
      }
    }

    "skip" - {
      "action" - {
        var i = 0
        val a = dsl.action("A")(_ => i += 1).skip
        val test = Plan.action(a).test(Observer(_.s)).stateless
        test.withRef(newState).run()
        assertEq(i, 0)
      }

      "invariant" - {
        var i = 0
        val c = dsl.point("X")(_ => {i += 1; None}).skip
        val test = Plan(a(1), c).test(Observer(_.s)).stateless
        test.withRef(newState).run()
        assertEq(i, 0)
      }

      "action" - {
        var i = 0
        val c = dsl.point("X")(_ => {i += 1; None}).skip
        val d = dsl.around("Y")(_ => {i += 1; i})((_, _) => {i += 1; None}).skip
        val test = Plan.action(a(1) addCheck c.beforeAndAfter addCheck d).test(Observer(_.s)).stateless
        test.withRef(newState).run()
        assertEq(i, 0)
      }
    }

    "choice" - {
      "invariant" - {
        var v = true
        val dsl = Dsl[Unit, Boolean, Boolean]
        val a = dsl.action("A")(_ => v = !v).updateState(!_)
        val i00 = dsl.test("IFF")(_ => true)
        val i11 = dsl.test("ITT")(_ => true)
        val i01 = dsl.test("IFT")(_ => false)
        val i10 = dsl.test("ITF")(_ => false)
        val i = dsl.chooseInvariant("I")(x => (x.obs, x.state) match {
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
      val dsl = Dsl[Unit, Unit, Unit]
      val plan = Plan.action(
        dsl.test("Before")(_ => i == 0)
          +> dsl.action("Inc")(_ => i += 1)
          +> dsl.test("After")(_ => i == 1))

      val test = plan.stateless.testU
      assertRun(test.runU(),
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

    "deltaInvariantReporting" - {
      "onlyDelta" - {
        var i = 0
        val dsl = Dsl[Unit, Unit, Unit]
        val action = dsl.action("Inc")(_ => i += 1)
        val deltaInv = dsl.testAround("InvDelta")((_, _) => true)
        val plan = Plan.action(action).addInvariants(deltaInv)
        val test = plan.stateless.testU
        assertRun(test.runU(),
          """
            |✓ Inc
            |  ✓ Action
            |  ✓ Invariants
            |    ✓ InvDelta
            |✓ All pass.
            |Performed 1 action, 1 check.
          """.stripMargin)
      }

      "withPostCond" - {
        var i = 0
        val dsl = Dsl[Unit, Unit, Unit]
        val action = dsl.action("Inc")(_ => i += 1) +> dsl.test("PostCond")(_ => true)
        val deltaInv = dsl.testAround("InvDelta")((_, _) => true)
        val plan = Plan.action(action).addInvariants(deltaInv)
        val test = plan.stateless.testU
        assertRun(test.runU(),
          """
            |✓ Inc
            |  ✓ Action
            |  ✓ Post-conditions
            |    ✓ PostCond
            |  ✓ Invariants
            |    ✓ InvDelta
            |✓ All pass.
            |Performed 1 action, 2 checks.
          """.stripMargin)
      }

      "aroundPreRunsBeforeAction" - {
        var state = 0
        var actionRan = false
        val dsl = Dsl[Unit, Int, Unit]
        val check = dsl.around("Recorded pre-state")(os => {
          assert(!actionRan)
          os.obs
        })((os, before) => {
          assert(actionRan)
          if (before == 0 && os.obs == 1) None else Some(s"Expected 0 -> 1, got $before -> ${os.obs}")
        })
        val action = dsl.action("Inc")(_ => {
          actionRan = true
          state = 1
        }) +> check
        val plan = Plan.action(action)
        val test = plan.stateless.test(Observer(_ => state))
        val report = test.runU()
        assert(report.result == Result.Pass)
      }
    }

    "callbacks" - {
      "before" - {
        var events = Vector.empty[String]
        val cb = dsl.runCallbacks.aroundActions.before((_, name) => events :+= s"before:${name.value}")
        val actOk = dsl.action("ok")(_ => events :+= "run:ok")
        val actCrash = dsl.action("crash")(_ => { events :+= "run:crash"; sys.error("boom") })
        Plan.action(actOk >> actCrash).test(Observer(_.s)).stateless.withRef(newState).withCallbacks(cb).run()
        assertEq(events, Vector(
          "before:ok",
          "run:ok",
          "before:crash",
          "run:crash",
        ))
      }

      "after" - {
        var events = Vector.empty[(String, Option[String])]
        val cb = dsl.runCallbacks.aroundActions.after((_, name, err) => events :+= (name.value -> err))
        val actOk = dsl.action("ok")(_ => ())
        val actFail = dsl.action("fail").attempt(_ => Some("err"))
        val actCrash = dsl.action("crash")(_ => sys.error("boom"))
        Plan.action(actOk >> actFail).test(Observer(_.s)).stateless.withRef(newState).withCallbacks(cb).run()
        Plan.action(actCrash).test(Observer(_.s)).stateless.withRef(newState).withCallbacks(cb).run()
        assertEq(events, Vector(
          "ok" -> None,
          "fail" -> Some("err"),
          "crash" -> Some("java.lang.RuntimeException: boom"),
        ))
      }
    }

    "singlePrepare" - {
      import scala.concurrent.duration._

      "whenException" - {
        val d = Dsl[Unit, Unit, Unit]
        val act = d.action("act")(_ => ()).when(_ => sys.error("when boom"))
        val report = Plan.action(act).testU.stateless.runU()
        assert(report.failed)
        assertEq(report.failure.map(_.failure), Some("java.lang.RuntimeException: when boom"))
      }

      "pmapRException" - {
        val d = Dsl[Unit, Unit, Unit]
        val act = d.action("act")(_ => ()).pmapR[Unit](_ => sys.error("pmapR boom"))
        val report = Plan.action(act).testU.stateless.runU()
        assert(report.failed)
        assertEq(report.failure.map(_.failure), Some("java.lang.RuntimeException: pmapR boom"))
      }

      "evaluatedOnceWhenNoRetry" - {
        var calls = 0
        val d = Dsl[Unit, Unit, Unit]
        val act = d.action("act")(_ => ()).when { _ =>
          calls += 1
          true
        }
        val report = Plan.action(act).testU.stateless.runU()
        assert(!report.failed)
        assertEq(calls, 1)
      }

      "exceptionAfterPreCheckRetry" - {
        var obsCall = 0
        val d = Dsl[Unit, Int, Unit]
        val obs = Observer[Unit, Int](_ => {
          obsCall += 1
          obsCall
        })
        val v = d.focus("v").value(_.obs)
        val act = (d.action("act")(_ => ()) <+ v.assert(2)).when { ros =>
          if (ros.obs == 1) true
          else sys.error("when retry boom")
        }
        val policy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 1)
        val report = Plan.action(act).test(obs).stateless.withRetryPolicy(policy).runU()
        assert(report.failed)
        assertEq(report.failure.map(_.failure), Some("java.lang.RuntimeException: when retry boom"))
      }

      "exceptionWithRetryAndObsFail" - {
        var obsCall = 0
        val d = Dsl[Unit, Int, Unit]
        val obs = Observer.attempt[Unit, Int, String] { _ =>
          obsCall += 1
          if (obsCall == 1) Right(1) else Left("obs err")
        }
        val act = d.action("act")(_ => ()).when(_ => sys.error("when boom"))
        val policy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 1)
        val report = Plan.action(act).test(obs).stateless.withRetryPolicy(policy).runU()
        assert(report.failed)
        assertEq(report.failure.map(_.failure), Some("java.lang.RuntimeException: when boom"))
      }
    }

  }
}
