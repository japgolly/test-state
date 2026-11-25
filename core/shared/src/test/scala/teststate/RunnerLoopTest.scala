package teststate

import java.time.{Duration => JavaDuration, Instant}
import scala.concurrent.duration.{Duration => ScalaDuration}
import teststate.Exports.{testStateAssertionSettings => _, testStateEqualByUnivEq => _, _}
import teststate.TestUtil._
import teststate.core.LoopStep
import teststate.data.{Id, Left => OrLeft, Or, Right => OrRight}
import utest._

object RunnerLoopTest extends TestSuite {

  private type Thunk[A] = () => A

  private final class VirtualClock(initialMs: Long = 1000L, onSchedule: Long => Unit = _ => ()) {
    var nowMs: Long = initialMs
    var schedules: Vector[(Instant, Instant)] = Vector.empty

    implicit val em: ExecutionModel[Thunk] = new ExecutionModel[Thunk] {
      override def point[A](a: => A): Thunk[A] = () => a
      override def pure[A](a: A): Thunk[A] = () => a
      override def map[A, B](fa: Thunk[A])(f: A => B): Thunk[B] = () => f(fa())
      override def flatMap[A, B](fa: Thunk[A])(f: A => Thunk[B]): Thunk[B] = () => f(fa())()
      override def tailrec[A, B](a: A)(rec: A => Thunk[A Or B]): Thunk[B] = () =>
        ExecutionModel.Immediate.tailrec[A, B](a)(x => rec(x)())
      override def tailrecA[A](a: A)(stop: A => Boolean)(rec: A => Thunk[A]): Thunk[A] = () =>
        ExecutionModel.Immediate.tailrecA[A](a)(stop)(x => rec(x)())
      override def recover[E, A](f: => Thunk[Failure[E] Or A])(implicit attempt: ErrorHandler[E]): Thunk[Failure[E] Or A] = () =>
        attempt.recover(f(), OrLeft(_))
      override def now: Thunk[Instant] = () => Instant.ofEpochMilli(nowMs)
      override def schedule[A](task: => Thunk[A], startAt: Instant): Thunk[A] = () => {
        val before = Instant.ofEpochMilli(nowMs)
        schedules :+= (before -> startAt)
        val targetMs = startAt.toEpochMilli
        val delta =
          if (targetMs > nowMs) {
            val d = targetMs - nowMs
            nowMs = targetMs
            d
          } else
            0L
        onSchedule(delta)
        task()
      }
      override def doFinally[A, B](main: => Thunk[A], last: => Thunk[B]): Thunk[A] = () =>
        try main() finally { val _ = last() }
    }
  }

  private final class Counter(var value: Int = 0) {
    var log: Vector[String] = Vector.empty
    def inc(label: String = "inc"): Unit = {
      value += 1
      log :+= label
    }
  }

  private def assertSleepStep[E: UnivEq](step: Report.History.Step[Failure[E]], expectedResult: Result[Failure[E]] = Result.Pass): Unit = {
    assert(step.name.value.toLowerCase.contains("sleep"))
    assertEq(step.result, expectedResult)
  }

  override def tests = Tests {

    "basic" - {
      "immediateBreak" - {
        var afterRan = false
        val dsl = Dsl[Counter, Int, Int]
        val loop = dsl.loop("L")(_ => dsl.loop.break)
        val after = dsl.action("After")(_ => afterRan = true)
        val c = new Counter(0)

        val r1 = Plan.action(loop).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertRun(r1,
          """
            |✓ L
            |✓ All pass.
            |Performed 0 actions, 0 checks.
          """.stripMargin)
        assertEq(r1.result, Result.Pass)
        assertEq(r1.history.steps.head.children.steps.length, 0)

        val r2 = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertEq(r2.result, Result.Pass)
        assertEq(afterRan, true)
      }

      "actThenBreak" - {
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val loop = dsl.loop("Count to 3") { ros =>
          if (ros.obs < 3) dsl.loop.act(inc) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()

        assertRun(r,
          """
            |✓ Count to 3
            |  ✓ Inc
            |  ✓ Inc
            |  ✓ Inc
            |✓ All pass.
            |Performed 3 actions, 0 checks.
          """.stripMargin)
        assertEq(c.value, 3)
        assertEq(c.log, Vector("inc", "inc", "inc"))
      }

      "multiActionPerIteration" - {
        val dsl = Dsl[Counter, Int, Int]
        val a1 = dsl.action("A1")(_.ref.inc("A1")).updateState(_ + 1)
        val a2 = dsl.action("A2")(_.ref.inc("A2")).updateState(_ + 10)
        var finalState = -1
        val after = dsl.action("After")(ros => finalState = ros.state)
        val loop = dsl.loop("Pairs") { ros =>
          if (ros.obs < 4) dsl.loop.act(a1 >> a2) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(0).withRef(c).run()

        assertRun(r,
          """
            |✓ Pairs
            |  ✓ A1
            |  ✓ A2
            |  ✓ A1
            |  ✓ A2
            |✓ After
            |✓ All pass.
            |Performed 5 actions, 0 checks.
          """.stripMargin)
        assertEq(c.value, 4)
        assertEq(c.log, Vector("A1", "A2", "A1", "A2"))
        assertEq(finalState, 22)
      }

      "emptyAct" - {
        var iterations = 0
        val dsl = Dsl[Unit, Unit, Unit]
        val loop = dsl.loop("EmptyAct") { _ =>
          iterations += 1
          if (iterations < 3) dsl.loop.act(dsl.emptyAction) else dsl.loop.break
        }
        val r = Plan.action(loop).testU.stateless.runU()
        assertEq(r.result, Result.Pass)
        assertEq(iterations, 3)
        assertEq(r.stats.performedActions, 0)
      }

      "actWithSkippedInnerAction" - {
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val skipped = dsl.action("Never")(_.ref.inc("never")).skip
        val loop = dsl.loop("WithSkip") { ros =>
          if (ros.obs < 2) dsl.loop.act(inc >> skipped) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✓ WithSkip
            |  ✓ Inc
            |  - Never
            |  ✓ Inc
            |  - Never
            |✓ All pass.
            |Performed 2 actions, 0 checks.
          """.stripMargin)
        assertEq(c.value, 2)
      }

      "actWithGroupAndChoice" - {
        val dsl = Dsl[Counter, Int, Unit]
        val a1 = dsl.action("A1")(_.ref.inc("A1"))
        val a2 = dsl.action("A2")(_.ref.inc("A2"))
        val grp = (a1 >> a2).group("Pair")
        val chosen = dsl.chooseAction("Pick")(ros => if (ros.obs == 0) grp else a1)
        val loop = dsl.loop("Complex") { ros =>
          if (ros.obs < 3) dsl.loop.act(chosen) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✓ Complex
            |  ✓ Pair
            |    ✓ A1
            |    ✓ A2
            |  ✓ A1
            |✓ All pass.
            |Performed 3 actions, 0 checks.
          """.stripMargin)
        assertEq(c.log, Vector("A1", "A2", "A1"))
      }

      "refByName" - {
        var refVar = 0
        var seenRefs = Vector.empty[Int]
        val dsl = Dsl[Int, Unit, Unit]
        val bump = dsl.action("Bump")(ros => refVar = ros.ref + 1)
        val loop = dsl.loop("RefByName") { ros =>
          seenRefs :+= ros.ref
          if (ros.ref < 3) dsl.loop.act(bump) else dsl.loop.break
        }
        val r = Plan.action(loop).testU.stateless.withRefByName(refVar).run()
        assertEq(r.result, Result.Pass)
        assertEq(refVar, 3)
        assertEq(seenRefs, Vector(0, 1, 2, 3))
      }

      "sequentialLoops" - {
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 1)
        val loop1 = dsl.loop("Loop1")(ros => if (ros.state < 2) dsl.loop.act(inc) else dsl.loop.break)
        val loop2 = dsl.loop("Loop2")(ros => if (ros.state < 5) dsl.loop.act(inc) else dsl.loop.break)
        val c = new Counter(0)
        val r = Plan.action(loop1 >> loop2).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(c.value, 5)
        assertEq(r.stats.performedActions, 5)
      }

      "nestedLoops" - {
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 1)
        val inner = dsl.loop("Inner") { ros =>
          if (ros.obs % 2 != 0 || ros.state == 0) dsl.loop.act(inc) else dsl.loop.break
        }
        val outer = dsl.loop("Outer") { ros =>
          if (ros.obs < 4) dsl.loop.act(inc >> inner) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(outer).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertRun(r,
          """
            |✓ Outer
            |  ✓ Inc
            |  ✓ Inner
            |    ✓ Inc
            |  ✓ Inc
            |  ✓ Inner
            |    ✓ Inc
            |✓ All pass.
            |Performed 4 actions, 0 checks.
          """.stripMargin)
        assertEq(c.value, 4)
      }

      "subTestInsideLoop" - {
        val dsl = Dsl[Counter, Int, Int]
        val obsEqState = dsl.focus("Val").obsAndState(identity, identity).assert.equal
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 1)
        val sub = Plan(inc, obsEqState).asAction("Sub")
        val loop = dsl.loop("LoopSub")(ros => if (ros.obs < 2) dsl.loop.act(sub) else dsl.loop.break)
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(c.value, 2)
        assertEq(r.stats.performedActions, 2)
        assertEq(r.stats.performedChecks, 4)
      }

      "loopInsideSubTest" - {
        val dsl = Dsl[Counter, Int, Int]
        val obsEqState = dsl.focus("Val").obsAndState(identity, identity).assert.equal
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 1)
        val loop = dsl.loop("InnerLoop")(ros => if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.break)
        val sub = Plan(loop, obsEqState).asAction("Sub")
        val c = new Counter(0)
        val r = Plan.action(sub).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(c.value, 2)
        assertEq(r.stats.performedActions, 2)
      }

      "timesOnLoop" - {
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 1)
        var target = 0
        val bumpTarget = dsl.action("SetTarget")(_ => target += 2)
        val loop = dsl.loop("ReachTarget")(ros => if (ros.obs < target) dsl.loop.act(inc) else dsl.loop.break)
        val c = new Counter(0)
        val r = Plan.action((bumpTarget >> loop).times(2)).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(c.value, 4)
        assertEq(r.stats.performedActions, 6)
      }

      "contextualLoopName" - {
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val nameFn = NameFn[dsl.ROS](o => s"Loop(start=${o.fold("?")(_.obs.toString)})")
        val loop = dsl.loop(nameFn)(ros => if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.break)
        val c = new Counter(1)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✓ Loop(start=1)
            |  ✓ Inc
            |✓ All pass.
            |Performed 1 action, 0 checks.
          """.stripMargin)
      }
    }

    "state" - {
      "actUpdatesState" - {
        var observedStates = Vector.empty[Int]
        var stateAfter = -1
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 10)
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("StateLoop") { ros =>
          observedStates :+= ros.state
          if (ros.obs < 3) dsl.loop.act(inc) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(1).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(observedStates, Vector(1, 11, 21, 31))
        assertEq(stateAfter, 31)
      }

      "setState" - {
        var observedStates = Vector.empty[Int]
        var stateAfter = -1
        val dsl = Dsl[Unit, Unit, Int]
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("SetState") { ros =>
          observedStates :+= ros.state
          if (ros.state < 3) dsl.loop.setState(ros.state + 1) else dsl.loop.break
        }
        val r = Plan.action(loop >> after).testU.withInitialState(0).runU()
        assertEq(r.result, Result.Pass)
        assertEq(observedStates, Vector(0, 1, 2, 3))
        assertEq(stateAfter, 3)
      }

      "setStateAndBreak" - {
        var stateAfter = -1
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc())
        val stateFocus = dsl.focus("state").value(_.state)
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("BreakState") { ros =>
          if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.setStateAndBreak(99)
        } +> stateFocus.assert(99)
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(stateAfter, 99)
        assertEq(r.stats.performedActions, 2)
        assertEq(r.stats.performedChecks, 1)
      }

      "setStateAndBreakImmediate" - {
        var stateAfter = -1
        val dsl = Dsl[Unit, Unit, Int]
        val stateFocus = dsl.focus("state").value(_.state)
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("ImmediateBreakState")(_ => dsl.loop.setStateAndBreak(42)) +> stateFocus.assert(42)
        val r = Plan.action(loop >> after).testU.withInitialState(0).runU()
        assertEq(r.result, Result.Pass)
        assertEq(stateAfter, 42)
      }

      "breakModStateLeft" - {
        var postRan = false
        var afterRan = false
        val dsl = Dsl[Unit, Unit, Int]
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = dsl.loop("BadBreak")(_ => LoopStep.Break[Id, Unit, Unit, Int, String](Some(_ => OrLeft("bad break state")))) +> post
        val r = Plan.action(loop >> after).testU.withInitialState(0).runU()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("bad break state")))
        assertEq(postRan, false)
        assertEq(afterRan, false)
      }

      "breakModStateException" - {
        val boom = new RuntimeException("break state boom")
        var postRan = false
        val dsl = Dsl[Unit, Unit, Int]
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val loop = dsl.loop("ThrowBreak")(_ => LoopStep.Break[Id, Unit, Unit, Int, String](Some(_ => throw boom))) +> post
        val r = Plan.action(loop).testU.withInitialState(0).runU()
        assert(r.failed)
        r.failure match {
          case Some(Failure.WithCause(_, cause)) => assert(cause eq boom)
          case other                             => fail("Expected WithCause, got: " + other)
        }
        assertEq(postRan, false)
      }

      "outerUpdateStateAppliedOnceOnFinish" - {
        val vc = new VirtualClock()
        import vc.em
        var world = 0
        var stateAfter = -1
        val dsl = Dsl.full[Thunk, Unit, Int, Int, String]
        val bump = dsl.action("Bump")(_ => () => { world += 1 })
        val after = dsl.action("After")(ros => () => { stateAfter = ros.state })
        val loop = dsl.loop("OuterMod") { ros =>
          if (ros.obs == 0) dsl.loop.act(bump)
          else if (ros.obs == 1) {
            world += 1
            dsl.loop.sleepMs(10)
          } else if (ros.obs == 2) dsl.loop.act(bump)
          else dsl.loop.break
        }.updateState(_ + 100)

        val r = Plan.action(loop >> after).test(Observer(_ => world)).withInitialState(5).withoutRef.run()()
        assertEq(r.result, Result.Pass)
        assertEq(world, 3)
        // Applied ONCE on finish: 5 + 100 = 105 (NOT once per Act/Sleep/Break)
        assertEq(stateAfter, 105)
      }

      "outerUpdateStateComposesWithSetStateAndBreak" - {
        var stateAfter = -1
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc())
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("ComposeBreak") { ros =>
          if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.setStateAndBreak(10)
        }.updateState(_ * 3)
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(1).withRef(c).run()
        assertEq(r.result, Result.Pass)
        // setStateAndBreak(10) then outer (* 3) => 30
        assertEq(stateAfter, 30)
      }

      "outerUpdateStateBySeesFinalObs" - {
        var stateAfter = -1
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc())
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("FinalObs") { ros =>
          if (ros.obs < 4) dsl.loop.act(inc) else dsl.loop.break
        }.updateStateBy(os => os.obs * 10 + os.state)
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(2).withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(stateAfter, 42)
      }

      "outerUpdateStateAttemptFail" - {
        var afterRan = false
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc())
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = dsl.loop("FailMod") { ros =>
          if (ros.obs < 1) dsl.loop.act(inc) else dsl.loop.break
        }.updateStateAttempt(_ => OrLeft("outer mod failed"))
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("outer mod failed")))
        assertEq(c.value, 1)
        assertEq(afterRan, false)
      }

      "outerUpdateStateNotAppliedWhenSkipped" - {
        var stateAfter = -1
        val dsl = Dsl[Unit, Unit, Int]
        val after = dsl.action("After")(ros => stateAfter = ros.state)
        val loop = dsl.loop("Skipped")(_ => dsl.loop.break).when(_ => false).updateState(_ + 100)
        val r = Plan.action(loop >> after).testU.withInitialState(7).runU()
        assertEq(r.result, Result.Pass)
        assertEq(stateAfter, 7)
      }

      "outerUpdateStateNotAppliedOnFail" - {
        var modCalled = false
        val dsl = Dsl[Unit, Unit, Int]
        val loop = dsl.loop("FailLoop")(_ => dsl.loop.fail("nope")).updateState { s =>
          modCalled = true
          s + 100
        }
        val r = Plan.action(loop).testU.withInitialState(7).runU()
        assert(r.failed)
        assertEq(modCalled, false)
      }
    }

    "sleep" - {
      "schedulesAndReObserves" - {
        var scheduled = 0
        val vc = new VirtualClock(initialMs = 5000L, onSchedule = { _ => scheduled += 1 })
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
        var seenObs = Vector.empty[Int]
        val loop = dsl.loop("WaitForReady") { ros =>
          seenObs :+= ros.obs
          ros.obs match {
            case 0 => dsl.loop.sleepMs(100)
            case 1 => dsl.loop.sleep(JavaDuration.ofMillis(250))
            case _ => dsl.loop.break
          }
        }
        val r = Plan.action(loop).test(Observer(_ => scheduled)).stateless.withoutRef.run()()
        assertEq(r.result, Result.Pass)
        assertEq(seenObs, Vector(0, 1, 2))
        assertEq(vc.schedules, Vector(
          Instant.ofEpochMilli(5000L) -> Instant.ofEpochMilli(5100L),
          Instant.ofEpochMilli(5100L) -> Instant.ofEpochMilli(5350L),
        ))
        assertEq(vc.nowMs, 5350L)

        // Sleep steps are reported as flat children under the loop step; Break is not
        val loopStep = r.history.steps.head
        assertEq(loopStep.name.value, "WaitForReady")
        assertEq(loopStep.result, Result.Pass)
        assertEq(loopStep.children.steps.length, 2)
        assertSleepStep(loopStep.children.steps(0))
        assertSleepStep(loopStep.children.steps(1))
      }

      "zeroDuration" - {
        var scheduled = 0
        val vc = new VirtualClock(initialMs = 1000L, onSchedule = { _ => scheduled += 1 })
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
        val loop = dsl.loop("ZeroSleep") { ros =>
          if (ros.obs == 0) dsl.loop.sleep(JavaDuration.ZERO) else dsl.loop.break
        }
        val r = Plan.action(loop).test(Observer(_ => scheduled)).stateless.withoutRef.run()()
        assertEq(r.result, Result.Pass)
        assertEq(scheduled, 1)
        assertEq(vc.schedules, Vector(Instant.ofEpochMilli(1000L) -> Instant.ofEpochMilli(1000L)))
        val loopStep = r.history.steps.head
        assertEq(loopStep.children.steps.length, 1)
        assertSleepStep(loopStep.children.steps.head)
      }

      "setStateAndSleep" - {
        var scheduled = 0
        val vc = new VirtualClock(initialMs = 2000L, onSchedule = { _ => scheduled += 1 })
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Int, String]
        var pairs = Vector.empty[(Int, Int)]
        var stateAfter = -1
        val after = dsl.action("After")(ros => () => { stateAfter = ros.state })
        val loop = dsl.loop("StateSleep") { ros =>
          pairs :+= (ros.obs -> ros.state)
          if (ros.obs == 0) dsl.loop.setStateAndSleepMs(10, 50)
          else if (ros.obs == 1) dsl.loop.setStateAndSleep(20, JavaDuration.ofMillis(75))
          else dsl.loop.break
        }
        val r = Plan.action(loop >> after).test(Observer(_ => scheduled)).withInitialState(0).withoutRef.run()()
        assertEq(r.result, Result.Pass)
        assertEq(pairs, Vector(0 -> 0, 1 -> 10, 2 -> 20))
        assertEq(stateAfter, 20)
        assertEq(vc.schedules, Vector(
          Instant.ofEpochMilli(2000L) -> Instant.ofEpochMilli(2050L),
          Instant.ofEpochMilli(2050L) -> Instant.ofEpochMilli(2125L),
        ))
      }

      "sleepModStateLeft" - {
        val vc = new VirtualClock()
        import vc.em

        var postRan = false
        val dsl = Dsl.full[Thunk, Unit, Unit, Int, String]
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val loop = dsl.loop("BadSleepState") { _ =>
          LoopStep.Sleep[Thunk, Unit, Unit, Int, String](JavaDuration.ofMillis(50), Some(_ => OrLeft("sleep state err")))
        } +> post
        val r = Plan.action(loop).testU.withInitialState(0).withoutRef.run()()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("sleep state err")))
        assertEq(vc.schedules.length, 0)
        assertEq(postRan, false)
      }

      "sleepModStateException" - {
        val vc = new VirtualClock()
        import vc.em

        val boom = new RuntimeException("sleep state boom")
        val dsl = Dsl.full[Thunk, Unit, Unit, Int, String]
        val loop = dsl.loop("ThrowSleepState") { _ =>
          LoopStep.Sleep[Thunk, Unit, Unit, Int, String](JavaDuration.ofMillis(50), Some(_ => throw boom))
        }
        val r = Plan.action(loop).testU.withInitialState(0).withoutRef.run()()
        assert(r.failed)
        r.failure match {
          case Some(Failure.WithCause(_, cause)) => assert(cause eq boom)
          case other                             => fail("Expected WithCause, got: " + other)
        }
        assertEq(vc.schedules.length, 0)
      }

      "sleepChecksInvariants" - {
        "passPointAndDelta" - {
          var scheduled = 0
          val vc = new VirtualClock(initialMs = 1000L, onSchedule = { _ => scheduled += 1 })
          import vc.em

          var pointInvSeen = Vector.empty[(Int, Int)]
          var deltaInvSeen = Vector.empty[(Int, Int)]
          val dsl = Dsl.full[Thunk, Unit, Int, Int, String]
          val pointInv = dsl.point("obs==state") { os =>
            pointInvSeen :+= (os.obs -> os.state)
            if (os.obs == os.state) None else Some(s"${os.obs} != ${os.state}")
          }
          val deltaInv = dsl.around("monotonic")(_.obs) { (after, before) =>
            deltaInvSeen :+= (before -> after.obs)
            if (after.obs >= before) None else Some(s"regressed $before -> ${after.obs}")
          }
          val loop = dsl.loop("SleepInv") { ros =>
            if (ros.obs < 2) dsl.loop.setStateAndSleepMs(ros.state + 1, 50) else dsl.loop.break
          }
          val r = Plan(loop, pointInv & deltaInv).test(Observer(_ => scheduled)).withInitialState(0).withoutRef.run()()
          assertEq(r.result, Result.Pass)
          // Initial state: (0,0); after sleep 1: (1,1); after sleep 2: (2,2); (and possibly once more at loop exit)
          assert(pointInvSeen.startsWith(Vector(0 -> 0, 1 -> 1, 2 -> 2)))
          assert(deltaInvSeen.contains(0 -> 1))
          assert(deltaInvSeen.contains(1 -> 2))
        }

        "failPointStopsLoop" - {
          var scheduled = 0
          val vc = new VirtualClock(initialMs = 1000L, onSchedule = { _ => scheduled += 1 })
          import vc.em

          var iterations = 0
          var postRan = false
          val dsl = Dsl.full[Thunk, Unit, Int, Int, String]
          val inv = dsl.focus("Val").obsAndState(identity, identity).assert.equal
          val post = dsl.point("Post")(_ => { postRan = true; None })
          // Sleeps without updating state, so after sleep obs=1 != state=0 -> invariant fails!
          val loop = dsl.loop("SleepInvFail") { _ =>
            iterations += 1
            if (iterations <= 5) dsl.loop.sleepMs(50) else dsl.loop.break
          } +> post
          val r = Plan(loop, inv).test(Observer(_ => scheduled)).withInitialState(0).withoutRef.run()()
          assert(r.failed)
          assertEq(iterations, 1)
          assertEq(postRan, false)
        }

        "failDeltaStopsLoop" - {
          var externalValue = 10
          val vc = new VirtualClock(initialMs = 1000L, onSchedule = { _ => externalValue -= 1 })
          import vc.em

          var iterations = 0
          val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
          val deltaInv = dsl.around("nonDecreasing")(_.obs) { (after, before) =>
            if (after.obs >= before) None else Some(s"dropped $before -> ${after.obs}")
          }
          val loop = dsl.loop("SleepDeltaFail") { _ =>
            iterations += 1
            if (iterations <= 5) dsl.loop.sleepMs(50) else dsl.loop.break
          }
          val r = Plan(loop, deltaInv).test(Observer(_ => externalValue)).stateless.withoutRef.run()()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause("dropped 10 -> 9")))
          assertEq(iterations, 1)
        }
      }

      "sleepReObserveFailure" - {
        var obsCalls = 0
        val vc = new VirtualClock()
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
        val obs = Observer.attempt[Unit, Int, String] { _ =>
          obsCalls += 1
          if (obsCalls == 1) OrRight(0) else OrLeft("obs failed after sleep")
        }
        val loop = dsl.loop("ObsFailSleep")(ros => if (ros.obs == 0) dsl.loop.sleepMs(20) else dsl.loop.break)
        val r = Plan.action(loop).test(obs).stateless.withoutRef.run()()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("obs failed after sleep")))
      }

      "sleepInterleavedWithAct" - {
        var externalValue = 0
        val vc = new VirtualClock(initialMs = 1000L, onSchedule = { _ => externalValue += 10 })
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
        val inc1 = dsl.action("Inc1")(_ => () => { externalValue += 1 })
        val loop = dsl.loop("Interleaved") { ros =>
          ros.obs match {
            case 0  => dsl.loop.act(inc1)
            case 1  => dsl.loop.sleepMs(50)
            case 11 => dsl.loop.act(inc1)
            case 12 => dsl.loop.sleepMs(75)
            case _  => dsl.loop.break
          }
        }
        val r = Plan.action(loop).test(Observer(_ => externalValue)).stateless.withoutRef.run()()
        assertEq(r.result, Result.Pass)
        assertEq(externalValue, 22)
        assertEq(r.stats.performedActions, 2)

        val children = r.history.steps.head.children.steps
        assertEq(children.length, 4)
        assertEq(children(0).name.value, "Inc1")
        assertSleepStep(children(1))
        assertEq(children(2).name.value, "Inc1")
        assertSleepStep(children(3))
      }
    }

    "failure" - {
      "loopStepFailImmediate" - {
        var postRan = false
        var afterRan = false
        val dsl = Dsl[Unit, Unit, Unit]
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = dsl.loop("FailNow")(_ => dsl.loop.fail("stop now")) +> post
        val r = Plan.action(loop >> after).testU.stateless.runU()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("stop now")))
        assertEq(postRan, false)
        assertEq(afterRan, false)
        assertEq(r.history.steps(1).result, Result.Skip)
      }

      "loopStepFailMidLoop" - {
        var postRan = false
        var afterRan = false
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = dsl.loop("FailAt2") { ros =>
          if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.fail("gave up at 2")
        } +> post
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✘ FailAt2
            |  ✓ Inc
            |  ✓ Inc
            |  ✘ gave up at 2
            |- After
            |Performed 2 actions, 0 checks.
          """.stripMargin)
        assertEq(r.failure, Some(Failure.NoCause("gave up at 2")))
        assertEq(c.value, 2)
        assertEq(postRan, false)
        assertEq(afterRan, false)
      }

      "runFunctionThrows" - {
        val boom = new RuntimeException("loop fn boom")
        var postRan = false
        var afterRan = false
        val dsl = Dsl[Unit, Unit, Unit]
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = dsl.loop("ThrowLoop")(_ => throw boom) +> post
        val r = Plan.action(loop >> after).testU.stateless.runU()
        assert(r.failed)
        r.failure match {
          case Some(Failure.WithCause(_, cause)) => assert(cause eq boom)
          case other                             => fail("Expected WithCause, got: " + other)
        }
        assertEq(postRan, false)
        assertEq(afterRan, false)
      }

      "innerActionFailsStopsLoop" - {
        var iterations = 0
        var postRan = false
        var afterRan = false
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val bad = dsl.action("Bad").attempt(_ => Some("inner boom"))
        val post = dsl.point("Post")(_ => { postRan = true; None })
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = dsl.loop("InnerFail") { ros =>
          iterations += 1
          if (ros.obs == 0) dsl.loop.act(inc)
          else if (ros.obs == 1) dsl.loop.act(bad)
          else dsl.loop.break
        } +> post
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).stateless.withRef(c).run()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("inner boom")))
        assertEq(iterations, 2)
        assertEq(c.value, 1)
        assertEq(postRan, false)
        assertEq(afterRan, false)
      }

      "innerActionThrowsStopsLoop" - {
        val boom = new RuntimeException("inner ex")
        var iterations = 0
        val dsl = Dsl[Unit, Unit, Unit]
        val bad = dsl.action("Crash")(_ => throw boom)
        val loop = dsl.loop("InnerCrash") { _ =>
          iterations += 1
          dsl.loop.act(bad)
        }
        val r = Plan.action(loop).testU.stateless.runU()
        assert(r.failed)
        r.failure match {
          case Some(Failure.WithCause(_, cause)) => assert(cause eq boom)
          case other                             => fail("Expected WithCause, got: " + other)
        }
        assertEq(iterations, 1)
      }

      "earlierStepFailedSkipsLoop" - {
        var loopFnCalled = false
        var preCalled = false
        val dsl = Dsl[Unit, Unit, Unit]
        val bad = dsl.action("FailFirst").attempt(_ => Some("first failed"))
        val pre = dsl.point("Pre")(_ => { preCalled = true; None })
        val loop = pre +> dsl.loop("L") { _ =>
          loopFnCalled = true
          dsl.loop.break
        }
        val r = Plan.action(bad >> loop).testU.stateless.runU()
        assert(r.failed)
        assertEq(loopFnCalled, false)
        assertEq(preCalled, false)
        assertEq(r.history.steps(1).name.value, "L")
        assertEq(r.history.steps(1).result, Result.Skip)
      }
    }

    "conditional" - {
      "skip" - {
        var loopFnCalled = false
        var preCalled = false
        var postCalled = false
        val dsl = Dsl[Unit, Unit, Unit]
        val pre = dsl.point("Pre")(_ => { preCalled = true; None })
        val post = dsl.point("Post")(_ => { postCalled = true; None })
        val loop = (pre +> dsl.loop("SkippedLoop") { _ =>
          loopFnCalled = true
          dsl.loop.break
        } +> post).skip
        val r = Plan.action(loop).testU.stateless.runU()
        assertRun(r,
          """
            |- SkippedLoop
            |- All skipped.
          """.stripMargin)
        assertEq(r.result, Result.Skip)
        assertEq(loopFnCalled, false)
        assertEq(preCalled, false)
        assertEq(postCalled, false)
        assertEq(r.stats.performedActions, 0)
        assertEq(r.stats.performedChecks, 0)
      }

      "whenFalseAtEntry" - {
        var loopFnCalled = false
        var preCalled = false
        var afterRan = false
        val dsl = Dsl[Counter, Int, Unit]
        val pre = dsl.point("Pre")(_ => { preCalled = true; None })
        val after = dsl.action("After")(_ => afterRan = true)
        val loop = (pre +> dsl.loop("Guarded") { _ =>
          loopFnCalled = true
          dsl.loop.break
        }).when(_.obs == 99)
        val c = new Counter(0)
        val r = Plan.action(loop >> after).test(Observer(_.value)).stateless.withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(r.history.steps(0).result, Result.Skip)
        assertEq(r.history.steps(1).result, Result.Pass)
        assertEq(loopFnCalled, false)
        assertEq(preCalled, false)
        assertEq(afterRan, true)
      }

      "whenEvaluatedOnlyAtEntryNotMidLoop" - {
        var condChecks = 0
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val loop = dsl.loop("EntryOnly") { ros =>
          if (ros.obs < 3) dsl.loop.act(inc) else dsl.loop.break
        }.when { ros =>
          condChecks += 1
          ros.obs == 0
        }
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertEq(r.result, Result.Pass)
        // Condition must only gate entering the loop, NOT mid-loop iterations when obs becomes 1, 2, 3
        assertEq(condChecks, 1)
        assertEq(c.value, 3)
        assertEq(r.stats.performedActions, 3)
      }

      "unless" - {
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc())
        val loop = dsl.loop("UnlessLoop")(ros => if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.break).unless(_.state == 0)

        val c1 = new Counter(0)
        val r1 = Plan.action(loop).test(Observer(_.value)).withInitialState(0).withRef(c1).run()
        assertEq(r1.result, Result.Skip)
        assertEq(c1.value, 0)

        val c2 = new Counter(0)
        val r2 = Plan.action(loop).test(Observer(_.value)).withInitialState(1).withRef(c2).run()
        assertEq(r2.result, Result.Pass)
        assertEq(c2.value, 2)
      }

      "whenException" - {
        val boom = new RuntimeException("cond boom")
        var loopFnCalled = false
        val dsl = Dsl[Unit, Unit, Unit]
        val loop = dsl.loop("BadCond") { _ =>
          loopFnCalled = true
          dsl.loop.break
        }.when(_ => throw boom)
        val r = Plan.action(loop).testU.stateless.runU()
        assert(r.failed)
        r.failure match {
          case Some(Failure.WithCause(_, cause)) => assert(cause eq boom)
          case other                             => fail("Expected WithCause, got: " + other)
        }
        assertEq(loopFnCalled, false)
      }

      "multipleWhenShortCircuit" - {
        var secondCalled = false
        var loopFnCalled = false
        val dsl = Dsl[Unit, Unit, Unit]
        val loop = dsl.loop("ShortCircuit") { _ =>
          loopFnCalled = true
          dsl.loop.break
        }.when(_ => false).when { _ =>
          secondCalled = true
          true
        }
        val r = Plan.action(loop).testU.stateless.runU()
        assertEq(r.result, Result.Skip)
        assertEq(loopFnCalled, false)
        assertEq(secondCalled, false)
      }
    }

    "checksAndInvariants" - {
      "preAndPostRunOnceAroundWholeLoop" - {
        var preCalls = 0
        var postCalls = 0
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val pre = dsl.point("Pre0") { os =>
          preCalls += 1
          if (os.obs == 0) None else Some(s"expected 0, got ${os.obs}")
        }
        val post = dsl.point("Post3") { os =>
          postCalls += 1
          if (os.obs == 3) None else Some(s"expected 3, got ${os.obs}")
        }
        val loop = pre +> dsl.loop("Count") { ros =>
          if (ros.obs < 3) dsl.loop.act(inc) else dsl.loop.break
        } +> post
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✓ Count
            |  ✓ Pre-conditions
            |    ✓ Pre0
            |  ✓ Inc
            |  ✓ Inc
            |  ✓ Inc
            |  ✓ Post-conditions
            |    ✓ Post3
            |✓ All pass.
            |Performed 3 actions, 2 checks.
          """.stripMargin)
        assertEq(preCalls, 1)
        assertEq(postCalls, 1)
      }

      "aroundDeltaSpansEntireLoop" - {
        val dsl = Dsl[Counter, Int, Unit]
        val v = dsl.focus("v").value(_.obs)
        val inc = dsl.action("Inc")(_.ref.inc())
        val loop = dsl.loop("Add3") { ros =>
          if (ros.obs < 3) dsl.loop.act(inc) else dsl.loop.break
        } +> v.assert.increaseBy(3)
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✓ Add3
            |  ✓ Inc
            |  ✓ Inc
            |  ✓ Inc
            |  ✓ Post-conditions
            |    ✓ v should increase by 3.
            |✓ All pass.
            |Performed 3 actions, 1 check.
          """.stripMargin)
      }

      "preConditionFailAbortsLoop" - {
        var loopFnCalled = false
        var postCalled = false
        val dsl = Dsl[Counter, Int, Unit]
        val v = dsl.focus("v").value(_.obs)
        val post = dsl.point("Post")(_ => { postCalled = true; None })
        val loop = v.assert(99) +> dsl.loop("NeverRun") { _ =>
          loopFnCalled = true
          dsl.loop.break
        } +> post
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assert(r.failed)
        assertEq(loopFnCalled, false)
        assertEq(postCalled, false)
        assertEq(r.stats.performedActions, 0)
        assertEq(r.stats.performedChecks, 1)
      }

      "postConditionFailFailsLoop" - {
        val dsl = Dsl[Counter, Int, Unit]
        val obs = dsl.focus("v").value(_.obs)
        val inc = dsl.action("Inc")(_.ref.inc())
        val loop = dsl.loop("PostFail") { ros =>
          if (ros.obs < 2) dsl.loop.act(inc) else dsl.loop.break
        } +> obs.assert(99)
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertRun(r,
          """
            |✘ PostFail
            |  ✓ Inc
            |  ✓ Inc
            |  ✘ Post-conditions
            |    ✘ v should be 99. -- Got 2.
            |Performed 2 actions, 1 check.
          """.stripMargin)
      }

      "invariantsFailMidLoopHaltsImmediately" - {
        var iterations = 0
        val dsl = Dsl[Counter, Int, Int]
        val inv = dsl.focus("Val").obsAndState(identity, identity).assert.equal
        val incOk = dsl.action("IncOk")(_.ref.inc()).updateState(_ + 1)
        val incBad = dsl.action("IncBad")(_.ref.inc()) // forgets updateState -> breaks invariant!
        val loop = dsl.loop("InvHalt") { ros =>
          iterations += 1
          if (ros.obs == 0) dsl.loop.act(incOk)
          else if (ros.obs == 1) dsl.loop.act(incBad)
          else dsl.loop.act(incOk)
        }
        val c = new Counter(0)
        val r = Plan(loop, inv).test(Observer(_.value)).withInitialState(0).withRef(c).run()
        assertRun(r,
          """
            |✓ Initial state.
            |  ✓ Val should be 0.
            |✘ InvHalt
            |  ✓ IncOk
            |    ✓ Action
            |    ✓ Invariants
            |      ✓ Val should be 1.
            |  ✘ IncBad
            |    ✓ Action
            |    ✘ Invariants
            |      ✘ Val should be 1. -- Got 2.
            |Performed 2 actions, 3 checks.
          """.stripMargin)
        assertEq(iterations, 2)
        assertEq(c.value, 2)
      }
    }

    "retry" - {
      val policy3 = Retry.Policy.fixedIntervalAndAttempts(ScalaDuration.Zero, 3)

      "preCondRetryProvidesFreshObsToLoop" - {
        var obsCalls = 0
        var firstLoopObs = -1
        var scopes = Vector.empty[Retry.Scope]
        val dsl = Dsl[Unit, Int, Unit]
        val v = dsl.focus("v").value(_.obs)
        val obs = Observer[Unit, Int] { _ =>
          obsCalls += 1
          if (obsCalls < 3) 0 else 10
        }
        val loop = v.assert(10) +> dsl.loop("AfterPre") { ros =>
          firstLoopObs = ros.obs
          dsl.loop.break
        }
        val policy = Retry.Policy { (ctx, _) =>
          scopes :+= ctx.scope
          if (ctx.retryCount < 3) Some(Instant.now()) else None
        }
        val r = Plan.action(loop).test(obs).stateless.withRetryPolicy(policy).runU()
        assertEq(r.result, Result.Pass)
        assertEq(firstLoopObs, 10)
        assert(scopes.nonEmpty)
        assert(scopes.forall(_ == Retry.Scope.PreConditions))
      }

      "postCondRetry" - {
        var obsCalls = 0
        var scopes = Vector.empty[Retry.Scope]
        val dsl = Dsl[Unit, Int, Unit]
        val v = dsl.focus("v").value(_.obs)
        val obs = Observer[Unit, Int] { _ =>
          obsCalls += 1
          if (obsCalls < 4) 0 else 50
        }
        val loop = dsl.loop("PostRetry")(_ => dsl.loop.break) +> v.assert(50)
        val policy = Retry.Policy { (ctx, _) =>
          scopes :+= ctx.scope
          if (ctx.retryCount < 4) Some(Instant.now()) else None
        }
        val r = Plan.action(loop).test(obs).stateless.withRetryPolicy(policy).runU()
        assertEq(r.result, Result.Pass)
        assert(scopes.nonEmpty)
        assert(scopes.forall(_ == Retry.Scope.PostConditions))
      }

      "loopStepFailRetriedUnderActionScope" - {
        var obsCalls = 0
        var scopes = Vector.empty[Retry.Scope]
        val dsl = Dsl[Unit, Int, Unit]
        val obs = Observer[Unit, Int] { _ =>
          obsCalls += 1
          obsCalls // 1, 2, 3
        }
        val loop = dsl.loop("RetryFailStep") { ros =>
          if (ros.obs < 3) dsl.loop.fail(s"not ready: ${ros.obs}")
          else dsl.loop.break
        }
        val policy = Retry.Policy { (ctx, _) =>
          scopes :+= ctx.scope
          if (ctx.retryCount < 3) Some(Instant.now()) else None
        }
        val r = Plan.action(loop).test(obs).stateless.withRetryPolicy(policy).runU()
        assertEq(r.result, Result.Pass)
        assertEq(obsCalls, 3)
        assertEq(scopes, Vector(Retry.Scope.Action, Retry.Scope.Action))
        assertEq(r.stats.retries, 2)
      }

      "loopStepFailExhaustsRetries" - {
        var attempts = 0
        val dsl = Dsl[Unit, Int, Unit]
        val obs = Observer[Unit, Int](_ => { attempts += 1; attempts })
        val loop = dsl.loop("AlwaysFail")(ros => dsl.loop.fail(s"err ${ros.obs}"))
        val policy = Retry.Policy.fixedIntervalAndAttempts(ScalaDuration.Zero, 2)
        val r = Plan.action(loop).test(obs).stateless.withRetryPolicy(policy).runU()
        assert(r.failed)
        assertEq(r.failure, Some(Failure.NoCause("err 3")))
        assertEq(r.stats.retries, 2)
      }

      "runExceptionRetriedUnderActionScope" - {
        var obsCalls = 0
        var scopes = Vector.empty[Retry.Scope]
        val dsl = Dsl[Unit, Int, Unit]
        val obs = Observer[Unit, Int] { _ =>
          obsCalls += 1
          obsCalls
        }
        val loop = dsl.loop("RetryThrow") { ros =>
          if (ros.obs < 3) sys.error(s"transient ${ros.obs}")
          else dsl.loop.break
        }
        val policy = Retry.Policy { (ctx, _) =>
          scopes :+= ctx.scope
          if (ctx.retryCount < 3) Some(Instant.now()) else None
        }
        val r = Plan.action(loop).test(obs).stateless.withRetryPolicy(policy).runU()
        assertEq(r.result, Result.Pass)
        assertEq(obsCalls, 3)
        assertEq(scopes, Vector(Retry.Scope.Action, Retry.Scope.Action))
        assertEq(r.stats.retries, 2)
      }

      "runExceptionExhaustsRetries" - {
        val boom = new RuntimeException("permanent boom")
        val dsl = Dsl[Unit, Unit, Unit]
        val loop = dsl.loop("AlwaysThrow")(_ => throw boom)
        val policy = Retry.Policy.fixedIntervalAndAttempts(ScalaDuration.Zero, 2)
        val r = Plan.action(loop).testU.stateless.withRetryPolicy(policy).runU()
        assert(r.failed)
        r.failure match {
          case Some(Failure.WithCause(_, cause)) => assert(cause eq boom)
          case other                             => fail("Expected WithCause, got: " + other)
        }
        assertEq(r.stats.retries, 2)
      }

      "innerActionRetry" - {
        var failsLeft = 2
        val dsl = Dsl[Counter, Int, Unit]
        val flakyInc = dsl.action("FlakyInc").attempt { ros =>
          if (failsLeft > 0) {
            failsLeft -= 1
            Some("flaky")
          } else {
            ros.ref.inc()
            None
          }
        }
        val loop = dsl.loop("FlakyLoop")(ros => if (ros.obs < 2) dsl.loop.act(flakyInc) else dsl.loop.break)
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).withRetryPolicy(policy3).run()
        assertEq(r.result, Result.Pass)
        assertEq(c.value, 2)
        assertEq(r.stats.retries, 2)
      }

      "sleepReObserveRetry" - {
        var obsCalls = 0
        val vc = new VirtualClock()
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
        val obs = Observer.attempt[Unit, Int, String] { _ =>
          obsCalls += 1
          obsCalls match {
            case 1 => OrRight(0)
            case 2 => OrLeft("transient obs err 1")
            case 3 => OrLeft("transient obs err 2")
            case _ => OrRight(1)
          }
        }
        val loop = dsl.loop("SleepObsRetry")(ros => if (ros.obs == 0) dsl.loop.sleepMs(25) else dsl.loop.break)
        val r = Plan.action(loop).test(obs).stateless.withoutRef.withRetryPolicy(policy3).run()()
        assertEq(r.result, Result.Pass)
        assertEq(obsCalls, 4)
        assertEq(r.stats.retries, 2)
      }

      "sleepInvariantRetry" - {
        var obsCalls = 0
        val vc = new VirtualClock()
        import vc.em

        val dsl = Dsl.full[Thunk, Unit, Int, Int, String]
        // After sleep updates state to 1, observer returns stale 0 twice before catching up to 1
        val obs = Observer[Unit, Int] { _ =>
          obsCalls += 1
          if (obsCalls <= 3) 0 else 1
        }
        val inv = dsl.focus("Val").obsAndState(identity, identity).assert.equal
        val loop = dsl.loop("SleepInvRetry") { ros =>
          if (ros.obs == 0) dsl.loop.setStateAndSleepMs(1, 25) else dsl.loop.break
        }
        val r = Plan(loop, inv).test(obs).withInitialState(0).withoutRef.withRetryPolicy(policy3).run()()
        assertEq(r.result, Result.Pass)
        assertEq(obsCalls, 4)
        assert(r.stats.retries >= 2)
      }
    }

    "callbacks" - {
      "firesOnlyOnInnerSingleActions" - {
        val vc = new VirtualClock()
        import vc.em

        var events = Vector.empty[String]
        var step = 0
        val dsl = Dsl.full[Thunk, Unit, Int, Unit, String]
        val cb = dsl.runCallbacks.aroundActions.before((_, name) => () => { events :+= s"before:${name.value}" }) ++
          dsl.runCallbacks.aroundActions.after((_, name, err) => () => { events :+= s"after:${name.value}:${err.getOrElse("ok")}" })
        val a1 = dsl.action("A1")(_ => () => { step += 1; events :+= "run:A1" })
        val loop = dsl.loop("OuterLoop") { ros =>
          ros.obs match {
            case 0 => dsl.loop.act(a1)
            case 1 => step += 1; dsl.loop.sleepMs(10)
            case 2 => dsl.loop.act(a1)
            case _ => dsl.loop.break
          }
        }
        val r = Plan.action(loop).test(Observer(_ => step)).stateless.withoutRef.withCallbacks(cb).run()()
        assertEq(r.result, Result.Pass)
        assertEq(events, Vector(
          "before:A1", "run:A1", "after:A1:ok",
          "before:A1", "run:A1", "after:A1:ok",
        ))
      }
    }

    "transforms" - {
      "mapR" - {
        final case class OuterRef(inner: Counter)
        val innerDsl = Dsl[Counter, Int, Unit]
        val inc = innerDsl.action("Inc")(_.ref.inc())
        val innerLoop = innerDsl.loop("L")(ros => if (ros.obs < 2) innerDsl.loop.act(inc) else innerDsl.loop.break)
        val lifted = innerLoop.mapR[OuterRef](_.inner)

        val outer = OuterRef(new Counter(0))
        val r = Plan.action(lifted).test(Observer[OuterRef, Int](_.inner.value)).stateless.withRef(outer).run()
        assertEq(r.result, Result.Pass)
        assertEq(outer.inner.value, 2)
      }

      "mapOS" - {
        final case class OuterState(tag: String, count: Int)
        implicit def univEqOuterState: UnivEq[OuterState] = UnivEq.derive
        val vc = new VirtualClock()
        import vc.em

        var world = 0
        val innerDsl = Dsl.full[Thunk, Unit, Int, Int, String]
        val bump = innerDsl.action("Bump")(_ => () => { world += 1 }).updateState(_ + 1)
        val innerLoop = innerDsl.loop("L") { ros =>
          if (ros.obs == 0) innerDsl.loop.act(bump)
          else if (ros.obs == 1) {
            world += 1
            innerDsl.loop.setStateAndSleepMs(ros.state + 10, 5)
          } else innerDsl.loop.setStateAndBreak(ros.state + 100)
        }

        val lifted = innerLoop.mapOS[String, OuterState](_.toInt, _.count)((os, c) => os.copy(count = c))
        var finalState: OuterState = null
        val outerDsl = Dsl.full[Thunk, Unit, String, OuterState, String]
        val capture = outerDsl.action("Capture")(ros => () => { finalState = ros.state })

        val r = Plan.action(lifted >> capture)
          .test(Observer(_ => world.toString))
          .withInitialState(OuterState("keep", 0))
          .withoutRef
          .run()()
        assertEq(r.result, Result.Pass)
        assertEq(finalState, OuterState("keep", 111))
      }

      "mapE" - {
        implicit val intErrHandler: ErrorHandler[Int] = ErrorHandler[Int](_ => -1)
        val dsl = Dsl[Unit, Unit, Int]

        "loopFail" - {
          val loop = dsl.loop("L")(_ => dsl.loop.fail("abc")).mapE(_.length)
          val r = Plan.action(loop).testU.withInitialState(0).runU()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause(3)))
        }

        "breakModState" - {
          val loop = dsl.loop("L")(_ => LoopStep.Break[Id, Unit, Unit, Int, String](Some(_ => OrLeft("hello")))).mapE(_.length)
          val r = Plan.action(loop).testU.withInitialState(0).runU()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause(5)))
        }

        "sleepModState" - {
          val vc = new VirtualClock()
          import vc.em
          val dt = Dsl.full[Thunk, Unit, Unit, Int, String]
          val loop = dt.loop("L") { _ =>
            LoopStep.Sleep[Thunk, Unit, Unit, Int, String](JavaDuration.ofMillis(5), Some(_ => OrLeft("seven!!")))
          }.mapE(_.length)
          val r = Plan.action(loop).testU.withInitialState(0).withoutRef.run()()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause(7)))
        }
      }

      "pmapR" - {
        val innerDsl = Dsl[Counter, Int, Unit]
        val inc = innerDsl.action("Inc")(_.ref.inc())
        val innerLoop = innerDsl.loop("L")(ros => if (ros.obs < 2) innerDsl.loop.act(inc) else innerDsl.loop.break)
        val lifted = innerLoop.pmapR[Option[Counter]] {
          case Some(c) => OrRight(c)
          case None    => OrLeft("missing ref")
        }

        "pass" - {
          val c = new Counter(0)
          val r = Plan.action(lifted).test(Observer[Option[Counter], Int](_.fold(0)(_.value))).stateless.withRef(Some(c)).run()
          assertEq(r.result, Result.Pass)
          assertEq(c.value, 2)
        }

        "failAtEntry" - {
          val r = Plan.action(lifted).test(Observer[Option[Counter], Int](_.fold(0)(_.value))).stateless.withRef(None).run()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause("missing ref")))
        }

        "failMidLoop" - {
          val c = new Counter(0)
          val r = Plan.action(lifted)
            .test(Observer[Option[Counter], Int](_.fold(c.value)(_.value)))
            .stateless
            .withRefByName(if (c.value == 0) Some(c) else None)
            .run()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause("missing ref")))
          assertEq(c.value, 1)
        }
      }

      "pmapO" - {
        val innerDsl = Dsl[Counter, Int, Unit]
        val inc = innerDsl.action("Inc")(_.ref.inc())
        val innerLoop = innerDsl.loop("L")(ros => if (ros.obs < 2) innerDsl.loop.act(inc) else innerDsl.loop.break)
        val lifted = innerLoop.pmapO[Option[Int]] {
          case Some(v) => OrRight(v)
          case None    => OrLeft("missing obs")
        }

        "pass" - {
          val c = new Counter(0)
          val r = Plan.action(lifted).test(Observer[Counter, Option[Int]](x => Some(x.value))).stateless.withRef(c).run()
          assertEq(r.result, Result.Pass)
          assertEq(c.value, 2)
        }

        "failAtEntry" - {
          val c = new Counter(0)
          val r = Plan.action(lifted).test(Observer[Counter, Option[Int]](_ => Option.empty[Int])).stateless.withRef(c).run()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause("missing obs")))
        }

        "failMidLoop" - {
          val c = new Counter(0)
          val r = Plan.action(lifted)
            .test(Observer[Counter, Option[Int]](x => if (x.value == 1) None else Some(x.value)))
            .stateless
            .withRef(c)
            .run()
          assert(r.failed)
          assertEq(r.failure, Some(Failure.NoCause("missing obs")))
          assertEq(c.value, 1)
        }
      }

      "transToFuture" - {
        import scala.concurrent.ExecutionContext.Implicits.global
        val dsl = Dsl[Counter, Int, Int]
        val inc = dsl.action("Inc")(_.ref.inc()).updateState(_ + 1)
        val loop = dsl.loop("SyncToFuture")(ros => if (ros.obs < 3) dsl.loop.act(inc) else dsl.loop.break)
        val c = new Counter(0)
        val testF = Plan.action(loop).test(Observer(_.value)).withInitialState(0).trans(ExecutionModel.toFuture)
        testF.withRef(c).run().map { r =>
          assertEq(r.result, Result.Pass)
          assertEq(c.value, 3)
          assertEq(r.stats.performedActions, 3)
        }
      }
    }

    "asyncAndStackSafety" - {
      "asyncFutureWithActAndSleep" - {
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.Future

        val dsl = Dsl.future[Counter, Int, Int]
        val inc = dsl.action("IncAsync")(ros => Future { ros.ref.inc() }).updateState(_ + 1)
        var slept = false
        val loop = dsl.loop("AsyncLoop") { ros =>
          if (ros.obs == 0) dsl.loop.act(inc)
          else if (!slept) {
            slept = true
            dsl.loop.setStateAndSleepMs(ros.state + 10, 1)
          } else if (ros.obs < 2) dsl.loop.act(inc)
          else dsl.loop.break
        }
        var finalState = -1
        val after = dsl.action("After")(ros => Future { finalState = ros.state })
        val c = new Counter(0)
        Plan.action(loop >> after).test(Observer(_.value)).withInitialState(0).withRef(c).run().map { r =>
          assertEq(r.result, Result.Pass)
          assertEq(c.value, 2)
          assertEq(finalState, 12)
          assertEq(r.stats.performedActions, 3)
        }
      }

      "stackSafetySync" - {
        val size = Platform.StackTestSize
        val dsl = Dsl[Counter, Int, Unit]
        val inc = dsl.action("Inc")(_.ref.inc())
        val loop = dsl.loop("BigSync") { ros =>
          if (ros.obs < size) dsl.loop.act(inc) else dsl.loop.break
        }
        val c = new Counter(0)
        val r = Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run()
        assertEq(r.result, Result.Pass)
        assertEq(c.value, size)
        assertEq(r.stats.performedActions, size)
      }

      "stackSafetyAsync" - {
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.Future

        val size = Platform.StackTestSize
        val dsl = Dsl.future[Counter, Int, Unit]
        val inc = dsl.action("Inc") { ros =>
          ros.ref.inc()
          Future.successful(())
        }
        val loop = dsl.loop("BigAsync") { ros =>
          if (ros.obs < size) dsl.loop.act(inc) else dsl.loop.break
        }
        val c = new Counter(0)
        Plan.action(loop).test(Observer(_.value)).stateless.withRef(c).run().map { r =>
          assertEq(r.result, Result.Pass)
          assertEq(c.value, size)
          assertEq(r.stats.performedActions, size)
        }
      }
    }

  }
}
