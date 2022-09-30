package teststate

import java.time.Instant
import scala.concurrent.duration._
import teststate.Exports._
import teststate.TestUtil._
import utest._

object RetryTest extends TestSuite {

  def debug(s: => String = ""): Unit =
    () // println(s)

  class SimFailure(desc: String) {
    var _calls = 0
    def calls() = _calls

    var _fails = 0
    def simFail(i: Int): Unit = {
      _fails = i
      debug(s"SimFailure on $desc: Setting fails to ${_fails}")
    }

    // var _skips = 0
    // def simSkip(i: Int): Unit = {
    //   _skips = i
    //   debug(s"SimFailure on $desc: Setting skips to ${_skips}")
    // }

    def apply[A](a: => A): A = {
      _calls += 1
      if (_fails > 0) {
        _fails -= 1
        debug(s"SimFailure on $desc: ${_fails} failures remaining")
        sys error s"SimFailure on $desc: ${_fails} failures remaining"
      // } else if (_skips > 0) {
      //   _skips -= 1
      //   debug(s"SimFailure on $desc: ${_skips} skips remaining")
      //   None
      } else
        a
    }
  }

  class Ref {
    var _value = 0
    val failOnValue = new SimFailure("value()")
    val failOnInc = new SimFailure("inc()")

    var _invariantKO = 0

    var _onValue = List.empty[() => Unit]

    def invariantOk(): Boolean = synchronized {
      if (_invariantKO > 0) {
        _invariantKO -= 1
        false
      } else
        true
    }

    def invalidateInvariant() = synchronized {
      _invariantKO = 3
    }

    def queueUpdate(newValue: Int): Unit = synchronized {
      _onValue =
        _onValue :::
        (() => debug("updated queued 1/3")) ::
        (() => debug("updated queued 2/3")) ::
        (() => debug("updated queued 3/3")) ::
        (() => _value = newValue) ::
        Nil
    }

    def value(): Int = synchronized {
      _onValue match {
        case h :: t =>
          _onValue = t
          h()
        case _ =>
      }
      // debug("="*200)
      // new Exception().printStackTrace()
      // debug("="*200)
      val v = failOnValue(_value)
      debug(s"value() = $v (calls=${failOnValue.calls()})")
      v
    }

    def inc(): Unit = synchronized {
      debug("inc ← " + _value)
      failOnInc(_value += 1)
      debug("inc → " + _value)
    }

    def toObs(): Obs = synchronized {
      val v = value()
      val o = Obs(
        value       = v,
        valueCalls  = failOnValue.calls(),
        incCalls    = failOnInc.calls(),
        invariantOk = invariantOk())
        // value2      = () => value2())
      debug("toObs() -> "+o)
      o
    }

    def apply(f: Ref => Unit): this.type = {
      f(this)
      this
    }
  }

  case class Obs(value: Int, valueCalls: Int, incCalls: Int, invariantOk: Boolean)

  /*
  var _value2 = 100
  val failOnValue2 = new SimFailure("value2()")
  def value2(): Int = synchronized { failOnValue2(_value2) }
  val value2         = dsl.focus("value2").value(_.obs.value2())
  val failOnValue2   = dsl.action("failOnValue2")(_.ref.failOnValue2.simFail(3))
  case class Obs(value: Int, valueCalls: Int, incCalls: Int, invariantOk: Boolean, value2: () => Int)
   */

  type State = Int

  val dsl = Dsl[Ref, Obs, State]

  val value          = dsl.focus("value").value(_.obs.value)
  val valueCalls     = dsl.focus("valueCalls").value(_.obs.valueCalls)
  val incCalls       = dsl.focus("incCalls").value(_.obs.incCalls)
  val invariantOk    = dsl.focus("invariantOk").value(_.obs.invariantOk)
  val invariant      = invariantOk.assert(true)
  val failOnValue    = dsl.action("failOnValue.simFail(3)")(_.ref.failOnValue.simFail(3))
  val failOnInc      = dsl.action("failOnInc.simFail(3)")(_.ref.failOnInc.simFail(3)) +> valueCalls.assert.increment +> value.assert.noChange
  val inc            = dsl.action("inc")(_.ref.inc())
  val queueUpdate    = dsl.action("queueUpdate")(_.ref.queueUpdate(123))
  val incNormal      = inc +> incCalls.assert.increment +> valueCalls.assert.increment +> value.assert.increment
  val incFailOnValue = inc +> incCalls.assert.increment +> valueCalls.assert.increaseBy(4) +> value.assert.increment
  val incFailOnInc   = inc +> incCalls.assert.increment +> valueCalls.assert.increment +> value.assert.increment // incCalls +1 only cos that's what a successful action does

  val retryPolicy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 3)
  val insufficientRetryPolicy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 2)
  val hugeRetryPolicy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 11)

  val observer = Observer((_: Ref).toObs())

  def mkTest(plan: dsl.Plan, refMod: Ref => Unit = _ => ()) =
    plan.addInvariants(invariant).test(observer).withInitialState(0).withLazyRef((new Ref)(refMod))

  def assertRetryWorks(plan: dsl.Plan, refMod: Ref => Unit = _ => ()): Unit = {
    _assertRetryWorks(plan, refMod)
    ()
  }

  def _assertRetryWorks(plan: dsl.Plan, refMod: Ref => Unit = _ => ()): Report[String] = {
    val test = mkTest(plan, refMod)
    debug()

    // With appropriate retry
    val retryResult: Report[String] = test.withRetryPolicy(retryPolicy).run()
    retryResult.assert()

    // No retry
    val resultWithoutRetry = test.run()
    assert(resultWithoutRetry.failed)
    debug()

    // TODO Too little retry
    // val resultWithInsufficientRetry = test.withRetryPolicy(insufficientRetryPolicy).run()
    // assert(resultWithInsufficientRetry.failed)
    // debug()

    retryResult
  }

  def explodingRef(): Ref => Unit = {
    var i = 3
    _ =>
      if (i > 0) {
        i -= 1
        ???
      } else
        ()
  }

  def retryCtx(i1: Instant, in: Instant*): Retry.Ctx = {
    val is = i1 +: in.toVector
    Retry.Ctx(Retry.Scope.Action, is.init, is.last)
  }

  implicit class InstantExt(private val self: Instant) extends AnyVal {
    def +[B](d: Duration): Instant = self.plusMillis(d.toMillis)
    def -[B](d: Duration): Instant = self.minusMillis(d.toMillis)
  }

  override def tests = Tests {

    "stackSafe" - {
      val dsl = Dsl[Unit, Unit, Unit]
      val fail = Some("fail")
      val result: Report[String] =
        Plan.action(dsl.emptyAction +> dsl.point("fail")(_ => fail))
          .testU
          .stateless
          .withRetryPolicy(Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, teststate.Platform.StackTestSize))
          .runU()
      // println(result.format)
      assert(result.failed)
    }

    "policy" - {
      "timeout" - {
        val interval = 1 second
        val timeout = 6 seconds
        val policy = Retry.Policy.fixedIntervalWithTimeout(interval, timeout)
        val now = Instant.now()
        def test(ds: Duration*)(expect: Option[Instant]) = {
          val is = ds.map(now - _)
          val ctx = retryCtx(is.head, is.tail: _*)
          val actual = policy.nextTry(ctx, now)
          assert(actual == expect)
        }

        //   +--- 1s --+ (interval)
        //   |    .    |
        // -200ms | +800ms
        "interval1" - test(200.millis)(Some(now + 800.millis))

        // 4s + interval is in the past! result should be now
        // -4s  |  +0ms
        "limitToNow" - test(4.seconds)(Some(now))

        //   +-------------- 6s --------------+
        //   +-- 5.3s --+-- 0.5s --|-- 0.2s --+
        //   |          |          .          |
        // -5.8s      -0.5s        |        +0.2s
        "limitToTimeout" - test(5.8.seconds,0.5.seconds)(Some(now + 200.millis))

        //  +-- 4s --+-- 1s --+ (interval)
        //  |        |    .   |
        // -4.3s   -0.3s  | +0.7s
        "interval2" - test(4.3.seconds, 0.3.seconds)(Some(now + 0.7.seconds))

        //  +-- 9s --+-- 0s --+ (scheduler caused huge delay, try once after deadline)
        //  |        |        |
        // -9s       |       +0s
        "overOnce" - test(9.seconds)(Some(now))

        //   +--- 6s ---+---- 1s ----+ (stop, already tried once past deadline)
        //   |          |        |   |
        // -6.8s    deadline   -0.2s |
        "overTwice" - test(6.8.seconds, 0.2.seconds)(None)
      }
    }

    "initial" - {
      "ref" - {
        val refMod = explodingRef()
        val test = dsl.emptyPlan.addInvariants(invariant).test(observer).withInitialState(0).withLazyRef((new Ref)(refMod))
        val result: Report[String] = test.withRetryPolicy(retryPolicy).run()
        assert(!result.failed)
      }
      "obs" - {
        val plan = Plan.action(dsl.emptyAction <+ valueCalls.assert(4))
        assertRetryWorks(plan, _.failOnValue.simFail(3))
      }
      "invariant" - {
        assertRetryWorks(dsl.emptyPlan, _.invalidateInvariant())
      }
    }

    "action" - {
       "ref" - {
         val ref = new Ref
         var refFn = (_: Ref) => ()
         val plan = Plan.action(dsl.action("hack")(_ => refFn = explodingRef()) >> dsl.emptyAction)
         val test = plan.addInvariants(invariant).test(observer).withInitialState(0).withRefByName(ref(refFn))
         val result: Report[String] = test.withRetryPolicy(retryPolicy).run()
         result.assert()
       }
      "obs" - {
        val plan = Plan.action(failOnValue >> dsl.emptyAction +> valueCalls.assert(5))
        assertRetryWorks(plan)
      }
      "actionSingle" - {
        val plan = Plan.action(incNormal >> failOnInc >> incFailOnInc >> incNormal)
        assertRetryWorks(plan)
      }
      "actionSubtest" - {
        val subtest = Plan.action(failOnInc >> incFailOnInc).asAction("subtest")
        val plan = Plan.action(incNormal >> subtest >> incNormal)
        assertRetryWorks(plan)
      }
      "actionGroup" - {
        val group = (failOnInc >> incFailOnInc >> incNormal).times(4)
        val plan = Plan.action(incNormal >> group >> incNormal)
        assertRetryWorks(plan)
      }
      "actionDueToBadObs" - {
        val plan = Plan.action(queueUpdate >> dsl.action("Throw unless obs.value = 123")(x => assert(x.obs.value == 123)))
        assertRetryWorks(plan)
      }
      "preCondFail" - {
        val plan = Plan.action(queueUpdate >> (dsl.emptyAction <+ value.assert(123)))
        assertRetryWorks(plan)
      }
      "postCondFail" - {
        val plan = Plan.action(queueUpdate +> value.assert(123))
        assertRetryWorks(plan)
      }
      "postEmptyFail" - {
        val plan = Plan.action(queueUpdate >> dsl.emptyAction +> value.assert(123))
        assertRetryWorks(plan)
      }
      // Actually no, checks are supposed to be pure!
      // 'preCondCrash {
      //   val plan = Plan.action(failOnValue2 >> (dsl.emptyAction <+ value2.assert(100)))
      //   assertRetryWorks(plan)
      // }
      // 'postCondCrash {
      //   val plan = Plan.action(failOnValue2 >> (dsl.emptyAction +> value2.assert(100)))
      //   assertRetryWorks(plan)
      // }
      "invariant" - {
        val plan = Plan.action(dsl.action("invalidate invariant")(_.ref.invalidateInvariant()))
        assertRetryWorks(plan)
      }

      "reportObsErrorAfterActionError" - {
        val plan = Plan.action(dsl.action("I love my little one, Nim") { x =>
          x.ref.failOnValue.simFail(3)
          ???
        })
        val report = mkTest(plan).withRetryPolicy(retryPolicy).run()
        assertRun(report,
          """
            |✓ Initial state.
            |  ✓ invariantOk should be true.
            |✘ I love my little one, Nim
            |  ✘ Action -- scala.NotImplementedError: an implementation is missing
            |  ✘ Observation -- java.lang.RuntimeException: SimFailure on value(): 0 failures remaining
            |Performed 1 action, 1 check (with 3 retries).
          """.stripMargin)
      }

      "state" - {
        val plan = Plan.action(
          (failOnInc >> incFailOnInc >> incNormal).updateState(_ + 1) >>
          dsl.action("blah")(_ => ()) +> dsl.focus("state").value(_.state).assert(1)
        )
        assertRetryWorks(plan)
      }
    }


  }
}
