package teststate

import scala.concurrent.duration.Duration
import teststate.Exports._
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

    var _invariantOk = true

    var _onValue = List.empty[() => Unit]

    def invariantOk(): Boolean = synchronized {
      val x = _invariantOk
      _invariantOk = true
      x
    }

    def invalidateInvariant() = synchronized {
      _invariantOk = false
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
  val value2         = *.focus("value2").value(_.obs.value2())
  val failOnValue2   = *.action("failOnValue2")(_.ref.failOnValue2.simFail(3))
  case class Obs(value: Int, valueCalls: Int, incCalls: Int, invariantOk: Boolean, value2: () => Int)
   */

  val * = Dsl[Ref, Obs, Unit]

  val value          = *.focus("value").value(_.obs.value)
  val valueCalls     = *.focus("valueCalls").value(_.obs.valueCalls)
  val incCalls       = *.focus("incCalls").value(_.obs.incCalls)
  val invariantOk    = *.focus("invariantOk").value(_.obs.invariantOk)
  val invariant      = invariantOk.assert(true)
  val failOnValue    = *.action("failOnValue.simFail(3)")(_.ref.failOnValue.simFail(3))
  val failOnInc      = *.action("failOnInc.simFail(3)")(_.ref.failOnInc.simFail(3)) +> valueCalls.assert.increment +> value.assert.noChange
  val inc            = *.action("inc")(_.ref.inc())
  val queueUpdate    = *.action("queueUpdate")(_.ref.queueUpdate(123))
  val incNormal      = inc +> incCalls.assert.increment +> valueCalls.assert.increment +> value.assert.increment
  val incFailOnValue = inc +> incCalls.assert.increment +> valueCalls.assert.increaseBy(4) +> value.assert.increment
  val incFailOnInc   = inc +> incCalls.assert.increment +> valueCalls.assert.increment +> value.assert.increment // incCalls +1 only cos that's what a successful action does

  val retryPolicy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 3)
  val insufficientRetryPolicy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 2)
  val hugeRetryPolicy = Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 11)

  val observer = Observer((_: Ref).toObs())

  def assertRetryWorks(plan: *.Plan, refMod: Ref => Unit = _ => ()): Unit = {
    val test = plan.addInvariants(invariant).test(observer).stateless.withLazyRef((new Ref)(refMod))
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
  }

  def assertInvariantFails(plan: *.Plan, refMod: Ref => Unit = _ => ()): Unit = {
    val test = plan.addInvariants(invariant).test(observer).stateless.withLazyRef((new Ref)(refMod))
    val result: Report[String] = test.withRetryPolicy(hugeRetryPolicy).run()
    val report = result.format
    assert(result.failed, report.contains("invariantOk should be true"))
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

  override def tests = TestSuite {

    'stackSafe {
      val * = Dsl[Unit, Unit, Unit]
      val fail = Some("fail")
      val result: Report[String] =
        Plan.action(*.emptyAction +> *.point("fail")(_ => fail))
          .testU
          .stateless
          .withRetryPolicy(Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, teststate.Platform.StackTestSize))
          .runU()
      // println(result.format)
      assert(result.failed)
    }

    'initial {
      'ref {
        val refMod = explodingRef()
        val test = *.emptyPlan.addInvariants(invariant).test(observer).stateless.withLazyRef((new Ref)(refMod))
        val result: Report[String] = test.withRetryPolicy(retryPolicy).run()
        assert(!result.failed)
      }
      'obs {
        val plan = Plan.action(*.emptyAction <+ valueCalls.assert(4))
        assertRetryWorks(plan, _.failOnValue.simFail(3))
      }
      'invariant {
        assertInvariantFails(*.emptyPlan, _.invalidateInvariant())
      }
    }

    'action {
       'ref {
         val ref = new Ref
         var refFn = (_: Ref) => ()
         val plan = Plan.action(*.action("hack")(_ => refFn = explodingRef()) >> *.emptyAction)
         val test = plan.addInvariants(invariant).test(observer).stateless.withRefByName(ref(refFn))
         val result: Report[String] = test.withRetryPolicy(retryPolicy).run()
         result.assert()
       }
      'obs {
        val plan = Plan.action(failOnValue >> *.emptyAction +> valueCalls.assert(5))
        assertRetryWorks(plan)
      }
      'actionSingle {
        val plan = Plan.action(incNormal >> failOnInc >> incFailOnInc >> incNormal)
        assertRetryWorks(plan)
      }
      'actionSubtest {
        val subtest = Plan.action(failOnInc >> incFailOnInc).asAction("subtest")
        val plan = Plan.action(incNormal >> subtest >> incNormal)
        assertRetryWorks(plan)
      }
      'actionGroup {
        val group = (failOnInc >> incFailOnInc >> incNormal).times(4)
        val plan = Plan.action(incNormal >> group >> incNormal)
        assertRetryWorks(plan)
      }
      'actionDueToBadObs {
        val plan = Plan.action(queueUpdate >> *.action("Throw unless obs.value = 123")(x => assert(x.obs.value == 123)))
        assertRetryWorks(plan)
      }
      'preCondFail {
        val plan = Plan.action(queueUpdate >> (*.emptyAction <+ value.assert(123)))
        assertRetryWorks(plan)
      }
      'postCondFail {
        val plan = Plan.action(queueUpdate +> value.assert(123))
        assertRetryWorks(plan)
      }
      'postEmptyFail {
        val plan = Plan.action(queueUpdate >> *.emptyAction +> value.assert(123))
        assertRetryWorks(plan)
      }
      // Actually no, checks are supposed to be pure!
      // 'preCondCrash {
      //   val plan = Plan.action(failOnValue2 >> (*.emptyAction <+ value2.assert(100)))
      //   assertRetryWorks(plan)
      // }
      // 'postCondCrash {
      //   val plan = Plan.action(failOnValue2 >> (*.emptyAction +> value2.assert(100)))
      //   assertRetryWorks(plan)
      // }
      'invariant {
        val plan = Plan.action(*.action("invalidate invariant")(_.ref.invalidateInvariant()))
        assertInvariantFails(plan)
      }
    }


  }
}
