package teststate

import teststate.Exports._
import teststate.TestUtil._
import teststate.core.Around
import teststate.data._
import utest._

object DslTest extends TestSuite {

  val dsl = Dsl[Unit, Unit, Unit]

//  def extract1[A, B](s: Sack[A, B]): B =
//     s match {
//       case Sack.Value(b) => b
//       case x => sys error ("Got: " + x)
//     }
//
//  def extract1E[A, B, E](s: SackE[A, B, E]): B =
//    extract1(s) match {
//      case Right(b) => b
//      case Left(e) => sys error e.toString
//    }

  def extractAroundDelta1[O, S, E](c: Arounds[O, S, E]): Around.DeltaA[OS[O, S], E] =
    c match {
      case Sack.Value(Right(Around.Delta(d))) => d
      case _ => sys error ("What? " + c)
    }

  def extractPoint1[O, S, E](c: Points[O, S, E]): teststate.core.Point[OS[O, S], E] =
    c match {
      case Sack.Value(Right(p)) => p
      case _ => sys error ("What? " + c)
    }

  def testName[O, S, E](c: Arounds[O, S, E], expectF: String, expectC: String): Unit = {
    val n = extractAroundDelta1(c).name
    assertEq(n(None).value, expectF)
    assertEq(n(Some(BeforeAfter(null, null))).value, expectC)
  }

  override def tests = Tests {

    "changeTo" - {
      "pos" - testName(dsl.focus("Counter").value(_ => 7).assert.changeTo(_ + 1),
        "Counter should be <?>.",
        "Counter should be 8.")

      "neg" - testName(dsl.focus("Counter").value(_ => 7).assert.not.changeTo(_ + 1),
        "Counter shouldn't be <?>.",
        "Counter shouldn't be 8.")

      // TODO Didn't catch. Try using changeTo in multiple actions and verify final history. Might be Name strictness.
    }

    "incrementBy" - {
      "pos" - testName(dsl.focus("Counter").value(_ => 7).assert.increaseBy(2),
        "Counter should increase by 2.",
        "Counter should increase by 2.")
//        "Counter should be 9.")

      "neg" - testName(dsl.focus("Counter").value(_ => 7).assert.not.increaseBy(2),
        "Counter shouldn't increase by 2.",
        "Counter shouldn't increase by 2.")
//        "Counter shouldn't be 9.")
    }

    "decrementBy" - {
      "pos" - testName(dsl.focus("Counter").value(_ => 7).assert.decreaseBy(2),
        "Counter should decrease by 2.",
        "Counter should decrease by 2.")
//        "Counter should be 5.")

      "neg" - testName(dsl.focus("Counter").value(_ => 7).assert.not.decreaseBy(2),
        "Counter shouldn't decrease by 2.",
        "Counter shouldn't decrease by 2.")
//        "Counter shouldn't be 5.")
    }

    "testAround" - {
      val dsl = Dsl[Unit, Unit, Int]
      val inc = dsl.action("inc")(_ => ()).updateState(_ + 1)
      val check = dsl.testAround("inc by 1", (before, after) => s"${before.state} -> ${after.state}")(
        (before, after) => after.state - before.state == 1)
      val report = Plan.action(inc +> check).testU.withInitialState(10).runU()
      report.assert()
    }

    "equalWhenDefined" - {
      val os = OS((), ())
      "none" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.equalWhenDefined(None)
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter should be <?>.")
        val result = point.test(os)
        assert(result == Skipped)
      }
      "noneNeg" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.not.equalWhenDefined(None)
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter shouldn't be <?>.")
        val result = point.test(os)
        assert(result == Skipped)
      }
      "some" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.equalWhenDefined(Some(7))
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter should be 7.")
        val result = point.test(os)
        assert(result == Tri.pass)
      }
    }

    "equalByWhenDefined" - {
      val os = OS((), ())
      "none" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.equalByWhenDefined(_ => None)
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter should be <?>.")
        assertEq(point.name(Some(os)).value, "Counter should be <?>.")
        val result = point.test(os)
        assert(result == Skipped)
      }
      "noneNeg" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.not.equalByWhenDefined(_ => None)
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter shouldn't be <?>.")
        assertEq(point.name(Some(os)).value, "Counter shouldn't be <?>.")
        val result = point.test(os)
        assert(result == Skipped)
      }
      "someMatch" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.equalByWhenDefined(_ => Some(7))
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter should be <?>.")
        assertEq(point.name(Some(os)).value, "Counter should be 7.")
        val result = point.test(os)
        assert(result == Tri.pass)
      }
      "someMismatch" - {
        val p = dsl.focus("Counter").value(_ => 7).assert.equalByWhenDefined(_ => Some(8))
        val point = extractPoint1(p)
        assertEq(point.name(None).value, "Counter should be <?>.")
        assertEq(point.name(Some(os)).value, "Counter should be 8.")
        val result = point.test(os)
        assert(result match { case Failed(_) => true; case _ => false })
      }
      "runner" - {
        val dsl = Dsl[Unit, Int, Unit]
        val check = dsl.focus("num").value(_.obs).assert.equalByWhenDefined(_ => None)
        val action = dsl.action("act")(_ => ()) +> check
        val report = Plan.action(action).test(Observer.watch(123)).stateless.runU()
        val text = report.format(Report.Format.Default.uncolouredAscii.alwaysShowChildren)
        assertContains(text, "[skip] num should be <?>.")
        assertEq(report.stats.performedChecks, 0)
      }
    }

    "focusColl" - {
      val os = OS((), ())

      "mapReusability" - {
        val coll = dsl.focus("Items").collection(_ => List(1, 2, 3))
        val mapped = coll.map(_ * 2)
        val res = mapped.run(os)
        assertEq(res.iterator.toList, List(2, 4, 6))
        assertEq(res.iterator.toList, List(2, 4, 6))
      }

      "filterReusability" - {
        val coll = dsl.focus("Items").collection(_ => List(1, 2, 3))
        val filtered = coll.filter(_ > 1)
        val res = filtered.run(os)
        assertEq(res.iterator.toList, List(2, 3))
        assertEq(res.iterator.toList, List(2, 3))
      }

      "elemChangesRetryWithMap" - {
        import scala.concurrent.duration._
        import teststate.run.Retry
        var items = List(1, 2)
        var postCheckAttempts = 0
        val dsl = Dsl[Unit, List[Int], Unit]
        val check = dsl.focus("Items")
          .collection((os: OS[List[Int], Unit]) => os.obs)
          .map(identity)
          .assert.elemChanges()(add = 3)
        val action = dsl.action("add 3")(_ => {
          postCheckAttempts = 0
        }) +> check
        val obs = Observer.watch({
          postCheckAttempts += 1
          if (postCheckAttempts >= 2) items = List(1, 2, 3)
          items
        })
        val plan = Plan.action(action)
        val test = plan.test(obs).stateless
          .withRetryPolicy(Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 2))
        val report = test.runU()
        report.assert()
      }

      "elemChangesRetryWithIterator" - {
        import scala.concurrent.duration._
        import teststate.run.Retry
        var items = List(1, 2)
        var postCheckAttempts = 0
        val dsl = Dsl[Unit, List[Int], Unit]
        val check = dsl.focus("Items")
          .collection((os: OS[List[Int], Unit]) => os.obs.iterator)
          .assert.elemChanges()(add = 3)
        val action = dsl.action("add 3")(_ => {
          postCheckAttempts = 0
        }) +> check
        val obs = Observer.watch({
          postCheckAttempts += 1
          if (postCheckAttempts >= 2) items = List(1, 2, 3)
          items
        })
        val plan = Plan.action(action)
        val test = plan.test(obs).stateless
          .withRetryPolicy(Retry.Policy.fixedIntervalAndAttempts(Duration.Zero, 2))
        val report = test.runU()
        report.assert()
      }

      "elemChangesMutable" - {
        val buf = scala.collection.mutable.ArrayBuffer(1, 2)
        val dsl = Dsl[Unit, Unit, Unit]
        val check = dsl.focus("Items")
          .collection(_ => buf)
          .map(_ * 2)
          .assert.elemChanges()(add = 6)
        val action = dsl.action("mutate")(_ => buf += 3) +> check
        val report = Plan.action(action).testU.stateless.runU()
        report.assert()
      }
    }

    "increaseBy" - {
      val dsl = Dsl[Unit, Int, Unit]
      val focus = dsl.focus("Counter").value(_.obs)
      val fmt = Report.Format.Default.uncolouredAscii.alwaysShowChildren

      "pass" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 9) +> focus.assert.increaseBy(2)
        Plan.action(a).test(Observer.watch(x)).stateless.runU().assert()
      }

      "failDifferent" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 8) +> focus.assert.increaseBy(2)
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Got 8, expected 7 to change into 9.")
      }

      "failUnchanged" - {
        val x = 7
        val a = dsl.action("act")(_ => ()) +> focus.assert.increaseBy(2)
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Expected 7 to change, but it didn't.")
      }

      "negPass" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 8) +> focus.assert.not.increaseBy(2)
        Plan.action(a).test(Observer.watch(x)).stateless.runU().assert()
      }

      "negFail" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 9) +> focus.assert.not.increaseBy(2)
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Expected 9 to change, but it didn't.")
      }
    }

    "decreaseBy" - {
      val dsl = Dsl[Unit, Int, Unit]
      val focus = dsl.focus("Counter").value(_.obs)
      val fmt = Report.Format.Default.uncolouredAscii.alwaysShowChildren

      "pass" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 5) +> focus.assert.decreaseBy(2)
        Plan.action(a).test(Observer.watch(x)).stateless.runU().assert()
      }

      "failDifferent" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 6) +> focus.assert.decreaseBy(2)
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Got 6, expected 7 to change into 5.")
      }

      "failUnchanged" - {
        val x = 7
        val a = dsl.action("act")(_ => ()) +> focus.assert.decreaseBy(2)
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Expected 7 to change, but it didn't.")
      }
    }

    "increment" - {
      val dsl = Dsl[Unit, Int, Unit]
      val focus = dsl.focus("Counter").value(_.obs)
      val fmt = Report.Format.Default.uncolouredAscii.alwaysShowChildren

      "pass" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 8) +> focus.assert.increment
        Plan.action(a).test(Observer.watch(x)).stateless.runU().assert()
      }

      "failDifferent" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 10) +> focus.assert.increment
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Got 10, expected 7 to change into 8.")
      }
    }

    "decrement" - {
      val dsl = Dsl[Unit, Int, Unit]
      val focus = dsl.focus("Counter").value(_.obs)
      val fmt = Report.Format.Default.uncolouredAscii.alwaysShowChildren

      "pass" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 6) +> focus.assert.decrement
        Plan.action(a).test(Observer.watch(x)).stateless.runU().assert()
      }

      "failDifferent" - {
        var x = 7
        val a = dsl.action("act")(_ => x = 10) +> focus.assert.decrement
        val r = Plan.action(a).test(Observer.watch(x)).stateless.runU()
        val text = r.format(fmt)
        assertContains(text, "Got 10, expected 7 to change into 6.")
      }
    }

    "nameUtils" - {
      import teststate.dsl.NameUtils

      "collChangeFn" - {
        val fn = NameUtils.collChangeFn[Unit, Int]("Items", true, "change", _ => List(1), _ => List(2))
        val name = fn(Some(BeforeAfter((), ())))
        assertEq(name.value, "Items should change: -1 +2.")
        assertEq(name.value, "Items should change: -1 +2.")
      }

      "subjectShouldVerbLaziness" - {
        var evaluated = false
        def focusName = { evaluated = true; "Items" }
        val name = NameUtils.subjectShouldVerb(focusName, true, "verb")
        assert(!evaluated)
        assertEq(name.value, "Items should verb.")
        assert(evaluated)
      }

      "equalLaziness" - {
        var evaluated = false
        def focusName = { evaluated = true; "Items" }
        val name = NameUtils.equal(focusName, true, 3)
        assert(!evaluated)
        assertEq(name.value, "Items should be 3.")
        assert(evaluated)
      }

      "equalFnLaziness" - {
        var evaluated = false
        def focusName = { evaluated = true; "Items" }
        val fn = NameUtils.equalFn[Unit, Int](focusName, true, _ => 3)
        assert(!evaluated)
        assertEq(fn(None).value, "Items should be <?>.")
        assert(evaluated)
      }

      "equalMultiline" - {
        val name = NameUtils.equal("Data", true, "line1\nline2")
        assertEq(name.value, "Data should be:\n  line1\n  line2")
      }
    }

  }
}
