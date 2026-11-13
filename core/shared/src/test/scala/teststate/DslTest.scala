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

  }
}
