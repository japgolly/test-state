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

  }
}
