package teststate

import utest._
import teststate.core.Around
import teststate.data.{OS, Sack, Left, Right}
import teststate.Exports._
import teststate.TestUtil._

object DslTest extends TestSuite {

  val * = Dsl.sync[Unit, Unit, Unit, String]

  val options = History.Options.uncolored.alwaysShowChildren

  def test(a: *.Action, i: *.Invariant)(expect: String): Unit = {
    val h = Test(a, i).observe(_ => ()).run((), ())
    val actual = h.format(options).trim
    assertEq(actual = actual, expect.trim)
  }

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
    assertEq(n(Some(null)).value, expectC)
  }

  override def tests = TestSuite {

    'changesTo {
      'pos - testName(*.focus("Counter").value(_ => 7).assert.changesTo(_ + 1),
        "Counter should be <?>.",
        "Counter should be 8.")

      'neg - testName(*.focus("Counter").value(_ => 7).assert.not.changesTo(_ + 1),
        "Counter shouldn't be <?>.",
        "Counter shouldn't be 8.")

      // TODO Didn't catch. Try using changeTo in multiple actions and verify final history. Might be Name strictness.
    }

    'incrementBy {
      'pos - testName(*.focus("Counter").value(_ => 7).assert.increaseBy(2),
        "Counter should increase by 2.",
        "Counter should be 9.")

      'neg - testName(*.focus("Counter").value(_ => 7).assert.not.increaseBy(2),
        "Counter shouldn't increase by 2.",
        "Counter shouldn't be 9.")
    }

    'decrementBy {
      'pos - testName(*.focus("Counter").value(_ => 7).assert.decreaseBy(2),
        "Counter should decrease by 2.",
        "Counter should be 5.")

      'neg - testName(*.focus("Counter").value(_ => 7).assert.not.decreaseBy(2),
        "Counter shouldn't decrease by 2.",
        "Counter shouldn't be 5.")
    }

  }
}
