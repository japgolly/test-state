package teststate

import teststate.TestUtil._
import utest._

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

  val test = Test(
    a(1)
    >> a(2)
    >> (a(3) >> a(4)).group("A34").addCheck(f.assert.equal(expectAt(4)).after)
  )(_.s)

  def newState = new RecordVar(Record(Vector.empty))

  def testHistory(h: History[String], expect: String): Unit = {
    val actual = h.format(History.Options.uncolored).trim
    assertEq(actual = actual, expect.trim)
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
        val test = Test(*.action("A").act(_ => sys error "Crash!"))(_.s)
        testHistory(test.run((), newState),
          """
            |✘ A -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      'before {
        val test = Test(nop, badPoint.before)(_.s)
        testHistory(test.run((), newState),
          """
            |✘ NOP
            |  ✘ Pre-conditions
            |    ✘ OMG -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      'after {
        val test = Test(nop, badPoint.after)(_.s)
        testHistory(test.run((), newState),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ OMG -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }

      'around {
        val test = Test(nop, *.focus("").value(_ => 0).testAround(_ => "what?", (_: Any, _: Any) => sys error "Crashhh!"))(_.s)
        testHistory(test.run((), newState),
          """
            |✘ NOP
            |  ✓ Action
            |  ✘ Post-conditions
            |    ✘ what? -- Caught exception: java.lang.RuntimeException: Crashhh!
          """.stripMargin)
      }

      'invariants {
        val test = Test(nop, badPoint)(_.s)
        testHistory(test.run((), newState),
          """
            |✘ Initial state.
            |  ✘ OMG -- Caught exception: java.lang.RuntimeException: Crash!
          """.stripMargin)
      }
    }
  }
}
