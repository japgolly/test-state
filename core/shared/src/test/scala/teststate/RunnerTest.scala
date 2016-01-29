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

  val test = Test(
    a(1)
    >> a(2)
    >> (a(3) >> a(4)).group("A34").addCheck(f.assert.equal(expectAt(4)).after)
  )(_.s)

  override def tests = TestSuite {
    val v = new RecordVar(Record(Vector.empty))
    val h = test.run((), v)
    val actual = formatHistory(h, Options.uncolored).trim
    val expect =
      """
        |✓ A1
        |✓ A2
        |✓ A34
        |✓ All pass.
      """.stripMargin
    assertEq(actual = actual, expect.trim)
    assertEq(actual = v.s, Record(Vector("A1", "A2", "A3", "A4")))
  }
}
