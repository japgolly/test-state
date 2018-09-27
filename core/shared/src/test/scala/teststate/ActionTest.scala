package teststate

import teststate.Exports._
import utest._
import TestUtil._
import scalaz.std.string._
import scalaz.std.vector._
import teststate.data.VectorTree

object ActionTest extends TestSuite {

  val * = Dsl[Unit, Unit, Unit]

  val a1 = *.action("A1")(_ => ())
  val a2 = *.action("A2")(_ => ())
  val a3 = *.action("A3")(_ => ())
  val c1 = *.chooseAction("C1")(_ => a1)
  val g1 = (a1 >> a2).group("G1")

  override def tests = TestSuite {

    'topLevelNames - {
      def test(actual: Vector[String])(expect: String*): Unit =
        assertEq(actual, expect.toVector)

      'single - test(a1.topLevelNames)("A1")
      'group - test(g1.topLevelNames)("G1")
      'product - test((a1 >> a2 >> a3).topLevelNames)("A1", "A2", "A3")
      'coproduct - test(c1.topLevelNames)("C1")
      'mix - test((a3 >> c1 >> g1 >> a3).topLevelNames)("A3", "C1", "G1", "A3")
    }

    'nameTree {
      def test(actual: VectorTree[String])(expect: String*): Unit =
        assertMultiline(actual.prettyPrintedLines().mkString("\n"), expect.mkString("\n"))

      'single - test(a1.nameTree)("A1")
      'group - test(g1.nameTree)("G1", "  A1", "  A2")
      'product - test((a1 >> a2 >> a3).nameTree)("A1", "A2", "A3")
      'coproduct - test(c1.nameTree)("C1")
      'mix - test((a3 >> c1 >> g1 >> a3).nameTree)("A3", "C1", "G1", "  A1", "  A2", "A3")
    }

    'toReport {
      val report: Report[Nothing] = (a3 >> g1 >> c1 >> a2).toReport
      val text = report.format(Report.Format.Default.uncolouredAscii.alwaysShowChildren)
      assertEq(text.trim,
        """
          |[skip] A3
          |[skip] G1
          |  [skip] A1
          |  [skip] A2
          |[skip] C1
          |[skip] A2
        """.stripMargin.trim)
    }

  }
}
