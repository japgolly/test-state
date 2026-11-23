package teststate

import teststate.Exports._
import teststate.TestUtil._
import teststate.data.Result.{Fail, Pass}
import teststate.run.History
import teststate.run.History.Step
import utest._

object HistoryTest extends TestSuite {

  override def tests = Tests {

    "appendAndConcat" - {
      val s1 = Step(Name("a"), Pass)
      val s2 = Step(Name("b"), Fail("err"))

      val h1 = History.empty :+ s1
      assertEq(h1.steps.length, 1)
      assert(!h1.failed)

      val h2 = h1 :+ s2
      assertEq(h2.steps.length, 2)
      assert(h2.failed)
      assertEq(h2.failure, Some("err"))

      assert((h2 ++ History.empty) eq h2)
      assert((History.empty ++ h2) eq h2)
      assert((h2 ++ Vector.empty) eq h2)

      val emptyFail = History(Vector.empty, Fail("e"))
      assertEq((emptyFail ++ History.empty).failure, Some("e"))
      assertEq((History.empty ++ emptyFail).failure, Some("e"))
    }

    "rootFailurePath" - {
      "passedIsEmpty" - {
        val h = History(Step(Name("ok"), Pass))
        assert(h.rootFailurePath.isEmpty)
      }

      "stackSafe" - {
        val depth = 50000
        var h: History[String] = History(Step(Name.now("leaf"), Fail("boom")))
        var i = 1
        while (i <= depth) {
          h = History(History.parent(Name.now(s"lvl-$i"), h))
          i += 1
        }
        val path = h.rootFailurePath
        assertEq(path.length, depth + 1)
        assertEq(path.head.name.value, s"lvl-$depth")
        assertEq(path.last.name.value, "leaf")
      }
    }

  }
}
