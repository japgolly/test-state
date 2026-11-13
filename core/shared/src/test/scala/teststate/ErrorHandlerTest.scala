package teststate

import java.util.regex.Pattern
import teststate.Exports._
import teststate.TestUtil._
import teststate.typeclass.ErrorHandler
import utest._

object ErrorHandlerTest extends TestSuite {

  override def tests = Tests {

    "name" - {
      "successSome" - {
        val eh = ErrorHandler.byToString
        val fn = NameFn[Int](o => s"val=${o.get}")
        val n = eh.name(fn, Some(123))
        assertEq(n.value, "val=123")
      }

      "fallbackToNoneOnSomeException" - {
        val eh = ErrorHandler.byToString
        val fn = NameFn[Int](o => o.fold("none")(_ => sys.error("boom")))
        val n = eh.name(fn, Some(123))
        assertEq(n.value, "none")
      }

      "exceptionOnNone" - {
        val eh = ErrorHandler.byToString
        val fn = NameFn[Int](_ => sys.error("boom"))
        val n = eh.name(fn, None)
        assertEq(n.value, "Name exception: java.lang.RuntimeException: boom")
      }

      "exceptionOnSomeAndNone" - {
        val eh = ErrorHandler[String](_.getMessage)
        val fn = NameFn[Int](_ => sys.error("custom error"))
        val n = eh.name(fn, Some(123))
        assertEq(n.value, "Name exception: custom error")
      }
    }

    "toStringWithStackTrace" - {
      val err = new RuntimeException("test error")

      "noDuplicateFirstLine" - {
        val eh = ErrorHandler.toStringWithStackTrace
        val s = eh.toE(err)
        val lines = s.split('\n').map(_.trim).toList
        assertEq(lines.head, "java.lang.RuntimeException: test error")
        assert(lines.tail.nonEmpty)
        assert(lines(1) != "java.lang.RuntimeException: test error")
        assert(lines(1).startsWith("at "))
      }

      "modFilterAll" - {
        val eh = ErrorHandler.toStringWithStackTrace(_ => Nil)
        val s = eh.toE(err)
        assertEq(s, "java.lang.RuntimeException: test error")
      }

      "modTakeFrames" - {
        val eh = ErrorHandler.toStringWithStackTrace(_.take(2))
        val s = eh.toE(err)
        val lines = s.split('\n').toList
        assertEq(lines.size, 3)
        assertEq(lines.head, "java.lang.RuntimeException: test error")
        assert(lines(1).trim.startsWith("at "))
        assert(lines(2).trim.startsWith("at "))
      }

      "pattern" - {
        val eh = ErrorHandler.toStringWithStackTrace(Pattern.compile("ErrorHandlerTest"))
        val s = eh.toE(err)
        val lines = s.split('\n').toList
        assertEq(lines.head, "java.lang.RuntimeException: test error")
        assert(lines.tail.forall(_.contains("ErrorHandlerTest")))
      }
    }

  }
}
