package teststate.domzipper

import cats.instances.string._
import japgolly.microlibs.testutil.TestUtil._
import utest._

object HtmlScrubTest extends TestSuite {

  override def tests = Tests {

    "empty" - {
      assert(HtmlScrub.empty eq HtmlScrub.empty)
      assertEq(HtmlScrub.empty.run("<a>\n  b</a>"), "<a>\n  b</a>")
    }

    "joinLines" - {
      assert(HtmlScrub.joinLines eq HtmlScrub.joinLines)
      assertEq(HtmlScrub.joinLines.run("<a>\n  <b>\n  c\n  d</b></a>"), "<a><b> c d</b></a>")
    }

    "composeIdentity" - {
      val s = HtmlScrub(_.trim)
      assert((HtmlScrub.empty >> s) eq s)
      assert((s >> HtmlScrub.empty) eq s)
      assert((HtmlScrub.empty << s) eq s)
      assert((s << HtmlScrub.empty) eq s)
    }

    "composeOrder" - {
      val a = HtmlScrub(_ + "A")
      val b = HtmlScrub(_ + "B")
      assertEq((a >> b).run(""), "AB")
      assertEq((a << b).run(""), "BA")
    }

  }
}
