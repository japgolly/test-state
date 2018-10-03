package teststate.domzipper.jsoup

import japgolly.microlibs.testutil.TestUtil._
import org.jsoup.Jsoup
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.option._
import scalaz.std.set._
import scalaz.std.vector._
import utest._
import Exports._

object DomZipperJsoupTest extends TestSuite {

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default >> HtmlScrub.joinLines

  val htmlSource =
    """
      |<!DOCTYPE html>
      |<html>
      |<head></head>
      |<body>
      |
      |  <form>
      |    <div class="name">
      |      <label for="name">Name:</label>
      |      <input type="text" id="name" name="user_name" value="Bob Loblaw" class=" a b  c ">
      |    </div>
      |    <select>
      |      <option value="volvo">Volvo</option>
      |      <option value="saab" selected="selected">Saab</option>
      |      <option value="mercedes">Mercedes</option>
      |      <option value="audi">Audi</option>
      |    </select>
      |    <select>
      |      <option value="a">A</option>
      |      <option value="b">B</option>
      |    </select>
      |    <div>
      |      <h3>EH??</h3>
      |      <input type="checkbox" id="coding" name="interest" value="coding" checked>
      |      <label for="coding" data-coding="1">Coding</label>
      |    </div>
      |    <h3>HI</h3>
      |    <div>
      |      <div><h3>EH?</h3></div>
      |      <input type="checkbox" id="music" name="interest" value="music">
      |      <label for="music">Music</label>
      |    </div>
      |  </form>
      |
      |</body>
      |</html>
    """.stripMargin

  lazy val $ : DomZipperJsoup =
    DomZipperJsoup(Jsoup.parse(htmlSource))

  def name = $("#name")
  def nameLabelHtml = """<label for="name">Name:</label>"""
  def nameInputHtml = """<input type="text" id="name" name="user_name" value="Bob Loblaw" class=" a b  c ">"""
  def checkboxes = $.collect0n("input[type=checkbox]")

  override def tests = Tests {

    'outerHTML - assertEq(name.outerHTML, nameInputHtml)

    'innerHTML - assertEq($("div.name").innerHTML.split("\n").map(_.trim).mkString, nameLabelHtml + nameInputHtml)

    'innerText - assertEq($("div.name").innerText, "Name:")

    'value - assertEq(name.value, "Bob Loblaw")

    'checkedT - assertEq($("input[type=checkbox]", 1 of 2).checked, true)
    'checkedF - assertEq($("input[type=checkbox]", 2 of 2).checked, false)

    'collect - {
      assertEq(checkboxes.size, 2)
      assertEq(checkboxes.mapDoms(_.isSelected), Vector(true, false))
      assertEq(checkboxes.map(_.checked), Vector(true, false))
    }

    'classes {
      'none - assertEq($("form").classes, Set.empty[String])
      'some - assertEq(name.classes, Set("a", "b", "c"))
    }

    'selectedOption {
      'nonSelect - assertEq($.failToOption.selectedOption.map(_ => ()), None)
      'some - assertEq($("select", 1 of 2).selectedOptionText, Some("Saab"))
      'none - assertEq($("select", 2 of 2).selectedOptionText, None)
    }

    'findSelfOrChildWithAttribute - {
      def attr = "data-coding"
      def html = """<label for="coding" data-coding="1">Coding</label>"""
      def child = $.findSelfOrChildWithAttribute(attr)
      'child - assertEq(child.map(_.outerHTML), Some(html))
      'self - assertEq(child.flatMap(_.findSelfOrChildWithAttribute(attr).map(_.outerHTML)), Some(html))
    }

    'matches - {
      val x = $("input[type=checkbox]", 1 of 2)
      assert(x.matches("input"))
      assert(!x.matches("a"))
      assert(x.matches("input[type=checkbox]"))
      assert(!x.matches("input[type=text]"))
      assert(x.matches("div > input"))
      assert(!x.matches("body a input"))
    }

    'child - {
      'sole - assertEq($("form").child("h3").innerText, "HI")
      'nOfM - assertEq($("form").child("div", 2 of 3)("h3").innerText, "EH??")
    }

    'children - {
      'nullary - assertEq(
        $("form").children1n.doms.map(_.getTagName.toLowerCase),
        Vector("div", "select", "select", "div", "h3", "div"))

      'sel - assertEq($("form").children1n("h3").innerTexts, Vector("HI"))
    }

    'parentAndChild - {
      val c = $("form").child("div", 3 of 3)("h3")
      assertEq(c.outerHTML, "<h3>EH?</h3>")
      assertEq(c.parent.outerHTML, "<div><h3>EH?</h3></div>")
      assertEq(c.parent.child().outerHTML, "<h3>EH?</h3>")
    }
  }

}
