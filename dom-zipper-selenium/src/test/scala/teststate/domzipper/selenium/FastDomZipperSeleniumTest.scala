package teststate.domzipper.selenium

import japgolly.microlibs.testutil.TestUtil._
import java.util.concurrent.TimeUnit
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import utest._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.option._
import scalaz.std.set._
import scalaz.std.vector._
import Exports._

object FastDomZipperSeleniumTest extends TestSuite {

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default >> HtmlScrub.joinLines

  lazy val $ : FastDomZipperSelenium = {
    val testHtmlPath = SeleniumTestUtil.testResource("test.html").getAbsoluteFile
    val options = new ChromeOptions()
    options.setHeadless(true)
    options.addArguments("--no-sandbox") // Travis workaround: https://github.com/SeleniumHQ/selenium/issues/4961
    val driver = new ChromeDriver(options)
    driver.manage().timeouts().implicitlyWait(1, TimeUnit.NANOSECONDS)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = driver.quit()
    })
    driver.get("file://" + testHtmlPath)
    FastDomZipperSelenium.html(driver)
  }

  def name = $("#name")
  def nameLabelHtml = """<label for="name">Name:</label>"""
  def nameInputHtml = """<input type="text" id="name" name="user_name" value="Bob Loblaw" class=" a b  c ">"""
  def checkboxes = $.collect0n("input[type=checkbox]")

  override def tests = Tests {

    'fast {
      'outerHTML - assertEq(name.outerHTML, nameInputHtml)

      'innerHTML - assertEq($("div.name").innerHTML.split("\n").map(_.trim).mkString, nameLabelHtml + nameInputHtml)

      'innerText - assertEq($("div.name").innerText, "Name:")

      'value - assertEq(name.value, "Bob Loblaw")

      'checkedT - assertEq($("input[type=checkbox]", 1 of 2).checked, true)
      'checkedF - assertEq($("input[type=checkbox]", 2 of 2).checked, false)

      'collect - {
        assertEq(checkboxes.size, 2)
  //      assertEq(checkboxes.mapDoms(_.isSelected), Vector(true, false))
        assertEq(checkboxes.map(_.checked), Vector(true, false))
      }

      'classes {
        'none - assertEq($("form").classes, Set.empty[String])
        'some - assertEq(name.classes, Set("a", "b", "c"))
      }

  //    'selectedOption {
  //      'nonSelect - assertEq($.failToOption.selectedOption.map(_ => ()), None)
  //      'some - assertEq($("select", 1 of 2).selectedOptionText, Some("Saab"))
  //      'none - assertEq($("select", 2 of 2).selectedOptionText, None)
  //    }

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
          $("form").children1n.map(_.tagName.toLowerCase),
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

    'slow - {
      def outerHtml(z: FastDomZipperSelenium) = htmlScrub.run(z.dom().getAttribute("outerHTML"))
      'outerHTML - assertEq(outerHtml(name), nameInputHtml)
      'innerText - assertEq($("div.name").dom().getText, "Name:")
      'checkedT - assertEq($("input[type=checkbox]", 1 of 2).dom().isSelected, true)
      'checkedF - assertEq($("input[type=checkbox]", 2 of 2).dom().isSelected, false)
      'parentAndChild - {
        val c = $("form").child("div", 3 of 3)("h3")
        assertEq(outerHtml(c), "<h3>EH?</h3>")
        assertEq(outerHtml(c.parent), "<div><h3>EH?</h3></div>")
        assertEq(outerHtml(c.parent.child()), "<h3>EH?</h3>")
      }
    }
  }
}
