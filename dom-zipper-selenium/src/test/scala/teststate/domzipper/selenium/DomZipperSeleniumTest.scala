package teststate.domzipper.selenium

import cats.instances.option._
import cats.instances.set._
import cats.instances.string._
import cats.instances.vector._
import japgolly.microlibs.testutil.TestUtil._
import java.util.concurrent.TimeUnit
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import teststate.domzipper.selenium.Exports._
import utest._

object DomZipperSeleniumTest extends TestSuite {

  lazy val $ : DomZipperSelenium = {
    val testHtmlPath = SeleniumTestUtil.testResource("test.html").getAbsoluteFile
    val options = new ChromeOptions()
    options.setHeadless(true)
    options.addArguments("--no-sandbox") // Travis workaround: https://github.com/SeleniumHQ/selenium/issues/4961
    val driver = new ChromeDriver(options)
    driver.manage().timeouts().implicitlyWait(1, TimeUnit.NANOSECONDS)
    driver.onShutdownQuit()
    driver.get("file://" + testHtmlPath)
    DomZipperSelenium.html(driver)
  }

  def name = $("#name")
  def nameLabelHtml = """<label for="name">Name:</label>"""
  def nameInputHtml = """<input type="text" id="name" name="user_name" value="Bob Loblaw" class=" a b  c ">"""
  def checkboxes = $.collect0n("input[type=checkbox]")

  override def tests = Tests {

    "outerHTML" - assertEq(name.outerHTML, nameInputHtml)

    "innerHTML" - assertEq($("div.name").innerHTML.split("\n").map(_.trim).mkString, nameLabelHtml + nameInputHtml)

    "innerText" - assertEq($("div.name").innerText, "Name:")

    "value" - assertEq(name.value, "Bob Loblaw")

    "checkedT" - assertEq($("input[type=checkbox]", 1 of 2).checked, true)
    "checkedF" - assertEq($("input[type=checkbox]", 2 of 2).checked, false)

    "collect" - {
      assertEq(checkboxes.size, 2)
      assertEq(checkboxes.map(_.dom().isSelected), Vector(true, false))
      assertEq(checkboxes.map(_.checked), Vector(true, false))
    }

    "classes" - {
      "none" - assertEq($("form").classes, Set.empty[String])
      "some" - assertEq(name.classes, Set("a", "b", "c"))
    }

    "selectedOption" - {
      "nonSelect" - assertEq($.failToOption.selectedOption.map(_ => ()), None)
      "some" - assertEq($("select", 1 of 2).selectedOptionText, Some("Saab"))
      "none" - assertEq($("select", 2 of 2).selectedOptionText, None)
    }

    "findSelfOrChildWithAttribute" - {
      def attr = "data-coding"
      def html = """<label for="coding" data-coding="1">Coding</label>"""
      def child = $.findSelfOrChildWithAttribute(attr)
      "child" - assertEq(child.map(_.outerHTML), Some(html))
      "self" - assertEq(child.flatMap(_.findSelfOrChildWithAttribute(attr).map(_.outerHTML)), Some(html))
    }

    "matches" - {
      val x = $("input[type=checkbox]", 1 of 2)
      assert(x.matches("input"))
      assert(!x.matches("a"))
      assert(x.matches("input[type=checkbox]"))
      assert(!x.matches("input[type=text]"))
      assert(x.matches("div > input"))
      assert(!x.matches("body a input"))
    }

    "child" - {
      "sole" - assertEq($("form").child("h3").innerText, "HI")
      "nOfM" - assertEq($("form").child("div", 2 of 3)("h3").innerText, "EH??")
    }

    "children" - {
      "nullary" - assertEq(
        $("form").children1n.map(_.tagName.toLowerCase),
        Vector("div", "select", "select", "div", "h3", "div", "section"))

      "sel" - assertEq($("form").children1n("h3").innerTexts, Vector("HI"))
    }

    "parentAndChild" - {
      val c = $("form").child("div", 3 of 3)("h3")
      assertEq(c.outerHTML, "<h3>EH?</h3>")
      assertEq(c.parent.outerHTML, "<div><h3>EH?</h3></div>")
      assertEq(c.parent.child().outerHTML, "<h3>EH?</h3>")
    }

  }
}
