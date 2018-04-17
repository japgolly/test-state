package teststate.domzipper.selenium

import japgolly.microlibs.testutil.TestUtil._
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import utest._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.option._
import scalaz.std.vector._
import Exports._

object SeleniumTest extends TestSuite {

  lazy val $ : DomZipperSelenium = {
    val testHtmlPath = SeleniumTestUtil.testResource("test.html").getAbsoluteFile
    val options = new ChromeOptions()
    options.setHeadless(true)
    options.addArguments("--no-sandbox") // Travis workaround: https://github.com/SeleniumHQ/selenium/issues/4961
    val driver = new ChromeDriver(options)
    driver.get("file://" + testHtmlPath)
    DomZipperSelenium.html(driver)
  }

  def name = $("#name")
  def nameLabelHtml = """<label for="name">Name:</label>"""
  def nameInputHtml = """<input type="text" id="name" name="user_name" value="Bob Loblaw">"""
  def checkboxes = $.collect0n("input[type=checkbox]")

  override def tests = SeleniumTestUtil.CI match {
    case None => TestSuite {

      'outerHTML - assertEq(name.outerHTML, nameInputHtml)

      'innerHTML - assertEq($("div", 1 of 3).innerHTML.split("\n").map(_.trim).mkString, nameLabelHtml + nameInputHtml)

      'innerText - assertEq($("div", 1 of 3).innerText, "Name:")

      'value - assertEq(name.value, "Bob Loblaw")

      'checkedT - assertEq($("input[type=checkbox]", 1 of 2).checked, true)
      'checkedF - assertEq($("input[type=checkbox]", 2 of 2).checked, false)

      'collect - {
        assertEq(checkboxes.size, 2)
        assertEq(checkboxes.mapDoms(_.isSelected), Vector(true, false))
        assertEq(checkboxes.mapZippers(_.checked), Vector(true, false))
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
    }

    case Some(_) => TestSuite {}
  }
}
