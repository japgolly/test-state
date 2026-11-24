package teststate.domzipper

import cats.instances.boolean._
import cats.instances.int._
import cats.instances.option._
import cats.instances.set._
import cats.instances.string._
import cats.instances.vector._
import japgolly.microlibs.testutil.TestUtil._
import org.scalajs.dom
import teststate.domzipper.sizzle.Exports._
import utest._

object DomZipperJsTest extends TestSuite {

  private def body =
    """
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
    """.stripMargin.trim

  lazy val $ : DomZipperJs = {
    val div = dom.document.createElement("div")
    div.innerHTML = body
    dom.document.body.appendChild(div)
    DomZipperJs(div.asInstanceOf[dom.html.Element])
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
      assertEq(checkboxes.map(_.checked), Vector(true, false))
    }

    "classes" - {
      "none" - assertEq($("form").classes, Set.empty[String])
      "some" - assertEq(name.classes, Set("a", "b", "c"))
      "svg" - {
        val svg = dom.document.createElementNS("http://www.w3.org/2000/svg", "svg")
        svg.setAttribute("class", "icon active")
        assertEq(DomZipperJs(svg).classes, Set("icon", "active"))
      }
    }

    "dynamicMethod" - {
      "nullReturnsNone" - assertEq(name.dynamicMethod[String](_ => null), None)
      "undefinedReturnsNone" - assertEq(name.dynamicMethod[String](_.nonExistentProp), None)
      "defined" - {
        assertEq(name.dynamicMethod[String](_.value), Some("Bob Loblaw"))
        assertEq(name.dynamicMethod[Boolean](_.disabled), Some(false))
      }
      "valueWithNullFailsCleanly" - {
        val el = dom.document.createElement("div")
        el.asInstanceOf[scala.scalajs.js.Dynamic].value = null
        assertEq(DomZipperJs.failToOption(el).value, None)
      }
    }

    "selectedOption" - {
      "nonSelect" - assertEq($.failToOption.selectedOption.map(_ => ()), None)
      "some" - assertEq($("select", 1 of 2).selectedOptionText, Some("Saab"))
      "default" - assertEq($("select", 2 of 2).selectedOptionText, Some("A"))
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
        Vector("div", "select", "select", "div", "h3", "div"))

      "sel" - assertEq($("form").children1n("h3").innerTexts, Vector("HI"))
    }

    "parentAndChild" - {
      val c = $("form").child("div", 3 of 3)("h3")
      assertEq(c.outerHTML, "<h3>EH?</h3>")
      assertEq(c.parent.outerHTML, "<div><h3>EH?</h3></div>")
      assertEq(c.parent.child().outerHTML, "<h3>EH?</h3>")
    }

    "enrichErrOnFailures" - {
      "needAttribute" - {
        val Left(err) = $("form").failToEither.needAttribute("nonexistent")
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
      }

      "checked" - {
        val Left(err) = $("form").failToEither.checked
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
      }

      "value" - {
        val Left(err) = $("form").failToEither.value
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
      }

      "parent" - {
        val Left(err) = DomZipperJs.failToEither(dom.document.createElement("div")).parent
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
      }

      "domAs" - {
        val Left(err) = $("form").failToEither.domAsInput
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
      }

      "domsAs" - {
        val Left(err) = $.failToEither.collect1n("form").domsAsInput
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
        err
      }
    }

    "addStyleTag" - {
      "withHead" - {
        val doc = dom.document.implementation.createHTMLDocument("test")
        doc.addStyleTag("body { color: red; }")
        val head = doc.getElementsByTagName("head")(0)
        assertEq(head.getElementsByTagName("style").length, 1)
        assertEq(head.getElementsByTagName("style")(0).textContent, "body { color: red; }")
      }

      "withoutHead" - {
        val doc = dom.document.implementation.createHTMLDocument("test")
        val head = doc.getElementsByTagName("head")(0)
        val _ = head.parentNode.removeChild(head)
        doc.addStyleTag("body { color: blue; }")
        assertEq(doc.documentElement.getElementsByTagName("style").length, 1)
        assertEq(doc.documentElement.getElementsByTagName("style")(0).textContent, "body { color: blue; }")
      }
    }

  }
}
