package teststate.domzipper.jsoup

import cats.instances.int._
import cats.instances.option._
import cats.instances.set._
import cats.instances.string._
import cats.instances.vector._
import japgolly.microlibs.testutil.TestUtil._
import org.jsoup.Jsoup
import teststate.domzipper.jsoup.Exports._
import teststate.domzipper.{DomZippersFastAndSlow, ErrorHandler}
import utest._

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

    "outerHTML" - assertEq(name.outerHTML, nameInputHtml)

    "innerHTML" - assertEq($("div.name").innerHTML.split("\n").map(_.trim).mkString, s"$nameLabelHtml $nameInputHtml")

    "innerText" - assertEq($("div.name").innerText, "Name:")

    "value" - assertEq(name.value, "Bob Loblaw")

    "checkedT" - assertEq($("input[type=checkbox]", 1 of 2).checked, true)
    "checkedF" - assertEq($("input[type=checkbox]", 2 of 2).checked, false)

    "collect" - {
      assertEq(checkboxes.size, 2)
      assertEq(checkboxes.mapDoms(_.isSelected), Vector(true, false))
      assertEq(checkboxes.map(_.checked), Vector(true, false))
    }

    "classes" - {
      "none" - assertEq($("form").classes, Set.empty[String])
      "some" - assertEq(name.classes, Set("a", "b", "c"))
    }

    "selectedOption" - {
      "nonSelect" - assertEq($.failToOption.selectedOption.map(_ => ()), None)
      "explicit" - assertEq($("select", 1 of 2).selectedOptionText, Some("Saab"))
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
      "nameOnSuccess" - {
        val c = $("form").child("FirstDiv", "div", 1 of 3)
        assertContains(c.describe, "FirstDiv")
      }
      "err0" - {
        val Left(actual) = $("form").failToEither.child("MyButton", "button")
        val expect = "MyButton query failed: [> button]. Expected 1 results, not 0."
        assertContains(actual, expect)
      }
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

    "collectErrMsgs" - {
      "1n" - {
        val expect =
          """
            |Query failed: [pre]. Expected [1,n] results, not 0.
            |DESC: root -> .name
            |HTML: <div class="name"><label for="name">Name:</label> <input type="text" id="name" name="user_name" value="Bob Loblaw" class=" a b  c "></div>
          """.stripMargin.trim
        val Left(actual) = $(".name").failToEither.collect1n("pre").zippers
        assertMultiline(actual, expect)
      }
      "01" - {
        val expect =
          """
            |Query failed: [option]. Expected [0,1] results, not 2.
            |DESC: root -> form -> select
            |HTML: <select> <option value="a">A</option> <option value="b">B</option> </select>
          """.stripMargin.trim
        val Left(actual) = $("form")("select", 2 of 2).failToEither.collect01("option").zippers
        assertMultiline(actual, expect)
      }
      "01FilterStrict" - {
        val expect =
          """
            |Query failed: [option]. Expected [0,1] results, not 2.
            |DESC: root -> form -> select
            |HTML: <select> <option value="a">A</option> <option value="b">B</option> </select>
          """.stripMargin.trim
        val Left(actual) = $("form")("select", 2 of 2).failToEither.collect01("option").filter(_.value == Right("a")).zippers
        assertMultiline(actual, expect)
      }
    }

    "fastAndSlowConsistency" - {
      implicit def errorHandler: ErrorHandler[Option] = ErrorHandler.ReturnOption
      val slowDoc = Jsoup.parse("<div><h3>Before</h3></div>")
      val slow = DomZipperJsoup.failToOption(slowDoc)
      val fast = DomZipperJsoup.failToOption.parseHtml(slow.outerHTML)
      val both = DomZippersFastAndSlow[Option, DomZipperJsoupF, JsoupElement, JsoupElement, DomZipperJsoupF, JsoupElement, JsoupElement](fast, slow)

      val res = both.ensureConsistency { d =>
        slowDoc.selectFirst("h3").text("After") // mutate live 'slow' DOM mid-observation
        d.innerText
      }
      assertEq(res, None)
    }

    "fastAndSlowCollect01FilterStrict" - {
      implicit def errorHandler: ErrorHandler[Option] = ErrorHandler.ReturnOption
      val doc = Jsoup.parse("<div><button>Cancel</button><button>Submit</button></div>")
      val slow = DomZipperJsoup.failToOption(doc)
      val fast = DomZipperJsoup.failToOption.parseHtml(slow.outerHTML)
      val both = DomZippersFastAndSlow[Option, DomZipperJsoupF, JsoupElement, JsoupElement, DomZipperJsoupF, JsoupElement, JsoupElement](fast, slow)
      val res = both.collect01("button").filter(_.innerText == "Submit").zippers
      assert(res.isEmpty)
    }

    "fastAndSlowCollect01FilterValid" - {
      implicit def errorHandler: ErrorHandler[Option] = ErrorHandler.ReturnOption
      val doc = Jsoup.parse("<div><button>Submit</button></div>")
      val slow = DomZipperJsoup.failToOption(doc)
      val fast = DomZipperJsoup.failToOption.parseHtml(slow.outerHTML)
      val both = DomZippersFastAndSlow[Option, DomZipperJsoupF, JsoupElement, JsoupElement, DomZipperJsoupF, JsoupElement, JsoupElement](fast, slow)
      val res = both.collect01("button").filter(_.innerText == "Submit").zippers
      assert(res.isDefined)
      assert(res.get.isDefined)
      assertEq(res.get.get.innerText, "Submit")
      assertEq(res.get.get.dom().map(_.innerText), Some("Submit"))
    }

    "fastAndSlowCmapDesync" - {
      type E[A] = Either[String, A]
      implicit def errorHandler: ErrorHandler[E] = ErrorHandler.ReturnEither

      def mkBoth(html: String) = {
        val doc = Jsoup.parse(html)
        val slow = DomZipperJsoup.failToEither(doc)
        val fast = DomZipperJsoup.failToEither.parseHtml(slow.outerHTML)
        val both = DomZippersFastAndSlow[E, DomZipperJsoupF, JsoupElement, JsoupElement, DomZipperJsoupF, JsoupElement, JsoupElement](fast, slow)
        (doc, both)
      }

      "collect0nElementRemoved" - {
        val (doc, both) = mkBoth("<div><button>Cancel</button><button>Submit</button></div>")
        val Right(btns) = both.collect0n("button").zippers
        doc.select("button").last().remove()
        val res = btns(1).dom()
        assert(res.isLeft)
        assertContains(res.left.getOrElse(""), "Inconsistency detected")
      }

      "collect01ElementRemoved" - {
        val (doc, both) = mkBoth("<div><button>Submit</button></div>")
        val Right(Some(btn)) = both.collect01("button").zippers
        doc.selectFirst("button").remove()
        val res = btn.dom()
        assert(res.isLeft)
        assertContains(res.left.getOrElse(""), "Inconsistency detected")
      }

      "collect01ElementAdded" - {
        val (doc, both) = mkBoth("<div><button>Submit</button></div>")
        val Right(Some(btn)) = both.collect01("button").zippers
        doc.selectFirst("div").append("<button>Extra</button>")
        val res = btn.dom()
        assert(res.isLeft)
        assertContains(res.left.getOrElse(""), "Expected [0,1] results, not 2.")
      }

      "collect01ToCollect0nFailsWhenInvalid" - {
        val (_, both) = mkBoth("<div><button>Cancel</button><button>Submit</button></div>")
        val res = both.collect01("button").collect0n.zippers
        assert(res.isLeft)
        assertContains(res.left.getOrElse(""), "Expected [0,1] results, not 2.")
      }
    }

    "html" - {
      val h = DomZipperJsoup.parseHtml(htmlSource)
      assertEq(h.tagName, "html")
      assertEq(h.child("body").tagName, "body")
    }

    "body" - {
      val b = DomZipperJsoup.body(Jsoup.parse(htmlSource))
      assertEq(b.tagName, "body")
      assertEq(b.children1n.map(_.tagName), Vector("form"))
    }

    "htmlScrubNotAppliedRedundantly" - {
      val expectedOuter =
        """<!--s--><html>
          | <head></head>
          | <body>
          |  <div>
          |   <span>hello</span>
          |  </div>
          | </body>
          |</html>""".stripMargin

      val expectedInner =
        """<!--s--><head></head>
          |<body>
          | <div>
          |  <span>hello</span>
          | </div>
          |</body>""".stripMargin

      "jsoup" - {
        var outerRuns = 0
        val scrubOuter = HtmlScrub { s =>
          outerRuns += 1
          s"<!--s-->$s"
        }
        val zOuter = DomZipperJsoup.parseHtml("<div><span>hello</span></div>")(scrubOuter)

        "outerHTML" - {
          val html = zOuter.outerHTML
          assertEq("outerHTML scrub invocation count", outerRuns, 1)
          assertEq("outerHTML output", html, expectedOuter)
        }

        var innerRuns = 0
        val scrubInner = HtmlScrub { s =>
          innerRuns += 1
          s"<!--s-->$s"
        }
        val zInner = DomZipperJsoup.parseHtml("<div><span>hello</span></div>")(scrubInner)

        "innerHTML" - {
          val html = zInner.innerHTML
          assertEq("innerHTML scrub invocation count", innerRuns, 1)
          assertEq("innerHTML output", html, expectedInner)
        }
      }

      "fastAndSlow" - {
        implicit val errorHandler: ErrorHandler[ErrorHandler.Id] = ErrorHandler.Throw

        var outerRuns = 0
        val scrubOuter = HtmlScrub { s =>
          outerRuns += 1
          s"<!--s-->$s"
        }
        val fastOuter = DomZipperJsoup.parseHtml("<div><span>hello</span></div>")(scrubOuter)
        val slowOuter = DomZipperJsoup.parseHtml("<div><span>hello</span></div>")(scrubOuter)
        val bothOuter = DomZippersFastAndSlow[ErrorHandler.Id, DomZipperJsoupF, JsoupElement, JsoupElement, DomZipperJsoupF, JsoupElement, JsoupElement](fastOuter, slowOuter)

        "outerHTML" - {
          val html = bothOuter.outerHTML
          assertEq("both.outerHTML scrub invocation count", outerRuns, 1)
          assertEq("both.outerHTML output", html, expectedOuter)
        }

        var innerRuns = 0
        val scrubInner = HtmlScrub { s =>
          innerRuns += 1
          s"<!--s-->$s"
        }
        val fastInner = DomZipperJsoup.parseHtml("<div><span>hello</span></div>")(scrubInner)
        val slowInner = DomZipperJsoup.parseHtml("<div><span>hello</span></div>")(scrubInner)
        val bothInner = DomZippersFastAndSlow[ErrorHandler.Id, DomZipperJsoupF, JsoupElement, JsoupElement, DomZipperJsoupF, JsoupElement, JsoupElement](fastInner, slowInner)

        "innerHTML" - {
          val html = bothInner.innerHTML
          assertEq("both.innerHTML scrub invocation count", innerRuns, 1)
          assertEq("both.innerHTML output", html, expectedInner)
        }
      }
    }

    "jsoupElement" - {
      final class ProbeElement extends org.jsoup.nodes.Element("span") {
        def hasAttrs: Boolean = hasAttributes()
      }

      "attrDoesNotAllocateAttributes" - {
        val raw = new ProbeElement
        val el = JsoupElement(raw)
        assert(!raw.hasAttrs)
        assertEq(el.attr("id"), None)
        assert(!raw.hasAttrs)
      }

      "attrCaseInsensitive" - {
        assertEq(name.dom.attr("VALUE"), Some("Bob Loblaw"))
        assertEq(name.dom.attr("nonexistent"), None)
      }

      "isSelectedDoesNotAllocateAttributes" - {
        val raw = new ProbeElement
        val el = JsoupElement(raw)
        assert(!raw.hasAttrs)
        assertEq(el.isSelected, false)
        assert(!raw.hasAttrs)
      }

      "isSelectedTrue" - {
        assertEq($("input[type=checkbox]", 1 of 2).dom.isSelected, true)
        assertEq($("input[type=checkbox]", 2 of 2).dom.isSelected, false)
      }

      "selectChildrenImmediateOnly" - {
        val form = $("form").dom
        assertEq(form.selectChildren("h3").map(_.innerText), Vector("HI"))
        assert(form.selectChildren("input").isEmpty)
        assertEq(form.selectChildren("div").size, 3)
      }

      "matchesDetached" - {
        val detached = JsoupElement(Jsoup.parse("<div class='a'><span id='s'></span></div>").selectFirst("span"))
        assert(detached.matches("span"))
        assert(detached.matches("div.a > span#s"))
        assert(!detached.matches("p"))
      }
    }

    "domCollection" - {
      def assertLeftContains(res: Either[String, Any], substr: String): Unit = {
        assert(res.isLeft)
        assertContains(res.left.getOrElse(""), substr)
      }

      "collect01HeadValidates" - {
        assertLeftContains($("form")("select", 2 of 2).failToEither.collect01("option").head, "Expected [0,1] results, not 2.")
      }

      "collect01LastValidates" - {
        assertLeftContains($("form")("select", 2 of 2).failToEither.collect01("option").last, "Expected [0,1] results, not 2.")
      }

      "collect01SingletonValidates" - {
        assertLeftContains($("form")("select", 2 of 2).failToEither.collect01("option").singleton, "Expected [0,1] results, not 2.")
      }

      "collect1nHeadValidates" - {
        assertLeftContains($("form").failToEither.collect1n("pre").head, "Expected [1,n] results, not 0.")
      }

      "collect1nLastValidates" - {
        assertLeftContains($("form").failToEither.collect1n("pre").last, "Expected [1,n] results, not 0.")
      }

      "collect1nSingletonValidates" - {
        assertLeftContains($("form").failToEither.collect1n("pre").singleton, "Expected [1,n] results, not 0.")
      }

      "collect01SingleSuccess" - {
        val col = $("form").failToEither.collect01("> h3")
        assertEq(col.head.map(_.innerText).toOption, Some("HI"))
        assertEq(col.last.map(_.innerText).toOption, Some("HI"))
        assertEq(col.singleton.map(_.innerText).toOption, Some("HI"))
      }

      "filterLazy" - {
        var calls = 0
        val col = $("form").collect0n("option").filter { _ => calls += 1; true }.filter(_.getAttribute("value").isDefined)
        assertEq(calls, 0)
        assertEq(col.size, 6)
        assertEq(calls, 6)
      }

      "mapAndTraverseDomsAndExtracts" - {
        val sel = $("form")("select", 2 of 2).failToEither
        val col = sel.collect1n("option")
        assertEq(col.mapDoms(_.attr("value")).toOption, Some(Vector(Some("a"), Some("b"))))
        assertEq(col.traverseDoms(d => Right(d.innerText)).toOption, Some(Vector("A", "B")))
        assertEq(col.mapExtracts(_.innerText).toOption, Some(Vector("A", "B")))
        assertEq(col.traverseExtracts(d => Right(d.innerText)).toOption, Some(Vector("A", "B")))
        assertLeftContains(sel.collect01("option").mapDoms(_.innerText), "Expected [0,1] results, not 2.")
      }

      "containerConversions" - {
        val sel = $("form")("select", 2 of 2).failToEither

        "collect01ToCollect0nFailsWhenInvalid" - {
          assertLeftContains(sel.collect01("option").collect0n.zippers, "Expected [0,1] results, not 2.")
        }

        "collect01ToCollect0nToCollect1nFailsWhenInvalid" - {
          assertLeftContains(sel.collect01("option").collect0n.collect1n.zippers, "Expected [0,1] results, not 2.")
        }

        "collect1nToCollect0nFailsWhenEmpty" - {
          assertLeftContains($("form").failToEither.collect1n("pre").collect0n.zippers, "Expected [1,n] results, not 0.")
        }

        "collect1nFilterToCollect01Valid" - {
          assertEq(sel.collect1n("option").filter(_.innerText == "B").collect01.zippers.map(_.map(_.innerText)).toOption, Some(Some("B")))
          assertEq(sel.collect1n("option").filter(_.innerText == "Z").collect01.zippers.map(_.map(_.innerText)).toOption, Some(None))
        }

        "collect0nToCollect1nValid" - {
          assertEq(sel.collect0n("option").collect1n.zippers.map(_.map(_.innerText)).toOption, Some(Vector("A", "B")))
        }
      }
    }

    "enrichErrOnFailures" - {
      "needAttribute" - {
        val Left(err) = $("form").failToEither.needAttribute("nonexistent")
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
        err
      }

      "parent" - {
        val Left(err) = $.failToEither.parent
        assertContains(err, "DESC:")
        assertContains(err, "HTML:")
        err
      }
    }

  }

}
