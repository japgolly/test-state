package teststate.selenium.util

import org.openqa.selenium._
import org.openqa.selenium.interactions.Actions
import scala.collection.JavaConverters._
import teststate.util.CssUtil

object SeleniumExt extends SeleniumExt

trait SeleniumExt {
  implicit def testStateExtWebDriver (d: WebDriver) : Internals.WebDriverExt  = new Internals.WebDriverExt(d)
  implicit def testStateExtWebElement(e: WebElement): Internals.WebElementExt = new Internals.WebElementExt(e)
  implicit def testStateExtPoint     (p: Point)     : Internals.PointExt      = new Internals.PointExt(p)

  final type JavaScriptNotSupported = Internals.JavaScriptNotSupported
  final val  JavaScriptNotSupported = Internals.JavaScriptNotSupported

  final val  ScrollLogic = Internals.ScrollLogic
  final type ScrollLogic = Internals.ScrollLogic

  implicit def testStateScrollLogic: ScrollLogic =
    ScrollLogic.simple
}

object Internals {
  private implicit def WebDriverExt(d: WebDriver): WebDriverExt = new WebDriverExt(d)

  class WebDriverExt(private val self: WebDriver) extends AnyVal {

    def onShutdownQuit(): Unit =
      Runtime.getRuntime().addShutdownHook(new Thread() {
        override def run(): Unit = self.quit()
      })

    def executeJsOrThrow(cmd: String, args: AnyRef*): AnyRef =
      self match {
        case j: JavascriptExecutor =>
          j.executeScript(cmd, args: _*)
        case _ =>
          throw JavaScriptNotSupported(cmd)
      }

    def unsetOnBeforeUnload(): Unit = {
      executeJsOrThrow("window.onbeforeunload = undefined")
      ()
    }

    def addStyleTag(content: String): Unit = {
      val js =
        """
          |var style = document.createElement('style');
          |style.type = 'text/css';
          |style.innerHTML = arguments[0];
          |document.getElementsByTagName('head')[0].appendChild(style);
        """.stripMargin.trim
      executeJsOrThrow(js, content)
      ()
    }

    def disableCssAnimation(disableAnimation  : Boolean = true,
                            disableTransitions: Boolean = true,
                            disableTransforms : Boolean = true): Unit = {
      val css = CssUtil.disableCssAnimation(
        disableTransitions = disableTransitions,
        disableTransforms  = disableTransforms,
        disableAnimation   = disableAnimation)
      addStyleTag(css)
    }
  }

  private val childrenXpath = By.xpath("./*")

  class WebElementExt(private val self: WebElement) extends AnyVal {
    def classes(): Set[String] = {
      val clsStr = self.getAttribute("class").trim
      if (clsStr.isEmpty)
        Set.empty
      else
        clsStr.split(" +").toSet
    }

    def children(): Vector[WebElement] =
      self.findElements(childrenXpath).asScala.toVector

    def getValue(): Option[String] =
      Option(self.getAttribute("value"))

    def setValueTo(value: String): Unit =
      if (value != self.getAttribute("value"))
        clearAndSendKeys(value)

    def clearAndSendKeys(keys: String): Unit = {
      self.clear()
      self.sendKeys(keys)
    }

    def dragAndDrop(to: WebElement)(implicit d: WebDriver): Unit =
      new Actions(d).dragAndDrop(self, to).build().perform()

    def dragAndDropBy(offsetX: Int, offsetY: Int)(implicit d: WebDriver): Unit =
      new Actions(d).dragAndDropBy(self, offsetX, offsetY).build().perform()

    def hoverMouseOver()(implicit d: WebDriver): Unit =
      new Actions(d).moveToElement(self).build().perform()

    def scrollTo()(implicit d: WebDriver, s: ScrollLogic): Unit = {
      import ScrollLogic._
      val loc         = self.getLocation
      val jsX: String = s.x(State(loc.x, Line.jsWindowScrollX))
      val jsY: String = s.y(State(loc.y, Line.jsWindowScrollY))
      d.executeJsOrThrow(s"window.scrollTo($jsX,$jsY)")
      ()
    }

    def scrollToX()(implicit d: WebDriver, s: ScrollLogic): Unit =
      scrollTo()(d, s.copy(y = ScrollLogic.Query.dontScroll))

    def scrollToY()(implicit d: WebDriver, s: ScrollLogic): Unit =
      scrollTo()(d, s.copy(x = ScrollLogic.Query.dontScroll))

    def scrollToAndClick()(implicit d: WebDriver, s: ScrollLogic): Unit = {
      scrollTo()(d, s)
      self.click()
    }

    def parent()(implicit d: WebDriver): WebElement =
      d.executeJsOrThrow("return arguments[0].parentNode", self).asInstanceOf[WebElement]
  }

  class PointExt(private val p: Point) extends AnyVal {
    def isPositive: Boolean =
      p.x >= 0 && p.y >= 0
  }

  final case class JavaScriptNotSupported(cmd: String) extends RuntimeException("Unable to execute: " + cmd)

  final case class ScrollLogic(x: ScrollLogic.Query, y: ScrollLogic.Query)
  object ScrollLogic {

    final case class Line[A](start: A, size: A, mid: A, end: A)

    final case class State(elementStart: Int, windowJS: Line[String])

    /** Result is a JavaScript expression that returns a number, which is the location to scroll to. */
    type Query = State => String

    object Line {
      def startAndSize(start: Int, size: Int): Line[Int] =
        Line(
          start = start,
          size  = size,
          mid   = start + size >> 1,
          end   = start + size)

      def startAndEnd(start: Int, end: Int): Line[Int] =
        startAndSize(
          start = start min end,
          size  = (start - end).abs)

      def startAndSizeJs(start: String, size: String): Line[String] =
        Line(
          start = start,
          size  = size,
          mid   = s"($start + $size/2)",
          end   = s"($start + $size)")

      val jsWindowScrollX: Line[String] =
        startAndSizeJs("window.scrollX", "window.innerWidth")

      val jsWindowScrollY: Line[String] =
        startAndSizeJs("window.scrollY", "window.innerHeight")
    }

    object Query {
      val simple    : Query = _.elementStart.toString
      val dontScroll: Query = _.windowJS.start
    }

    val simple: ScrollLogic =
      apply(Query.simple, Query.simple)
  }
}
