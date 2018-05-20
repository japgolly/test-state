package teststate.domzipper.selenium

import teststate.domzipper._
import org.openqa.selenium.{By, WebDriver, WebElement}
import scala.collection.JavaConverters._
import teststate.selenium.util.SeleniumExt._
import ErrorHandler._

object DomZipperSeleniumModule extends DomZipperModule {
  import AbstractCollector._

  override type Base = WebElement
  override type NextBase = WebElement

  final class Constructors[Out[_]](implicit h: ErrorHandler[Out]) {

    def root(tag: String, driver: WebDriver)(implicit scrub: HtmlScrub): DomZipper[WebElement, WebElement, Out] =
      apply(tag, driver.findElement(By.tagName(tag)))

    def html(driver: WebDriver)(implicit scrub: HtmlScrub): DomZipper[WebElement, WebElement, Out] =
      root("html", driver)

    def body(driver: WebDriver)(implicit scrub: HtmlScrub): DomZipper[WebElement, WebElement, Out] =
      root("body", driver)

    def apply[W <: Base](dom: W)(implicit scrub: HtmlScrub): DomZipper[W, WebElement, Out] =
      apply("<provided>", dom)

    def apply[W <: Base](name: String, webElement: W)(implicit scrub: HtmlScrub): DomZipper[W, WebElement, Out] =
      new DomZipper(Vector.empty, Layer(name, "", webElement), scrub)(h)
  }

  private val cssSelSelenium: CssSelEngine =
    CssSelEngine((css, parent) => parent.findElements(By.cssSelector(css)).asScala.toVector)

  // ===================================================================================================================

  /** DOM Zipper through Selenium.
    *
    * @param h The error handler.
    * @tparam Out The shape of all output that can potentially fail.
    */
  final class DomZipper[+Cur <: Base, Next <: NextBase, Out[_]] private[selenium](prevLayers: Vector[Layer[Base]],
                                                                                  curLayer: Layer[Cur],
                                                                                  htmlScrub: HtmlScrub)
                                                                                 (implicit h: ErrorHandler[Out])
    extends AbstractDomZipper[Cur, Next, Out](prevLayers, curLayer, htmlScrub)(cssSelSelenium, h) {

    override protected def setScrubHtml(f: HtmlScrub): DomZipper[Cur, Next, Out] =
      new DomZipper(prevLayers, curLayer, f)

    override def failBy[Result[_]](errorHandler: ErrorHandler[Result]): DomZipper[Cur, Next, Result] =
      new DomZipper(prevLayers, curLayer, htmlScrub)(errorHandler)

    override protected[domzipper] def addLayer[NewCur <: WebElement](nextLayer: Layer[NewCur]): DomZipper[NewCur, Next, Out] =
      new DomZipper(prevLayers :+ curLayer, nextLayer, htmlScrub)

    override protected def collect[C[_]](sel: String, c: Container[C, Out]): Collector[C, Next, Next, Out] =
      new Collector(this, sel, c, None)

    def dom: WebElement =
      curLayer.dom

    def getAttribute(name: String): Option[String] =
      Option(dom.getAttribute(name))

    def needAttribute(name: String): Out[String] =
      h.option(getAttribute(name), s"${dom.getTagName} doesn't have attribute $name")

    protected override def _outerHTML =
      getAttribute("outerHTML").fold("null")(htmlScrub.run)

    protected override def _innerHTML =
      getAttribute("innerHTML").fold("null")(htmlScrub.run)

    override def innerText: String =
      dom.getText()

    override def value: Out[String] =
      getAttribute("value") orFail s".value failed on <${dom.getTagName}>."

    override def checked: Out[Boolean] =
      dom.isSelected()

    override def classes: Set[String] =
      dom.classes()

    /** The currently selected option in a &lt;select&gt; dropdown. */
    def selectedOption: Out[Collector[Option, Next, Next, Out]] =
      dom.getTagName.toUpperCase match {
        case "SELECT" => collect01("option[selected]")
        case x        => h.fail(s"<$x> is not a <SELECT>")
      }

    /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
    def selectedOptionText: Out[Option[String]] =
      selectedOption.flatMap(_.mapDoms(_.getText))

    def findSelfOrChildWithAttribute[DD >: Cur <: Base](attr: String)(implicit ev: DomZipper[Next, Next, Out] <:< DomZipper[DD, Next, Out]): Out[Option[DomZipper[DD, Next, Out]]] =
      getAttribute(attr) match {
        case None => collect01(s"*[$attr]").zippers.map(_.map(ev))
        case Some(_) => Some(this)
      }
  }

  // ===================================================================================================================

  final class Collector[C[_], D <: Next, Next <: NextBase, Out[_]](from: DomZipper[_, Next, Out],
                                                                   sel: String,
                                                                   cont: Container[C, Out],
                                                                   colFilter: Option[NextBase => Boolean])
                                                                  (implicit h: ErrorHandler[Out])
      extends AbstractCollector[C, D, Next, Out](from, sel, cont, colFilter) {

    override protected def withFilter(colFilter: Option[NextBase => Boolean]): Collector[C, D, Next, Out] =
      new Collector(from, sel, cont, colFilter)
  }
}
