package teststate.domzipper.selenium

import org.openqa.selenium.{By, WebDriver, WebElement}
import scala.jdk.CollectionConverters._
import teststate.domzipper._
import teststate.selenium.util.SeleniumExt._
import DomZipper.{CssSelResult, DomCollection, Layer}
import ErrorHandler.{ErrorHandlerOptionOps, ErrorHandlerResultOps}

object DomZipperSeleniumF {

  // The Function0 part here means that DomZipperSelenium has the same types as FastDomZipperSelenium.
  // This means one can switch their types from one to the other without having to change usage.
  type Dom = () => WebElement

  type DomCollection[F[_], C[_], A] = DomZipper.DomCollection[DomZipperSeleniumF, F, C, Dom, A]

  type CssSelEngine = DomZipper.CssSelEngine[Dom, Dom]

  private implicit val cssSelSelenium: CssSelEngine =
    DomZipper.CssSelEngine((css, parent) => parent().findElements(By.cssSelector(css)).asScala.iterator.map(() => _).toVector)

  private val rootDomFn: DomZipperBase.Layers[Dom] => Dom =
    _.latest.dom

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {

    def apply(name: String, webElement: WebElement)(implicit scrub: HtmlScrub, driver: WebDriver): DomZipperSeleniumF[F, Dom] =
      new DomZipperSeleniumF(DomZipperBase.Layers init Layer(name, "", () => webElement), rootDomFn)

    def apply(webElement: WebElement)(implicit scrub: HtmlScrub, driver: WebDriver): DomZipperSeleniumF[F, Dom] =
      apply("<provided>", webElement)

    def apply(name: String, webElement: WebElement, driver: WebDriver)(implicit scrub: HtmlScrub): DomZipperSeleniumF[F, Dom] =
      apply(name, webElement)(scrub, driver)

    def apply(webElement: WebElement, driver: WebDriver)(implicit scrub: HtmlScrub): DomZipperSeleniumF[F, Dom] =
      apply(webElement)(scrub, driver)

    def tag(tag: String, driver: WebDriver)(implicit scrub: HtmlScrub): DomZipperSeleniumF[F, Dom] =
      apply(tag, driver.findElement(By.tagName(tag)))(scrub, driver)

    def html(driver: WebDriver)(implicit scrub: HtmlScrub): DomZipperSeleniumF[F, Dom] =
      tag("html", driver)

    def body(driver: WebDriver)(implicit scrub: HtmlScrub): DomZipperSeleniumF[F, Dom] =
      tag("body", driver)

    def failBy[G[_]: ErrorHandler]: Constructors[G]   = new Constructors
    def failToOption: Constructors[Option           ] = failBy(ErrorHandler.ReturnOption)
    def failToEither: Constructors[Either[String, ?]] = failBy(ErrorHandler.ReturnEither)

    type DomCollection [      C[_], A] = DomZipperSeleniumF.DomCollection[F, C, A]
    type DomCollectionF[G[_], C[_], A] = DomZipperSeleniumF.DomCollection[G, C, A]
  }
}

import DomZipperSeleniumF.Dom

final class DomZipperSeleniumF[F[_], A](override protected val layers: DomZipperBase.Layers[Dom],
                                        override protected val peek: DomZipperBase.Layers[Dom] => A
                                      )(implicit
                                        override protected val $: DomZipperSeleniumF.CssSelEngine,
                                        override protected[domzipper] val htmlScrub: HtmlScrub,
                                        override protected val F: ErrorHandler[F],
                                        driver: WebDriver
                                      ) extends DomZipperBase.WithStore[F, Dom, A, DomZipperSeleniumF] {

  override protected def newStore[B](pos: Pos, peek: Peek[B]): DomZipperSeleniumF[F, B] =
    new DomZipperSeleniumF(pos, peek)

  override def isCapable(c: DomZipper.Capability) = c match {
    case DomZipper.Capability.RadioButtonChecked => true
  }

  override def unmap =
    new DomZipperSeleniumF(layers, DomZipperSeleniumF.rootDomFn)

  override protected def self = this

  override protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]) =
    new DomZipperSeleniumF(layers, peek)($, h, g, driver)

  override protected def _parent: F[Dom] =
    F.attempt(() => dom().parent()(driver))

  override protected def _outerHTML: String =
    getAttribute("outerHTML").getOrElse("null")

  override protected def _innerHTML: String =
    getAttribute("innerHTML").getOrElse("null")

  private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]): DomCollection[DomZipperSeleniumF, F, C, Dom, A] =
    DomCollection[DomZipperSeleniumF, F, C, Dom, Dom, A](desc, enrichErr, result, C)(addLayer)

  override protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperSeleniumF, F, C, Dom, A] =
    newDomCollection(sel, runCssQuery(sel), C)

  override protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperSeleniumF, F, C, Dom, A] =
    newDomCollection(desc, dom().children().map(() => _), C)

  override protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]): DomCollection[DomZipperSeleniumF, F, C, Dom, A] = {
    // WebElement implements hashCode and equals sensibly
    val all: Set[WebElement] = runCssQuery(sel).iterator.map(_()).toSet
    val children = dom().children().iterator.filter(all.contains).map(() => _).toVector
    newDomCollection(desc, children, C)
  }

  override def matches(css: String): F[Boolean] = {
    val all = driver.findElements(By.cssSelector(css)).asScala
    val dom = this.dom()
    F pass all.contains(dom) // WebElement implements hashCode and equals sensibly
  }

  override def getAttribute(name: String): Option[String] =
    Option(dom().getAttribute(name))

  override def tagName: String =
    dom().getTagName()

  override def innerText: String =
    dom().getText()

  override def checked: F[Boolean] =
    F pass dom().isSelected()

  override def classes: Set[String] =
    dom().classes()

  override def value: F[String] =
    getAttribute("value") orFail s".value failed on <${dom().getTagName}>."
}
