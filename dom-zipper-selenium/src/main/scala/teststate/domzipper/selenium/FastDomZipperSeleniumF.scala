package teststate.domzipper.selenium

import org.openqa.selenium.{WebDriver, WebElement}
import teststate.domzipper.jsoup.DomZipperJsoupF
import teststate.domzipper.jsoup.DomZipperJsoupF.{Dom => JDom}
import teststate.domzipper.{DomZippersFastAndSlow, ErrorHandler, HtmlScrub}

object FastDomZipperSeleniumF {

  type Dom = WebElement

  type FastDomZipperSeleniumF[F[_]] = DomZippersFastAndSlow.AtHome[F, Dom]

  def apply[F[_]: ErrorHandler](fast: DomZipperJsoupF[F, JDom], slow: DomZipperSeleniumF[F, () => Dom]): FastDomZipperSeleniumF[F] =
    DomZippersFastAndSlow[F, DomZipperJsoupF, JDom, JDom, DomZipperSeleniumF, () => Dom, Dom](fast, slow.map(_()))

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {
    private val DomZipperSelenium = new DomZipperSeleniumF.Constructors[F]()
    private val DomZipperJsoup = new DomZipperJsoupF.Constructors[F]()

    def html(driver: WebDriver)(implicit scrub: HtmlScrub): FastDomZipperSeleniumF[F] = {
      val s = DomZipperSelenium.html(driver)
      val f = DomZipperJsoup.parseHtml(s.outerHTML)
      FastDomZipperSeleniumF(f, s)
    }

    def body(driver: WebDriver)(implicit scrub: HtmlScrub): FastDomZipperSeleniumF[F] = {
      val s = DomZipperSelenium.body(driver)
      val f = DomZipperJsoup.parseBody(s.outerHTML)
      FastDomZipperSeleniumF(f, s)
    }

    def failBy[G[_]: ErrorHandler]: Constructors[G]   = new Constructors
    def failToOption: Constructors[Option           ] = failBy(ErrorHandler.ReturnOption)
    def failToEither: Constructors[Either[String, *]] = failBy(ErrorHandler.ReturnEither)

    type DomCollection [      C[_], A] = DomZippersFastAndSlow.DomCollection[F, C, Dom, A]
    type DomCollectionF[G[_], C[_], A] = DomZippersFastAndSlow.DomCollection[G, C, Dom, A]
  }
}
