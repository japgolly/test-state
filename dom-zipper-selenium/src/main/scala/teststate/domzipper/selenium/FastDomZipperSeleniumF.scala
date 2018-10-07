package teststate.domzipper.selenium

import org.jsoup.Jsoup
import org.openqa.selenium.WebDriver
import teststate.domzipper.{DomZipperPair, ErrorHandler, HtmlScrub}
import teststate.domzipper.jsoup.DomZipperJsoupF

object FastDomZipperSeleniumF {

  type Type[F[_]] = DomZipperPair[F, () => F[DomZipperSeleniumF.Dom]]

  def apply[F[_]: ErrorHandler](fast: DomZipperJsoupF[F, DomZipperJsoupF.Dom], slow: DomZipperSeleniumF[F, DomZipperSeleniumF.Dom]): Type[F] =
    DomZipperPair[F, DomZipperJsoupF, DomZipperJsoupF.Dom, DomZipperSeleniumF, DomZipperSeleniumF.Dom](fast, slow)

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {
    private val DomZipperSelenium = new DomZipperSeleniumF.Constructors[F]()
    private val DomZipperJsoup = new DomZipperJsoupF.Constructors[F]()

    def html(driver: WebDriver)(implicit scrub: HtmlScrub): Type[F] = {
      val s = DomZipperSelenium.html(driver)
      val f = DomZipperJsoup.body(Jsoup.parse(s.outerHTML))
      FastDomZipperSeleniumF(f, s)
    }

    def body(driver: WebDriver)(implicit scrub: HtmlScrub): Type[F] = {
      val s = DomZipperSelenium.html(driver)
      val f = DomZipperJsoup.body(Jsoup.parse(s.outerHTML))
      FastDomZipperSeleniumF(f, s)
    }
  }
}
