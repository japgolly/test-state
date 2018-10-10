package teststate.domzipper.selenium

import org.jsoup.Jsoup
import org.openqa.selenium.WebDriver
import teststate.domzipper.{DomZipperPair, ErrorHandler, HtmlScrub}
import teststate.domzipper.jsoup.DomZipperJsoupF
import DomZipperJsoupF.{Dom => JDom}
import DomZipperSeleniumF.Dom

object FastDomZipperSeleniumF {

  type Type[F[_]] = DomZipperPair.Home[F, Dom]

  def apply[F[_]: ErrorHandler](fast: DomZipperJsoupF[F, JDom], slow: DomZipperSeleniumF[F, Dom]): Type[F] =
    DomZipperPair[F, DomZipperJsoupF, JDom, DomZipperSeleniumF, Dom](fast, slow)

  final class Constructors[F[_]](implicit F: ErrorHandler[F]) {
    private val DomZipperSelenium = new DomZipperSeleniumF.Constructors[F]()
    private val DomZipperJsoup = new DomZipperJsoupF.Constructors[F]()

    def html(driver: WebDriver)(implicit scrub: HtmlScrub): Type[F] = {
      val s = DomZipperSelenium.html(driver)
      val f = DomZipperJsoup.body(Jsoup.parse(s.outerHTML))
      FastDomZipperSeleniumF(f, s)
    }

    def body(driver: WebDriver)(implicit scrub: HtmlScrub): Type[F] = {
      val s = DomZipperSelenium.body(driver)
      val f = DomZipperJsoup.body(Jsoup.parseBodyFragment(s.outerHTML))
      FastDomZipperSeleniumF(f, s)
    }
  }
}
