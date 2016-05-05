package teststate

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import teststate.domzipper.DomZipper.CssSelEngine
import ExtScalaJsReact._

trait ExtScalaJsReact extends domzipper.Exports {

  implicit def toReactExtHtmlScrubObject(a: HtmlScrub.type): ReactExtHtmlScrubObject =
    new ReactExtHtmlScrubObject(a)

  implicit def toExtScalaJsReactCompExt[N <: TopNode](c: CompScope.Mounted[N]): ExtScalaJsReactCompExt[N] =
    new ExtScalaJsReactCompExt(c)

  implicit override val testStateHtmlScrub: HtmlScrub =
    HtmlScrub.default >> HtmlScrub.removeReactDataAttr
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ExtScalaJsReact extends ExtScalaJsReact {

  final class ReactExtHtmlScrubObject(private val self: HtmlScrub.type) extends AnyVal {
    def removeReactDataAttr: HtmlScrub =
      HtmlScrub(ReactTestUtils.removeReactDataAttr)
  }

  final class ExtScalaJsReactCompExt[N <: TopNode](private val c: CompScope.Mounted[N]) extends AnyVal {
    def domZipper(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperAt[N] =
      DomZipper(c.displayName, c.getDOMNode())($, scrub)

    def htmlDomZipper(implicit $: CssSelEngine, scrub: HtmlScrub): HtmlDomZipper =
      domZipper($, scrub).asHtml
  }
}

