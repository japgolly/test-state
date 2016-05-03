package teststate

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import TestStateReact._
import teststate.domzipper.DomZipper.CssSelEngine

trait TestStateReact extends domzipper.Exports {

  implicit def toReactExtHtmlScrubObject(a: HtmlScrub.type): ReactExtHtmlScrubObject =
    new ReactExtHtmlScrubObject(a)

  implicit def toTestStateReactCompExt[N <: TopNode](c: CompScope.Mounted[N]): TestStateReactCompExt[N] =
    new TestStateReactCompExt(c)

  // TODO Separate Defaults ↓ from Exports ↑

  implicit override def defaultHtmlScrub: HtmlScrub =
    super.defaultHtmlScrub >> HtmlScrub.removeReactDataAttr
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object TestStateReact extends TestStateReact {

  final class ReactExtHtmlScrubObject(private val self: HtmlScrub.type) extends AnyVal {
    def removeReactDataAttr: HtmlScrub =
      HtmlScrub(ReactTestUtils.removeReactDataAttr)
  }

  final class TestStateReactCompExt[N <: TopNode](private val c: CompScope.Mounted[N]) extends AnyVal {
    def domZipper(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperAt[N] =
      DomZipper(c.displayName, c.getDOMNode())($, scrub)

    def htmlDomZipper(implicit $: CssSelEngine, scrub: HtmlScrub): HtmlDomZipper =
      domZipper($, scrub).asHtml
  }
}

