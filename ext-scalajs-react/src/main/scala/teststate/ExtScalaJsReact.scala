package teststate

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import ExtScalaJsReact._

trait ExtScalaJsReact extends domzipper.Exports {

  final implicit def toReactExtHtmlScrubObject(a: HtmlScrub.type): ReactExtHtmlScrubObject =
    new ReactExtHtmlScrubObject(a)

  final implicit def toExtScalaJsReactCompExt(m: GenericComponent.MountedRaw): ExtScalaJsReactCompExt =
    new ExtScalaJsReactCompExt(m)

  implicit override val htmlScrub: HtmlScrub =
    HtmlScrub.default >> HtmlScrub.removeReactInternals
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ExtScalaJsReact extends ExtScalaJsReact {

  final class ReactExtHtmlScrubObject(private val self: HtmlScrub.type) extends AnyVal {
    @deprecated("Use .removeReactInternals", "2.1.2") def removeReactDataAttr: HtmlScrub = removeReactInternals

    def removeReactInternals: HtmlScrub =
      HtmlScrub(ReactTestUtils.removeReactInternals)
  }

  final class ExtScalaJsReactCompExt(private val m: GenericComponent.MountedRaw) extends AnyVal {
    def domZipper(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJs =
      DomZipperJs(m.displayName, ReactDOM.findDOMNode(m.raw).get.asElement)($, scrub)
  }
}

