package teststate.domzipper

trait SharedExports {

  final implicit def toMofNOps(i: Int): MofN.IntExt =
    new MofN.IntExt(i)

  final type HtmlScrub = teststate.domzipper.HtmlScrub
  final val  HtmlScrub = teststate.domzipper.HtmlScrub

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default

}
