package teststate.domzipper

trait SharedExports {

  implicit final def toMofNOps(i: Int): MofN.IntExt =
    new MofN.IntExt(i)

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default

  type HtmlScrub = teststate.domzipper.HtmlScrub
  val  HtmlScrub = teststate.domzipper.HtmlScrub

}
