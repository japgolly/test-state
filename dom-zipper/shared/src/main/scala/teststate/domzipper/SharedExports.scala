package teststate.domzipper

trait SharedExports extends teststate.util.Exports {

  final implicit def toMofNOps(i: Int): MofN.IntExt =
    new MofN.IntExt(i)

  final type HtmlScrub = teststate.domzipper.HtmlScrub
  final val  HtmlScrub = teststate.domzipper.HtmlScrub

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default

  final val DomZipper = teststate.domzipper.DomZipper

  final type DomZipper[F[_], Dom, A, Self[G[_], B] <: teststate.domzipper.DomZipper[G, Dom, B, Self]] =
    teststate.domzipper.DomZipper[F, Dom, A, Self]
}
