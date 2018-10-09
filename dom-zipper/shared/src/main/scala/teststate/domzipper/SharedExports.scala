package teststate.domzipper

trait SharedExports {

  final implicit def toMofNOps(i: Int): MofN.IntExt =
    new MofN.IntExt(i)

  final type HtmlScrub = teststate.domzipper.HtmlScrub
  final val  HtmlScrub = teststate.domzipper.HtmlScrub

  implicit def htmlScrub: HtmlScrub =
    HtmlScrub.default

  final val DomZipper = teststate.domzipper.DomZipper

  final type DomZipper[F[_], A, Self[G[_], B] <: DomZipper[G, B, Self]] =
    teststate.domzipper.DomZipper[F, A, Self]
}
