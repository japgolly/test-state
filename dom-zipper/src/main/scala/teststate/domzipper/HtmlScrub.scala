package teststate.domzipper

/**
 * Arbitrary preprocessor that [[DomZipper]] applies before returning any HTML text.
 */
case class HtmlScrub(run: String => String) {
  def >>(that: HtmlScrub): HtmlScrub =
    HtmlScrub(run andThen that.run)

  def <<(that: HtmlScrub): HtmlScrub =
    that >> this
}

object HtmlScrub {

  def replaceAll(regex: String, replacement: String): HtmlScrub = {
    val r = regex.r
    HtmlScrub(r.replaceAllIn(_, replacement))
  }

  def empty: HtmlScrub =
    HtmlScrub(identity)

  val removeComments: HtmlScrub =
    replaceAll("<!--[^\u0000]*?-->", "")

  def default = removeComments
}