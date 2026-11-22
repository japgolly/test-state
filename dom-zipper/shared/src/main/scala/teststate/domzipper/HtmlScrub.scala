package teststate.domzipper

/**
 * Arbitrary preprocessor that [[DomZipper]] applies before returning any HTML text.
 */
case class HtmlScrub(run: String => String) {
  def >>(that: HtmlScrub): HtmlScrub =
    if (this eq HtmlScrub.empty) that
    else if (that eq HtmlScrub.empty) this
    else HtmlScrub(run andThen that.run)

  def <<(that: HtmlScrub): HtmlScrub =
    that >> this
}

object HtmlScrub {

  def replaceAll(regex: String, replacement: String): HtmlScrub = {
    val r = regex.r
    HtmlScrub(r.replaceAllIn(_, replacement))
  }

  val empty: HtmlScrub =
    HtmlScrub(identity)

  val removeComments: HtmlScrub =
    replaceAll("<!--[^\u0000]*?-->", "")

  def default = removeComments

  val joinLines: HtmlScrub =
    replaceAll(">[\n\r\t ]*\n[\n\r\t ]*<", "><") >> replaceAll("[\n\r\t ]*\n[\n\r\t ]*", " ")
}