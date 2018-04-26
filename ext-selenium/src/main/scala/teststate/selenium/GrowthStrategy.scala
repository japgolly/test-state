package teststate.selenium

/** Strategy for spreading tabs across browsers */
trait GrowthStrategy {

  /** Determine where to open the next tab
    *
    * @param openTabsPerBrowser For each open browser, the number of tabs currently open in it.
    * @return The index of the target browser in which to open the next tab. Invalid index = new browser.
    */
  def nextBrowser(openTabsPerBrowser: IndexedSeq[Int]): Int
}

object GrowthStrategy {

  def apply(f: IndexedSeq[Int] => Int): GrowthStrategy =
    new GrowthStrategy {
      override def nextBrowser(openTabsPerBrowser: IndexedSeq[Int]): Int =
        f(openTabsPerBrowser)
    }

  def apply(desc: String, f: IndexedSeq[Int] => Int): GrowthStrategy =
    new GrowthStrategy {
      override def toString: String =
        desc
      override def nextBrowser(openTabsPerBrowser: IndexedSeq[Int]): Int =
        f(openTabsPerBrowser)
    }

  def singleBrowser: GrowthStrategy =
    apply("singleBrowser", _ => 0)

  /** Max out browsers first, then start adding tabs */
  def maxBrowsers(maxBrowsers: Int): GrowthStrategy =
    apply(s"maxBrowsers($maxBrowsers)", x =>
      if (x.length < maxBrowsers) {
        val i = x.indexWhere(_ == 0)
        if (i >= 0) i else x.length
      } else
        x.zipWithIndex.minBy(_._1)._2)

  /** Max out tabs first, then start adding browsers */
  def maxTabs(maxTabsPerBrowser: Int): GrowthStrategy =
    apply(s"maxTabs($maxTabsPerBrowser)", x => {
      val i = x.indexWhere(_ < maxTabsPerBrowser)
      if (i >= 0) i else x.length
    })
}