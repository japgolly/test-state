# Test Automation using Selenium


These instructions will help you get the most of Selenium testing.


* Never use `Thread.sleep`; instead use a retry policy.

* Don't expose `WebDriver` or `WebElement` instances from your observation.
  It's a general bad-practice and opens up the door to potential bugs like code
  outside of your observation looking up dynamic properties via the dom.
  It also makes it quite hard to actions to perform dom actions that need a
  WebDriver instance because the action function then needs to manually write
  `ref.tab.use(webDriver => ..._)` which is needless boilerplate.

  So in order to avoid both bugs and boilerplate separately, don't expose dom but
  provide methods for your actions to call instead.

  For example, instead of

  ```scala
  class Obs($: FastDomZipperSelenium) {
    val okButton: () => WebElement =
      $("button", 1 of 2).dom
  }

  object Dsl {
    val clickOk = dsl.action("Click OK")(_.obs.okButton.click())
  }
  ```

  do this:

  ```scala
  class Obs($: FastDomZipperSelenium) {
    val clickOk: () => Unit =
      $("button", 1 of 2).prepare(_.dom().click())
  }

  object Dsl {
    val clickOk = dsl.action("Click OK")(_.obs.clickOk())
  }
  ```

* Use `FastDomZipperSelenium` instead of `DomZipperSelenium` for speed-ups in the range of 5-50x.
  It works by combining two DomZippers -- a Selenium one, and a [Jsoup](https://jsoup.org) one --
  such that nearly all observations are performed in-memory, and dom lookups are verified in-memory
  during observation but only aquired on-demand as needed by actions.

* If you'd like to run multiple tests in parallel...
  * MultiBrowser
  * auto-shutdown
  * onNewTab -- tab init (CSS anim here?)
  * onNewDriverWithTempTab -- browser-setup
  * .withSeleniumTab(_.tab)

* It's a good idea to disable CSS animations, transformations and transitions.

* Use consistency checks

* Order your largest tests first
