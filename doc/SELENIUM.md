# Test Automation using Selenium


These instructions will help you get the most of test-state and Selenium.


* Never use `Thread.sleep`. Instead use a retry policy and just write your observations and actions
  as if `Thread.sleep` were already in all of the right places.

  ```scala
  // Example of how to set a retry policy
  plan
    .test(observer)
    .stateless
    .withRef(driver)
    .withRetryPolicy(Retry.Policy.fixedIntervalWithTimeout(200 millis, 60 seconds)) // **** HERE ****
    .run()
  ```

* Don't expose `WebDriver` or `WebElement` instances from your observation.
  It's a general bad-practice and opens up the door to potential bugs like code
  outside of your observation looking up dynamic properties via the dom
  (which violates a rule that observations are complete and immutable snapshots).
  It also makes it quite hard to write actions that need a `WebDriver` instance because
  the action function then needs to manually write `ref.tab.use(webDriver => ..._)`
  all over the place which is needless boilerplate.

  So in order to avoid both bugs and boilerplate, don't expose `WebDriver`/`WebElement`s;
  instead provide methods for your actions to call.

  For example, instead of

  ```scala
  // DON'T DO THIS

  class Obs($: FastDomZipperSelenium) {
    val okButton: WebElement =
      $("button", 1 of 2).dom()
  }

  object Dsl {
    val clickOk = dsl.action("Click OK")(_.obs.okButton.click())
  }
  ```

  do this:

  ```scala
  // THIS IS GOOD

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
  during observation but only acquired on-demand as needed by actions.

* It's a good idea to disable CSS animations, transformations and transitions.
  There is an extension method on `WebDriver` called `.disableCssAnimation()` to do this.
  To use it, create an action that calls it and integrate it early in to your test plans.

  ```scala
  case class Ref(tab: Tab[WebDriver])

  val action = Dsl.action("Disable CSS animations")(_.ref.tab.use(_.disableCssAnimation()))
  ```

* Use consistency checks.

  Consistency checks ensure that the page hasn't changed since the check was created.
  This is useful to ensure that both the page doesn't change mid-observation,
  and the page hasn't changed by the time the next action is scheduled.

  ```scala
  class Obs($: FastDomZipperSelenium) {
    // Before making any observations...
    val checkConsistency = $.startConsistencyCheck()

    // ... obs here ...

    // After making all observations, ensure the page didn't change mid-observation
    checkConsistency()
  }

  val dsl = Dsl[Ref, Obs, Unit]
    .withPreActionAssertion(_.obs.checkConsistency()) // Ensure the page hasn't changed by action time
    .withSeleniumTab(_.tab) // Ensure this comes last
  ```

* Order your largest tests first.

  If you're running lots of tests in parallel,
  you can reduce the total test time by scheduling the largest tests to run first.

  It's quite easy to programmatically approximate test size: simply call `.nameTree.deepSize` on your plan.

* To run multiple tests in parallel...

  test-state provides support for Selenium parallelism and concurrency.
  The steps to follow are below.
  (Also see `SeleniumExample2.scala` for an example.)

  * Create an instance of `MultiBrowser`
  * Either ensure you call one of the following on your `MultiBrowser` instance:
    * `.close()`
    * `.closeRoot()`
    * `.onShutdownClose()`
    * `.onShutdownCloseRoot()`
  * If you need to perform per-browser setup, use `.onNewDriverWithTempTab` on your `MultiBrowser` instance
  * If you need to perform per-tab setup, use `.onNewTab` on your `MultiBrowser` instance
  * Add `.withSeleniumTab(_.tab)` to the end of your `Dsl` creation
