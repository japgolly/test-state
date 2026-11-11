# Bug and Defect Report for `test-state`

This report details bugs, defects, and problems identified across the `test-state` codebase, ordered roughly by severity.

---

## 5. `RunCallbacks.aroundAction` completion callback is skipped when an action throws an exception (`Runner.scala`)

* **Location:** `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 508–520)
* **Code:**
  ```scala
  val act1a: () => F[Or[Failure[E], S]] =
    () => action.run(ros)
  val act1: () => F[Or[Failure[E], S]] =
    callbacks.fold(act1a)(callbacks =>
      () => EM.flatMap(callbacks.get.aroundAction(ros, name))(complete =>
        EM.flatMap(act1a())(result =>
          EM.map(complete(result.leftOption))(_ => result)
        )
      )
    )
  val act2 = wrapStack(ros, name)(act1)
  def act3() = EM.recover(act2())
  ```
* **Problem:**
  `EM.recover` wraps `act2()` (outside `act1`), whereas the `aroundAction` completion callback `complete(result.leftOption)` is chained inside `act1` via `EM.flatMap(act1a())(...)`.
  Most actions (such as those created via `dsl.action(name)(ros => ...)`) signal failure by throwing an exception, which `EM.recover` catches at `act3()`. When `act1a()` throws synchronously or returns a failed `F` (e.g. failed `Future`), `EM.flatMap(act1a())` short-circuits over `complete(...)` before `EM.recover` converts the exception into `Left(Failure(...))`.
* **Impact:**
  `RunCallbacks.aroundActions` (`after` and `timed` callbacks) are never invoked when an action fails by throwing an exception.

---

## 6. `startConsistencyCheck()` and `ensureConsistency()` are a no-op on `FastDomZipperSelenium` (`DomZipper.scala`, `DomZippersFastAndSlow.scala`)

* **Location:**
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipper.scala` (lines 53–71)
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZippersFastAndSlow.scala` (line 83)
  * `doc/SELENIUM.md` (lines 76–96) and `example-selenium/src/test/scala/teststate/example/selenium/SeleniumExample2.scala` (lines 17–19)
* **Code:**
  ```scala
  // DomZipper.scala
  final def startConsistencyCheck(): ConsistencyCheck = {
    val initial = outerHTML
    F.map(initial)(i => () => {
      val current = outerHTML
      F.flatMap(current)(c =>
        if (c == i) F.pass else F.fail("DOM changed during observation"))
    })
  }

  // DomZippersFastAndSlow.scala
  override protected def _outerHTML: F[String] = fast.outerHTML
  ```
* **Problem:**
  `doc/SELENIUM.md` and `SeleniumExample2.scala` specifically instruct users to call `.startConsistencyCheck()` / `.ensureConsistency(...)` on `FastDomZipperSelenium` so that `test-state` can detect if the browser DOM changes mid-observation and retry.
  However, `FastDomZipperSelenium` is a `DomZippersFastAndSlow` instance whose `_outerHTML` delegates to `fast.outerHTML`. `fast` is a `DomZipperJsoup` parsed once into an immutable in-memory Jsoup DOM when `FastDomZipperSelenium` was instantiated.
* **Impact:**
  Calling `startConsistencyCheck()` and later invoking the returned `ConsistencyCheck` (or using `ensureConsistency`) on `FastDomZipperSelenium` simply compares the static in-memory Jsoup `outerHTML` against itself (`c == i` is always `true`) without ever querying `slow().outerHTML` on the live Selenium `WebDriver`. It never detects DOM changes during observation.

---

## 7. Mismatched root elements in `FastDomZipperSelenium.html` and `DomZipperJsoup.body` (`FastDomZipperSeleniumF.scala`, `DomZipperJsoupF.scala`)

* **Location:**
  * `dom-zipper-selenium/src/main/scala/teststate/domzipper/selenium/FastDomZipperSeleniumF.scala` (lines 21–25)
  * `dom-zipper-jsoup/src/main/scala/teststate/domzipper/jsoup/DomZipperJsoupF.scala` (lines 29–36)
* **Code:**
  ```scala
  // FastDomZipperSeleniumF.scala
  final def html(driver: WebDriver)(implicit errorHandler: ErrorHandler[F]): FastDomZipperSelenium = {
    val s = DomZipperSelenium.html(driver)
    val f = DomZipperJsoup.parseHtml(s.outerHTML)
     new FastDomZipperSeleniumF( Nil, f, () => s)(errorHandler, htmlScrub)
  }

  // DomZipperJsoupF.scala
  final def body(body: Document)(implicit errorHandler: ErrorHandler[F]): DomZipperJsoup =
    apply("body", body)

  final def parseHtml(html: String)(implicit errorHandler: ErrorHandler[F]): DomZipperJsoup =
    this.html(Jsoup.parse(html, ""))
  ```
* **Problem:**
  1. In `DomZipperJsoupF.Constructors.body(body: Document)` (line 33), `apply("body", body)` wraps the `org.jsoup.nodes.Document` node (`#root`) directly instead of `body.body()` (whereas `parseBody` on line 40 calls `.selectFirst("body")`).
  2. In `DomZipperJsoupF.Constructors.parseHtml`, `Jsoup.parse(html, "")` returns a Jsoup `Document` whose root node has `tagName == "#root"` and whose child is the `<html>` element.
     Meanwhile, in `FastDomZipperSeleniumF.Constructors.html(driver)`, `s = DomZipperSelenium.html(driver)` is positioned at the `<html>` element (`driver.findElement(By.tagName("html"))`), whereas `f = DomZipperJsoup.parseHtml(s.outerHTML)` is positioned at the Jsoup `Document` (`#root`) *above* `<html>`.
* **Impact:**
  In `FastDomZipperSelenium.html(driver)`, `fast` and `slow` point to different levels of the DOM hierarchy:
  * `$.tagName` returns `"#root"` instead of `"html"`.
  * `$.children0n` on `fast` returns `Vector(<html>)`, whereas on `slow` it returns `Vector(<head>, <body>)`.
  * Direct child queries like `$.child("body")` or `$("> body")` fail on `FastDomZipperSelenium.html(driver)` while succeeding on `DomZipperSelenium.html(driver)`.

---

## 8. Bitwise shift operator precedence bug in `ScrollLogic.Line.startAndSize` (`SeleniumExt.scala`)

* **Location:** `util-selenium/src/main/scala/teststate/util/SeleniumExt.scala` (lines 148–153)
* **Code:**
  ```scala
  def startAndSize(start: Int, size: Int): Line =
    new Line(
      start = start,
      size  = size,
      mid   = start + size >> 1,
      end   = start + size - 1)
  ```
* **Problem:**
  In Scala, `+` has higher precedence than `>>`. Therefore, `start + size >> 1` parses as `(start + size) >> 1` instead of `start + (size >> 1)` (compare with `startAndSizeJs` on line 164: `s"($start + $size/2)"`).
* **Impact:**
  For any non-zero `start`, `mid` is miscalculated (e.g. for `start = 100, size = 50`, `mid` is calculated as `(100 + 50) >> 1 = 75`, which is outside `[100, 149]`, instead of `125`). This corrupts `ScrollLogic.ShowMiddle` and `ScrollLogic.Minimal` scroll offset calculations whenever `Line.startAndSize` is used.

---

## 9. Copy-paste bug in `TestStateNFEBA.thruAfter` and broken `NamedOps` for `Invariant.Delta`, `NamedError`, and `Sack.CoProduct` (`core/package.scala`, `checks.scala`, `NamedOps.scala`)

* **Location:**
  * `core/shared/src/main/scala/teststate/core/package.scala` (lines 13–19)
  * `core/shared/src/main/scala/teststate/core/checks.scala` (lines 137–143)
  * `core/shared/src/main/scala/teststate/core/NamedOps.scala` (lines 48–64)
* **Code:**
  ```scala
  // core/package.scala
  implicit final class TestStateNFEBA[A](private val f: NameFn[BeforeAfter[A]] => NameFn[BeforeAfter[A]]) extends AnyVal {
    def thruBefore: NameFn[A] => NameFn[A] =
      n => f(n.cmap(_.before)).cmap(BeforeAfter.same)

    def thruAfter: NameFn[A] => NameFn[A] =
      n => f(n.cmap(_.before)).cmap(BeforeAfter.same)
  }

  // checks.scala
  implicit def invariantInstanceNamedOps[O, S, E]: NamedOps[Invariant[O, S, E], OS[O, S]] =
    NamedOps((s, f) => s match {
      case Point(x) => Point(x renameBy f)
      case Delta(x) => Delta(x renameBy (n => f(n.cmap(BeforeAfter.same)).cmap(_.before)))
    })
  ```
* **Problem:**
  1. `TestStateNFEBA.thruAfter` is an exact copy-paste of `thruBefore` — it uses `_.before` instead of `_.after`.
  2. In `invariantInstanceNamedOps`, renaming an `Invariant.Delta` transforms `NameFn[BeforeAfter[OS]]` via `n => f(n.cmap(BeforeAfter.same)).cmap(_.before)`. When the renamed `NameFn` is later invoked with `Some(BeforeAfter(before, after))`, `.cmap(_.before)` extracts `before` and `.cmap(BeforeAfter.same)` turns it into `BeforeAfter(before, before)`, completely discarding the `after` state when formatting the invariant's name.
  3. In `NamedOps.scala`:
     * `namedErrorOrXInstanceNamedOps` (line 62) matches `case l @ Left(_) => l` without renaming the `Name` inside `NamedError(name, err)`.
     * `sackInstanceNamedOps` (line 51) and `sackInstanceNamedOpsBA` (line 58) match `case CoProduct(n, p) => CoProduct(f(n), p)` without propagating the rename into the inner `Sack` returned by `p(b)`.

---

## 10. `DomZipperBase.child(name, sel, which)` ignores `name` on success and produces wrong error on 0 matches (`DomZipperBase.scala`)

* **Location:** `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipperBase.scala` (lines 61–73)
* **Code:**
  ```scala
  final override def child[ sel: CssSel](name: String, sel: sel, which: MofN): Next = {
    val results = if (sel.css.isEmpty) children1n else children1n(sel)
    F.flatMap(results.zippers) { zippers =>
      def q = if (name.isEmpty) "Q" else s"$name q"
      def desc = if (sel.css.isEmpty) ">*" else sel.css
      if (zippers.isEmpty)
        F.fail(s"${q}uery failed: [$desc]. Expected ${which.n} results, not 0.")
      else if (zippers.length != which.n)
        F.fail(s"${q}uery failed: [$desc]. Expected ${which.n} results, not ${zippers.length}.")
      else
        F.point(zippers(which.m - 1))
    }
  }
  ```
* **Problem:**
  1. Line 62 calls `children1n` / `children1n(sel)` instead of `children0n` / `children0n(sel)`. When 0 child elements match, `results.zippers` fails inside `Container1N` with `"Query failed: [>*]. Expected [1,n] results, not 0."` before reaching line 65. Consequently, the `if (zippers.isEmpty)` branch on lines 65–66 is dead code, and both the custom `name` and expected count `which.n` are omitted from the failure message.
  2. When `zippers.length == which.n` succeeds (line 71), it returns `zippers(which.m - 1)` whose `Layer` was created by `children1n` with `name = "collect"`, discarding the `name` parameter passed to `child(name, sel, which)`.

---

## 11. `DomZippersFastAndSlow.cmap` evaluates `colS.zippers` on the unfiltered collection, breaking `.filter` on `collect01` / `children01` (`DomZippersFastAndSlow.scala`)

* **Location:** `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZippersFastAndSlow.scala` (lines 45–61)
* **Code:**
  ```scala
  private def cmap[C[x] <: DomCollection.Container[F, x], A](f: Fast => DomCollection[F, C, NextF],
                                                             s: Slow => DomCollection[F, C, NextS]): DomCollection[F, C, Next] = {
    val colF = f(fast)
    import colF.C
    lazy val colS = F.map(slow())(s)
    val rawResults: F[Vector[Next]] =
      F.map(colF.rawResults)(_.iterator.zipWithIndex.map { case (f, i) =>
        lazy val s = F.flatMap(colS)(_.zippers).map(C.get(_, i))
        addLayer(f.curLayer, f, () => s)
      }.toVector)
    new DomCollection(colF.layerDesc, rawResults)(F, C)
  }
  ```
* **Problem:**
  `colF.rawResults` is the unfiltered `Vector[NextF]` before the container constraint `C` (e.g., `Container01` for `[0,1]`) or `.filter` is applied.
  However, for each element at raw index `i`, `lazy val s` calls `F.flatMap(colS)(_.zippers).map(C.get(_, i))` instead of indexing into `colS.rawResults(i)`.
  If a user runs `$.collect01("button").filter(_.innerText == "Submit")` on a page with 3 buttons (only 1 of which is `"Submit"`), `fast` filters `rawResults` down to 1 element and `Container01` succeeds. However, as soon as `.dom()` is called on that element, `s` forces `colS.zippers` on the **unfiltered** `colS` (`Container01` with 3 elements), which throws `"Expected [0,1] results, not 3."` (or for `i >= 1`, `C.get( Option, i )` ignores `i` because `Container01.get` is `(c, _) => c.get`).

---

## 12. `DomZipperJsF.value` throws `js.TypeError` when `.value` is `undefined` (`DomZipperJsF.scala`)

* **Location:** `dom-zipper/js/src/main/scala/teststate/domzipper/DomZipperJsF.scala` (lines 120–131)
* **Code:**
  ```scala
  private def dynamicMethod[B](f: js.Dynamic => Any): Option[B] =
     f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[B]].toOption

  override protected def _value: F[String] =
    dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."
  ```
* **Problem:**
  Inside `dynamicMethod`, `_.value.toString` calls `.toString` on `_.value` *before* `dynamicMethod` wraps the result in `js.UndefOr`. If `dom` does not have a `value` property (e.g. a `<div>` or `<span>`), `_.value` is `js.undefined`, and calling `.toString` on `undefined` immediately throws a JavaScript `TypeError` instead of returning `None` and failing cleanly via `ErrorHandler[F]` (`orFail`).

---

## 13. Dynamic DOM state (`value`, checkbox `checked`, `selectedOption`) reads static HTML attributes instead of live DOM properties (`DomZippersFastAndSlow.scala`, `DomZipper.scala`)

* **Location:**
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZippersFastAndSlow.scala` (lines 92–101)
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipper.scala` (lines 189–193)
* **Code:**
  ```scala
  // DomZippersFastAndSlow.scala
  override def value: F[String] = fast.value

  override def checked: F[Boolean] =
    F.flatMap(fast.tagName.map(_ == "input") && fast.attr("type").map(_.contains("radio"))) {
      case true  => F.flatMap(slow())(_.checked)
      case false => fast.checked
    }

  // DomZipper.scala
  final def selectedOption[Out[_]](implicit C: Container[Option, Out]): Out[Self] =
    collect("option[selected]")(C)
  ```
* **Problem:**
  In browsers, user interactions (such as typing into an `<input>` or `<textarea>`, clicking an `<input type="checkbox">`, or choosing an `<option>` in a `<select>`) update the live DOM properties (`.value`, `.checked`, `.selected`), **not** the initial HTML attributes serialized in `outerHTML`.
  1. `DomZippersFastAndSlow.checked` delegates to `slow().checked` only for `input[type=radio]`, not for `input[type=checkbox]`. Checking/unchecking a checkbox via Selenium does not add/remove the `checked` attribute in `outerHTML`, so `FastDomZipperSelenium.checked` returns stale results for checkboxes (which is why checkbox assertions had to be commented out in `FastDomZipperSeleniumTest.scala` lines 62–66).
  2. `DomZippersFastAndSlow.value` always delegates to `fast.value` (Jsoup's `val()`), which only sees the static `value="..."` HTML attribute from `outerHTML` and misses any text typed into `<input>` or `<textarea>` via `sendKeys`.
  3. `DomZipper.selectedOption` queries `collect("option[selected]")`, which matches the static `[selected]` HTML attribute rather than the live `selected` DOM property (which is why the test in `DomZipperJsTest.scala` line 85 had to be commented out).

---

## 14. `ExecutionModel.scalaFuture` leaks `java.util.Timer` threads and hangs `Promise`s on synchronous exceptions (`ExecutionModel.scala`)

* **Location:** `core/shared/src/main/scala/teststate/typeclass/ExecutionModel.scala` (lines 90–143)
* **Code:**
  ```scala
  implicit def scalaFuture(implicit ec: ExecutionContext): ExecutionModel[Future] =
    new ExecutionModel[Future] {
      private val timer = new Timer(true)

      override def schedule[A](task: => Future[A], delay: FiniteDuration): Future[A] = {
        val promise = Promise[A]()
        timer.schedule(new TimerTask {
          override def run(): Unit = {
            ec.execute(new Runnable {
              override def run(): Unit =
                promise.completeWith(task)
            })
          }
        }, delay.toMillis)
        promise.future
      }

      override def doFinally[A, B](main: => Future[A], last: => Future[B]): Future[A] = {
        val p = Promise[A]()
        val fa = main
        fa.onComplete { a =>
          val fb = last
          fb.onComplete {
            case t: util.Failure[_] => p.complete(t.asInstanceOf[util.Failure[A]])
            case _: util.Success[_] => p.complete(a)
          }
        }
        p.future
      }
  ```
* **Problem:**
  1. **Thread leak:** `implicit def scalaFuture` creates a new `java.util.Timer(true)` instance every time the implicit is resolved. Each `new Timer(true)` spawns a new background `TimerThread` OS thread that is never cancelled.
  2. **Uncompleted `Promise` in `schedule`:** In `promise.completeWith(task)` (line 115), `task` is evaluated strictly before `completeWith` is called. If evaluating `task` throws a synchronous exception, the `Runnable` dies uncaught and `promise.future` hangs forever.
  3. **Uncompleted `Promise` and skipped `last` in `doFinally`:** In `doFinally` (lines 126–129), `val fa = main` is evaluated outside a `try`/`catch` (if `main` throws synchronously, `last` never runs), and `val fb = last` inside `fa.onComplete` is also evaluated outside a `try`/`catch` (if `last` throws synchronously, `p` is never completed and `p.future` hangs forever).

---

## 15. Check execution, categorization, and retry bugs in `Runner` (`Runner.scala`)

* **Location:** `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 383–478, 434–443, 491–547, 645–670)
* **Problems:**
  1. **Delta invariants reported under `"Post-conditions"` instead of `"Invariants"` (lines 395, 404, 464):**
     `halfChecksI` (created from `checksPre.invariants.deltas`) is combined with `checksPre.pre.deltas` into `halfChecks` (line 404). In `runPostChecks` (line 464), all `halfChecks` are added to `post` (`b.group(PostName)` = `"Post-conditions"`), while `invs` (`b.group(InvariantsName)` = `"Invariants"`) only receives point invariants (`checksPostI.afters`). Thus, all delta/`Around` invariants appear under `"Post-conditions"` in test reports instead of `"Invariants"`.
  2. **Pre-action `Around` check `before` functions (`checksPre.pre.deltas`) are evaluated *after* the action (line 404):**
     While `halfChecksI = runHalfChecks(checksPre.invariants.deltas, Vector.empty)` is evaluated *before* the action runs (line 395), `val halfChecks = runHalfChecks(checksPre.pre.deltas, halfChecksI)` is placed inside `val postChecksFn = (ros: ROS[R, O, S]) => { ... }` (line 404), which only executes *after* the action has completed (and re-runs on every post-check retry). Although `runHalfChecks` passes `p.ros` (the pre-action `ROS`), if `p.ros.obs` or `p.ros.ref` accesses live or lazy state (such as `DomZipperSelenium`), `d.before(p.ros)` observes post-action state instead of pre-action state.
     *(A related issue exists in `Dsl.scala` line 272 in `FocusValue.AssertOps.changeTo`, where `NameFn(NameUtils.equalFn(focusName, positive, i => expect(focusFn(i.before))))` re-evaluates `focusFn(i.before)` after the action when generating the assertion name, as noted in `DslTest.scala` line 48.)*
  3. **Pre-check retry in `Action.Group` and `Action.SubTest` discards the re-observed `ROS` (lines 434–443, 645–647, 666–670):**
     When `runPreChecksWithRetry` retries a failing pre-condition, `reObserve` produces a new `(r, o)` and checks `preChecks(ROS(r, o, p0.ros.state))`. However, `runPreChecksWithRetry` returns `F[Boolean]` and discards the newly observed `ROS(r, o, p0.ros.state)`. `Action.Group` and `Action.SubTest` then proceed to run `run()` using the stale, pre-retry `ros`.
  4. **`Action.Single` retries pre-condition failures under `Retry.Scope.Action` instead of `Retry.Scope.PreConditions` (line 546):**
     In `runActionWithRetry` (line 546), `retry(Retry.Scope.Action, attempt0, ...)` is used for the entire `tryAction` loop, which includes `preChecks`. If a pre-condition fails on an `Action.Single`, it is retried using `Retry.Scope.Action` instead of `Retry.Scope.PreConditions`. Furthermore, if `reObserve()` fails on retry, `onRetry` (lines 536–544) re-evaluates `initialPreCheck` (double-counting `stats.checks` and mutating the report builder a second time) and falls back to the first attempt's error rather than the latest attempt's error.
  5. **`Action.SubTest` post-conditions are not retried (line 664):**
     In `case Action.SubTest(sac, cond) =>` (line 664), `val postFailed = runPostChecks(ros2)._2` is called directly without `retryPostCheck`, unlike `Action.Single` (line 595) and `Action.Group` (line 637).

---

## 16. Concurrency, tab-counting, and resource-leak bugs in `MultiBrowser` (`MultiBrowser.scala`)

* **Location:** `ext-selenium/src/main/scala/teststate/selenium/MultiBrowser.scala` (lines 98–156, 200–212)
* **Code:**
  ```scala
  override def openTab(): Tab[D] = {
    outerMutex.lock()
    try {
      // ...
      onNewDriverWithTempTab match {
        case Some(setupFn) if isNewBrowser =>
          val setupTab = createTabWithoutLocking(browserIndex)
          val tab = createTabWithoutLocking(browserIndex)
          val browserLock = browsers(browserIndex).mutex
          browserLock.lock()
          outerMutex.unlock()
          try
            setupFn(setupTab)
          finally
            browserLock.unlock()
          setupTab.closeTab()
          tab
  ```
* **Problems:**
  1. **Leaked temp tab on exception:** In `openTab` (lines 142–146), `setupTab.closeTab()` is placed *outside* the `try ... finally` block. If `setupFn(setupTab)` throws an exception, `setupTab.closeTab()` is never called, leaving the temporary tab open in the browser and registered in `browser.tabs`.
  2. **Temporary setup tab triggers `onNewTab` and inflates tab counts for `GrowthStrategy`:** `setupTab = createTabWithoutLocking(browserIndex)` registers `setupTab` in `browser.tabs` and schedules `onNewTab(setupTab)` on it. Furthermore, `outerMutex.unlock()` is called on line 141 *before* `setupTab.closeTab()` runs on line 146, so concurrent callers to `openTab()` while `setupFn` is executing see `browser.tabs.length` inflated by the temporary setup tab, causing `GrowthStrategy` (e.g. `maxTabs` or `maxBrowsers`) to misroute new tabs to other browsers.
  3. **Thread pool leak in `MultiBrowser.par` (lines 205–210):**
     ```scala
     val threadPool = Executors.newFixedThreadPool(n)
     implicit val ec = ExecutionContext.fromExecutorService(threadPool)
     val fs = Future.traverse(browsers.indices)(i => Future( f(browsers(i))))
     Await.result(fs, Duration.Inf)
     threadPool.shutdown()
     ```
     `threadPool.shutdown()` is not in a `try`/`finally` block. If `f` throws in any browser (e.g. during `close()`), `Await.result` rethrows and the non-daemon fixed thread pool is leaked.

---

## 17. Additional Minor Defects

1. **`SeleniumExt.scala` (line 75) — `NullPointerException` in `WebElementExt.classes`:**
   ```scala
   def classes: Set[String] =
     self.getAttribute("class").trim match {
   ```
   In Selenium, `WebElement.getAttribute("class")` returns `null` when the element has no `class` attribute (or on certain drivers). Calling `.trim` directly without `Option(self.getAttribute("class"))` (unlike `getValue()` on line 86) throws a `NullPointerException`.

2. **`DomZipperJsoupF.scala` (lines 99–103) & `DomZippersFastAndSlow.scala` (lines 83–84) — Redundant `htmlScrub` applications:**
   `DomZipper.outerHTML` and `DomZipper.innerHTML` (`DomZipper.scala` lines 154–155) already apply `F.map(_outerHTML)(htmlScrub.run)`. However, `DomZipperJsoupF._outerHTML` and `_innerHTML` also call `htmlScrub.run(...)`, and `DomZippersFastAndSlow._outerHTML` delegates to `fast.outerHTML` (which calls `DomZipper.outerHTML` on `fast`). As a result, `htmlScrub` is applied **twice** on `DomZipperJsoup` and **three times** on `FastDomZipperSelenium`.

3. **`GrowthStrategy.scala` (line 34) — `GrowthStrategy.maxBrowsers(n)` crashes when `n <= 0`:**
   ```scala
   if (openTabsPerBrowser.length < maxBrowsers)
     openTabsPerBrowser.length
   else
     openTabsPerBrowser.zipWithIndex.minBy(_._1)._2
   ```
   When `openTabsPerBrowser` isInitially empty (`Vector.empty`) and `maxBrowsers <= 0`, `openTabsPerBrowser.length < maxBrowsers` is `false`, and `.minBy(_._1)` throws `UnsupportedOperationException: empty.minBy`.

4. **`VectorTree.scala` (line 18) — Misnamed recursion scheme `ana`:**
   `def ana[B](f: (A, Vector[B]) => B): Vector[B]` folds a `VectorTree[A]` bottom-up into `Vector[B]`, which is a catamorphism (`cata`), not an anamorphism (`ana`).

5. **`TodoComponent.scala` (lines 42–46, 82–86) — Wrong item index completed in example React app when `showCompleted` is `false`:**
   In `example-react`, `render` filters `s.items` with `s.items.iterator.filterNot(_.completed)` *before* `renderList` calls `.zipWithIndex` to bind `completeItem(idx)`. When completed items are hidden (`showCompleted == false`) and an earlier item in `s.items` is already completed, `idx` is the index in the filtered pending list rather than in `s.items`, so clicking "Complete" marks the wrong item in `s.items`.
