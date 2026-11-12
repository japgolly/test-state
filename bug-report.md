# Bug and Defect Report for `test-state`

This report details bugs, defects, and problems identified across the `test-state` codebase, ordered roughly by severity.

---

## 15. Check execution, categorization, and retry bugs in `Runner` (`Runner.scala`)

* **Location:** `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 383–478, 434–443, 491–547, 645–670)
* **Problems:**
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
