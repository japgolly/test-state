# Project Suggestions, Improvements & Optimisations

This document contains numbered suggestions, bug fixes, improvements, and optimisations identified across the `test-state` codebase.

---

## 7. `ReportFormat.Default.StatsFormat.formatDuration` has off-by-one time unit checks

* **Location:** `core/shared/src/main/scala/teststate/run/ReportFormat.scala` (lines 151–160)
* **Current Code:**
  ```scala
  def formatDuration(secPrec: Int): FiniteDuration => String = {
    val secfmt = "%." + secPrec + "f sec"
    d =>
      if (d.toMicros == 0)
        d.toNanos.toString + " μs"
      else if (d.toSeconds == 0)
        d.toMillis.toString + " ms"
      else
        secfmt.format(d.toMicros.toDouble / 1000000)
  }
  ```
* **Issue:**
  1. When `d < 1 microsecond` (e.g. `500 nanoseconds`), `d.toMicros == 0` is true, and it formats `d.toNanos` with the `" μs"` suffix (`"500 μs"`, off by 1000x).
  2. When `1 μs <= d < 1 ms` (e.g. `450 microseconds`), `d.toMicros == 0` is false and `d.toSeconds == 0` is true, so it falls into `d.toMillis.toString + " ms"` and prints `"0 ms"`.
* **Proposed Change:**
  ```scala
  def formatDuration(secPrec: Int): FiniteDuration => String = {
    val secfmt = "%." + secPrec + "f sec"
    d =>
      if (d.toMicros == 0)
        d.toNanos.toString + " ns"
      else if (d.toMillis == 0)
        d.toMicros.toString + " μs"
      else if (d.toSeconds == 0)
        d.toMillis.toString + " ms"
      else
        secfmt.format(d.toMicros.toDouble / 1000000)
  }
  ```

---

## 8. `ErrorHandler.toStringWithStackTrace` duplicates `err.toString` on the first two lines, and `ErrorHandler.name` prints `"WithCause(...)"` on exception

* **Location:** `core/shared/src/main/scala/teststate/typeclass/ErrorHandler.scala` (lines 21–35, 45–69)
* **Current Code:**
  ```scala
  def name[A](f: NameFn[A], o: Option[A]): Name =
    o match {
      case Some(_) =>
        attempt(f(o).value) match {
          case Right(n) => Name now n
          case Left(_)  => name(f, None)
        }
      case None =>
        Name.now(
          attempt(f(None).value) match {
            case Right(n) => n
            case Left(e)  => "Name exception: " + e.toString
          })
    }
  ...
  def toStringWithStackTrace(stackTraceMod: List[String] => List[String]): ErrorHandler[String] =
    ErrorHandler { err =>
      val st = stackTraceMod(stackTrace(err).split('\n').toList).mkString("\n").trim
      if (st.isEmpty)
        err.toString
      else
        s"$err\n$st"
    }
  ```
* **Issue:**
  1. `Throwable#printStackTrace(PrintWriter)` inside `stackTrace(err)` already includes `err.toString` as its first line. When `toStringWithStackTrace` (i.e. `toStringWithStackTrace(l => l)`) or a `pattern` matching the first line is used, `st` already starts with `err.toString`, and `s"$err\n$st"` prepends `err.toString` a second time.
  2. In `ErrorHandler.name`, `attempt(...)` returns `Left(e)` where `e` is `Failure.WithCause[E]`. Calling `e.toString` formats the case class `WithCause(toE(t), t)` instead of `toE(t)`. Also, in the `Some(_)` branch, `attempt` allocates `Right` on every successful name evaluation, and on failure runs `toE(t)` (which may format a stack trace) and allocates `Failure.WithCause` + `Left` only to discard it and call `name(f, None)`.
* **Proposed Change:**
  ```scala
  def name[A](f: NameFn[A], o: Option[A]): Name =
    o match {
      case Some(_) =>
        try Name.now(f(o).value)
        catch { case _: Throwable => name(f, None) }
      case None =>
        Name.now(
          try f(None).value
          catch { case t: Throwable => "Name exception: " + toE(t) })
    }

  def toStringWithStackTrace(stackTraceMod: List[String] => List[String]): ErrorHandler[String] =
    ErrorHandler { err =>
      val errStr = err.toString
      val raw = stackTrace(err)
      val traceOnly = if (raw.startsWith(errStr)) raw.substring(errStr.length).dropWhile(c => c == '\r' || c == '\n') else raw
      val st = stackTraceMod(traceOnly.split('\n').toList).mkString("\n").trim
      if (st.isEmpty)
        errStr
      else
        s"$errStr\n$st"
    }
  ```

---

## 9. `Runner.withRetryF1I` passes the first attempt `a0` instead of the latest attempt to `repeatAttempts`, and `retryPostChecks` reverts to `initialPostCheck` on `reObserve()` failure

* **Locations:**
  * `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 296–298, 506–510, 569–589)
  * `core/shared/src/main/scala/teststate/run/Retry.scala` (lines 30–55)
* **Current Code:**
  ```scala
  // Runner.scala
  private def withRetryF1I[A](scope: Retry.Scope, firstAttempt: => F[A])(repeatAttempts: A => F[A], failed: A => Boolean)
                            (implicit stats: Stats.Mutable): F[A] =
    EM.flatMap(firstAttempt)(a0 => _withRetryF(scope, a0)(repeatAttempts(a0), failed))
  ```
* **Issue:**
  1. In `runAction`, `onRetry(lastFailure: Result)` is passed to `withRetryF1I`. However, `withRetryF1I` always passes `a0` (the very first attempt's result) to `repeatAttempts(a0)` on every subsequent retry rather than the previous retry's result. If Attempt 1 fails with `ActionFail(e1, rx1)`, Retry 1 re-observes and runs the action again failing with `ActionFail(e2, rx2)`, and Retry 2 fails during `reObserve()`, `onRetry` receives `a0` (`e1, rx1`) instead of `e2, rx2`.
  2. Similarly, in `retryPostChecks` (`Runner.scala:506–510`), if `reObserve()` fails on a later retry after an earlier retry succeeded in `reObserve()`, `case Left(_) => initialPostCheck` reverts to the initial `ROS` and `PostCheckResults` instead of the latest one.
* **Proposed Change:**
  Update `Retry.Policy.retryI` (or add a stateful overload) to pass the current `a: A` from the `tailrec` state `(a, retryCtx1)` into `retryF: A => F[A]`.

---

## 10. Uncaught exceptions in `Action.Single` preparation (`.when` / `.unless` / `pmapR` / `pmapO`) crash `Runner` instead of failing the step, and `actionFn` is evaluated twice

* **Location:** `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 552–567)
* **Current Code:**
  ```scala
    def tryAttempt(ros: ROS): F[Result] =
      actionFn(ros) match {
        case None =>
          EM.pure(ActionResult.Skip)

        case Some(_) =>
          EM.flatMap(runPreChecksWithRetryFor(ros)) {
            case Right((rosPass, x)) =>
              actionFn(rosPass) match {
                case Some(act) => runActionOnly(rosPass, x, act)
                case None      => EM.pure(ActionResult.Skip)
              }
            case Left(ActionResult.PreCheckFail(p2, _)) =>
              EM.pure(ActionResult.PreCheckFail(p2, false))
          }
      }
  ```
* **Issue:**
  1. Unlike `Action.Group` (which wraps `cond.permit(p.ros)` in `attempt.attempt(...)` at line 667), `Action.Single` calls `actionFn(ros)` directly without `attempt.attempt(...)`. When a user attaches `.when(ros => ...)` or `.unless(ros => ...)` to a `Single` action (or `pmapR` / `pmapO`), that predicate runs inside `actionFn(ros)`. If it throws an exception, `tryAttempt` throws synchronously and aborts `Runner.run` instead of recording a failed step in the `Report`.
  2. When pre-checks pass without needing a retry (`rosPass eq ros`), `actionFn(rosPass)` is called a second time on the exact same `ROS` instance, re-evaluating `.when` predicates and `pmapR`/`pmapO` functions unnecessarily.
* **Proposed Change:**
  Wrap `actionFn` evaluation in `attempt.attempt` and reuse `act0` when `rosPass eq ros`:
  ```scala
    def tryAttempt(ros: ROS): F[Result] =
      attempt.attempt(actionFn(ros)) match {
        case Right(None) =>
          EM.pure(ActionResult.Skip)

        case Right(Some(act0)) =>
          EM.flatMap(runPreChecksWithRetryFor(ros)) {
            case Right((rosPass, x)) =>
              if (rosPass eq ros)
                runActionOnly(rosPass, x, act0)
              else
                attempt.attempt(actionFn(rosPass)) match {
                  case Right(Some(act)) => runActionOnly(rosPass, x, act)
                  case Right(None)      => EM.pure(ActionResult.Skip)
                  case Left(fe)         => EM.pure(ActionResult.ActionFail(fe, (rosPass, x)))
                }
            case Left(ActionResult.PreCheckFail(p2, _)) =>
              EM.pure(ActionResult.PreCheckFail(p2, false))
          }

        case Left(fe) =>
          val p2 = p0.copy(ros = ros) :+ History.Step(name, Fail(fe))
          EM.pure(ActionResult.PreCheckFail(p2, true))
      }
  ```

---

## 11. `Runner.runPreChecksWithRetry` re-runs `preCheckFn(p0)` on stale `ROS` when `reObserve()` fails, inflating `stats.checks`

* **Location:** `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 433–445)
* **Current Code:**
  ```scala
  private def runPreChecksWithRetry[A, B](preCheckFn: P => ActionResult.PreCheckFail[A] Or B,
                                          reObserve : () => F[FE Or (R, O)],
                                          p0        : P): F[ActionResult.PreCheckFail[A] Or (ROS, B)] = {
    def retry: F[ActionResult.PreCheckFail[A] Or (ROS, B)] =
      EM.map(reObserve()) {
        case Right((r, o)) =>
          val ros = ROS(r, o, p0.ros.state)
          preCheckFn(p0.copy(ros = ros)).map((ros, _))
        case Left(_) =>
          preCheckFn(p0).map((p0.ros, _))
      }
    withRetryF1(Retry.Scope.PreConditions, EM.point(preCheckFn(p0).map((p0.ros, _))), retry)(retryCheckAround0)
  }
  ```
* **Issue:**
  When `reObserve()` fails (`Left(_)`), `retry` calls `preCheckFn(p0)` again on the old `p0`. Because `preCheckFn` executes `runPreChecks`, it re-evaluates all pre-condition checks on the stale `p0.ros` and increments `stats.checks` for each check even though no new observation was obtained.
* **Proposed Change:**
  Reuse the previous `PreCheckFail` result when `reObserve()` returns `Left(_)` instead of calling `preCheckFn(p0)` again.

---

## 12. `ReportFormat.Default.addColour` ignores `uncoloured.indent` and hardcodes `"  "`

* **Location:** `core/shared/src/main/scala/teststate/run/ReportFormat.scala` (lines 246–266)
* **Current Code:**
  ```scala
    def addColour(uncoloured: Settings): Settings = {
      ...
      Settings(
        indent        = "  ",
        onPass        = BOLD + BRIGHT_GREEN + uncoloured.onPass + RESET + WHITE,
  ```
* **Issue:**
  Every field of `uncoloured` is preserved when constructing the coloured `Settings` except `indent`, which is hardcoded to `"  "`. If a caller customizes `indent` on `Settings` and passes it to `addColour`, their custom indentation is lost.
* **Proposed Change:**
  Use `indent = uncoloured.indent`.

---

## 13. `DomZipperSeleniumF._parent` wraps closure allocation in `F.attempt` instead of the `dom().parent()` call (Bug & Performance)

* **Location:** `dom-zipper-selenium/src/main/scala/teststate/domzipper/selenium/DomZipperSeleniumF.scala` (lines 87–88)
* **Current Code:**
  ```scala
  override protected def _parent: F[Dom] =
    F.attempt(() => dom().parent()(driver))
  ```
* **Issue:**
  1. **Error handling bug:** Because `type Dom = () => WebElement`, the expression passed to `F.attempt` is the lambda literal `() => dom().parent()(driver)`. Constructing a `Function0` never throws an exception, so `F.attempt` is a complete no-op when `.parent` is called. If `dom().parent()(driver)` fails or returns `null`, `z.failToOption.parent` and `z.failToEither.parent` erroneously succeed with `Some`/`Right` and throw an unhandled exception later when the zipper is inspected.
  2. **Performance issue:** Unlike `cssSelSelenium` (line 21) and `collectChildren` (lines 103, 108)—which eagerly resolve each `WebElement` and wrap the resolved reference in `() => elem`—`_parent` re-evaluates `dom().parent()(driver)` (executing JS over WebDriver) on **every single operation** on the parent zipper (`.tagName`, `.outerHTML`, `.innerText`, `.classes`, etc.).
* **Proposed Change:**
  ```scala
  override protected def _parent: F[Dom] =
    F.attempt {
      val p = dom().parent()(driver)
      if (p == null) sys.error(enrichErr("Element doesn't have a parent."))
      () => p
    }
  ```

---

## 14. `FastDomZipperSeleniumF.html` and `.body` apply `htmlScrub` redundantly (twice) and prior to Jsoup parsing

* **Location:** `dom-zipper-selenium/src/main/scala/teststate/domzipper/selenium/FastDomZipperSeleniumF.scala` (lines 21–31)
* **Current Code:**
  ```scala
  def html(driver: WebDriver)(implicit scrub: HtmlScrub): FastDomZipperSeleniumF[F] = {
    val s = DomZipperSelenium.html(driver)
    val f = DomZipperJsoup.parseHtml(s.outerHTML)
    FastDomZipperSeleniumF(f, s)
  }

  def body(driver: WebDriver)(implicit scrub: HtmlScrub): FastDomZipperSeleniumF[F] = {
    val s = DomZipperSelenium.body(driver)
    val f = DomZipperJsoup.parseBody(s.outerHTML)
    FastDomZipperSeleniumF(f, s)
  }
  ```
* **Issue:**
  1. `s.outerHTML` applies `scrub.run(s._outerHTML)` to the entire raw HTML string before passing it to `DomZipperJsoup.parseHtml` / `parseBody`. Then `DomZipperJsoup` also captures the implicit `scrub` and applies `scrub.run(...)` a **second time** whenever `.outerHTML` or `.innerHTML` is called on the `FastDomZipperSeleniumF`.
  2. Running `scrub` eagerly on the entire document HTML when instantiating `FastDomZipperSelenium` performs unnecessary regex work on every observation step even when `outerHTML`/`innerHTML` is never inspected.
  3. Applying `scrub` before `Jsoup.parse` can alter the parsed Jsoup DOM structure relative to the live Selenium DOM, risking desynchronization between `fast` and `slow`.
* **Proposed Change:**
  Use `s._outerHTML` (which is `protected[domzipper]`) when feeding the raw HTML into `DomZipperJsoup`:
  ```scala
  def html(driver: WebDriver)(implicit scrub: HtmlScrub): FastDomZipperSeleniumF[F] = {
    val s = DomZipperSelenium.html(driver)
    val f = DomZipperJsoup.parseHtml(s._outerHTML)
    FastDomZipperSeleniumF(f, s)
  }

  def body(driver: WebDriver)(implicit scrub: HtmlScrub): FastDomZipperSeleniumF[F] = {
    val s = DomZipperSelenium.body(driver)
    val f = DomZipperJsoup.parseBody(s._outerHTML)
    FastDomZipperSeleniumF(f, s)
  }
  ```

---

## 15. `JsoupElement.matches` and `JsoupElement.selectChildren` perform full-document / full-subtree queries instead of direct element evaluation

* **Location:** `dom-zipper-jsoup/src/main/scala/teststate/domzipper/jsoup/JsoupElement.scala` (lines 12–28, 42–52)
* **Current Code:**
  ```scala
  lazy val children: Vector[JsoupElement] =
    underlying.children().asScala.iterator.map(JsoupElement.apply).toVector

  lazy val childrenSet: Set[JsoupElement] =
    children.toSet

  def selectChildren(css: String): Vector[JsoupElement] =
    select(css).filter(childrenSet.contains)
  ...
  def attr(name: String): Option[String] =
    if (underlying.attributes().hasKeyIgnoreCase(name))
      Some(underlying.attr(name))
    else
      None
  ...
  def isSelected: Boolean =
    underlying.attributes().hasKeyIgnoreCase("checked")
  ...
  def root: JsoupElement =
    JsoupElement(underlying.root())

  def matches(sel: String): Boolean =
    root.underlying.select(sel).contains(this.underlying)
  ```
* **Issue:**
  1. `matches(sel)` traverses up to `underlying.root()`, runs a full-document `select(sel)` across every node in the document, builds an `Elements` `ArrayList`, and linearly scans it via `.contains(this.underlying)`. Notably, `DomZippersFastAndSlow.checked` calls `fast.matches("input[type=checkbox], input[type=radio]")` on every `.checked` call. Jsoup's `Element#is(String cssQuery)` evaluates the selector directly on `this.underlying` in $O(1)$ without querying the document tree.
  2. `selectChildren(css)` calls `select(css)` which searches the **entire descendant subtree** of `underlying`, wraps all matching descendants at any depth into `JsoupElement`s, builds a `Vector`, and then filters by `childrenSet.contains`. Using `QueryParser.parse(css)` (or `children.filter(_.matches(css))`) checks only immediate child elements and eliminates `childrenSet`.
  3. In `attr` and `isSelected`, calling `underlying.attributes().hasKeyIgnoreCase(...)` forces Jsoup to lazily allocate an `Attributes` instance on elements without attributes, whereas `underlying.hasAttr(...)` checks `hasAttributes()` first without allocating.
* **Proposed Change:**
  ```scala
  lazy val children: Vector[JsoupElement] =
    underlying.children().asScala.iterator.map(JsoupElement.apply).toVector

  def selectChildren(css: String): Vector[JsoupElement] = {
    val eval = org.jsoup.select.QueryParser.parse(css)
    children.filter(c => eval.matches(underlying, c.underlying))
  }
  ...
  def attr(name: String): Option[String] =
    if (underlying.hasAttr(name))
      Some(underlying.attr(name))
    else
      None
  ...
  def isSelected: Boolean =
    underlying.hasAttr("checked")
  ...
  def matches(sel: String): Boolean =
    underlying.is(sel)
  ```

---

## 16. `DomCollection.validateRaw` skips container validation in `head`, `last`, and `singleton` when unfiltered; eager `result` filtering and double-pass collection ops

* **Location:** `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipper.scala` (lines 254–351)
* **Current Code:**
  ```scala
  private val result: Vector[Z[F, A]] =
    filterFn match {
      case None    => rawResults
      case Some(f) => rawResults.filter(f)
    }

  private def validateRaw: F[Unit] =
    if (filterFn.isEmpty)
      F.pass(())
    else
      F.map(C(enrichErr, rawResults))(_ => ())
  ...
  def head: F[Z[F, A]] =
    F.flatMap(validateRaw)(_ =>
      if (result.isEmpty)
        fail("You called .head but collection is empty.")
      else
        F pass result.head
    )
  ...
  def collect01 = new DomCollection[Z, F, Option, Dom, A](enrichErr, result, None, new DomCollection.Container01[F])
  def collect0n = new DomCollection[Z, F, Vector, Dom, A](enrichErr, result, None, new DomCollection.Container0N[F])
  def collect1n = new DomCollection[Z, F, Vector, Dom, A](enrichErr, result, None, new DomCollection.Container1N[F])
  ```
* **Issue:**
  1. **Container validation inconsistency:** `validateRaw` returns `F.pass(())` when `filterFn.isEmpty` because `zippers` and `map` subsequently call `C(enrichErr, result)`. However, `head`, `last`, and `singleton` (lines 314–342) do **not** call `C(enrichErr, result)`. As a result, calling `$.collect01("button").head` when there are 2 buttons silently succeeds, whereas `$.collect01("button").filter(_ => true).head` fails with `"Expected [0,1] results, not 2."`.
  2. **Eager filter evaluation:** `private val result` is evaluated strictly in the constructor. Chaining `.filter(f1).filter(f2)` runs `rawResults.filter(f1)` eagerly on the intermediate `DomCollection` and then re-runs `f1` alongside `f2` on the final `DomCollection`. Making `result` a `lazy val` avoids intermediate filtering.
  3. **Double-pass & redundant allocations:** `mapDoms`, `traverseDoms`, `mapExtracts`, and `traverseExtracts` (lines 294–307) first call `doms` / `extracts` (allocating an intermediate `C[Dom]` / `C[A]`) and then map/traverse a second time. `collect01`, `collect0n`, and `collect1n` (lines 348–350) allocate new `Container01`/`0N`/`1N` objects instead of reusing `F.C01`, `F.C0N`, `F.C1N`.
* **Proposed Change:**
  ```scala
  private lazy val result: Vector[Z[F, A]] =
    filterFn match {
      case None    => rawResults
      case Some(f) => rawResults.filter(f)
    }

  private def validateRaw: F[Unit] =
    F.map(C(enrichErr, rawResults))(_ => ())

  def zippers: F[C[Z[F, A]]] =
    if (filterFn.isEmpty) C(enrichErr, rawResults)
    else F.flatMap(validateRaw)(_ => C(enrichErr, result))

  def map[B](f: Z[F, A] => B): F[C[B]] =
    if (filterFn.isEmpty) C(enrichErr, rawResults.map(f))
    else F.flatMap(validateRaw)(_ => C(enrichErr, result.map(f)))

  def mapDoms[B](f: Dom => B): F[C[B]] =
    map(z => f(z.dom))

  def traverseDoms[B](f: Dom => F[B]): F[C[B]] =
    traverse(z => f(z.dom))

  def mapExtracts[B](f: A => B): F[C[B]] =
    map(z => f(z.extract))

  def traverseExtracts[B](f: A => F[B]): F[C[B]] =
    traverse(z => f(z.extract))

  def collect01 = new DomCollection[Z, F, Option, Dom, A](enrichErr, result, None, F.C01)
  def collect0n = new DomCollection[Z, F, Vector, Dom, A](enrichErr, result, None, F.C0N)
  def collect1n = new DomCollection[Z, F, Vector, Dom, A](enrichErr, result, None, F.C1N)
  ```

---

## 17. `DomZippersFastAndSlow.cmap` uses `colS.flatMap(_.zippers).map(C.get(_, i))`, breaking after container conversion and throwing `IndexOutOfBoundsException` on DOM desync

* **Location:** `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZippersFastAndSlow.scala` (lines 45–61)
* **Current Code:**
  ```scala
  private def cmap[C[_]](runF: Fast => DomCollection[FastF, F, C, FX, FD],
                         runS: Slow => DomCollection[SlowF, F, C, SX, SD]) = {
    val colF = runF(fast)
    lazy val colS = slow().map(runS)
    val C = colF.C
    val rawResults = Vector.tabulate(colF.size) { i =>
      val f = colF.rawResults(i)
      lazy val s = colS.flatMap(_.zippers).map(C.get(_, i))
      FastAndSlow[F, FastF, FX, FD, SlowF, SX, SD](f, () => s, isCapableFn).toDomZipper(peek)
    }
  ```
* **Issue:**
  1. Each element's slow zipper `s` closes over the original `C` and calls `colS.flatMap(_.zippers).map(C.get(_, i))`. If a `collect01` collection has 2 elements and is converted to `0n` via `.collect0n`, or inspected via `.headOption` / `.lastOption`, accessing the slow zipper on any element fails because `colS.flatMap(_.zippers)` still validates against `Container01`, and `Container01.get(as, idx)` is `as.get` (ignoring `idx`).
  2. If the live (`slow`) DOM changes mid-observation and has fewer matching elements than `colF`, `C.get(_, i)` throws an uncatchable `IndexOutOfBoundsException` / `NoSuchElementException` instead of failing cleanly via `ErrorHandler[F]`.
* **Proposed Change:**
  Index `runS(_).rawResults` directly with a bounds check:
  ```scala
  private def cmap[C[_]](runF: Fast => DomCollection[FastF, F, C, FX, FD],
                         runS: Slow => DomCollection[SlowF, F, C, SX, SD]) = {
    val colF = runF(fast)
    lazy val rawS = slow().map(runS(_).rawResults)
    val rawResults = Vector.tabulate(colF.size) { i =>
      val f = colF.rawResults(i)
      lazy val s = rawS.flatMap { rs =>
        if (rs.isDefinedAt(i))
          F.pass(rs(i))
        else
          F.fail(colF.enrichErr(s"Inconsistency detected: element $i missing in slow DOM (found ${rs.length})."))
      }
      FastAndSlow[F, FastF, FX, FD, SlowF, SX, SD](f, () => s, isCapableFn).toDomZipper(peek)
    }
    new DomCollection[({ type L[X[_], Y] = DomZippersFastAndSlow[X, Dom, Y] })#L, F, C, Dom, A](
      enrichErr  = colF.enrichErr,
      rawResults = rawResults,
      filterFn   = None,
      C          = colF.C
    )
  }
  ```

---

## 18. Wasted allocations in `DomZipperBase.child`, `count`, `exists`, redundant WebDriver calls on empty child queries, and duplicated collection / `selectedOption` logic across backends

* **Locations:**
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipperBase.scala` (lines 46–104)
  * `dom-zipper/js/src/main/scala/teststate/domzipper/DomZipperJsF.scala` (lines 89–106, 135–149)
  * `dom-zipper-jsoup/src/main/scala/teststate/domzipper/jsoup/DomZipperJsoupF.scala` (lines 84–95)
  * `dom-zipper-selenium/src/main/scala/teststate/domzipper/selenium/DomZipperSeleniumF.scala` (lines 96–110, 136–150)
* **Current Code:**
  ```scala
  // DomZipperBase.scala
  final override def child(name: String, sel: String, which: MofN): F[Self[F, A]] = {
    val results = if (sel.isEmpty) children0n else children0n(sel)
    val desc = if (sel.isEmpty) ">*" else cssPrepend_>(sel)
    F.flatMap(results.zippers) { zippers =>
      if (zippers.length != which.n)
        F fail {
          val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
          enrichErr(s"${q}uery failed: [$desc]. Expected ${which.n} results, not ${zippers.length}.")
        }
      else
        F pass addLayer(Layer(name, desc, zippers(which.m - 1).dom))
    }
  }
  ```
* **Issue:**
  1. `DomZipperBase.child` calls `children0n` / `children0n(sel)`, which constructs a `DomCollection` and wraps **every** child DOM element in `addLayer(Layer("collect", desc, a))` (allocating a `Layer`, a new `Layers` vector, and a new `DomZipper` instance per child), only to extract `zippers(which.m - 1).dom`, discard all of them, and call `addLayer(Layer(name, desc, ...))` again.
  2. `DomZipper.count(sel)` and `DomZipper.exists(sel)` (`DomZipper.scala:173–177`) call `collect0n(sel).size` and `collect0n(sel).nonEmpty`, wrapping every matched DOM node in a `Layer`, `Layers`, and `DomZipper` instance just to check the vector length.
  3. In `DomZipperSeleniumF.collectChildren(desc, sel, C)` (lines 105–110) and `DomZipperJsF.collectChildren(desc, sel, C)` (lines 103–106), even when `runCssQuery(sel)` returns an empty result (`all.isEmpty`), it still executes `dom().children()` (a second WebDriver network round-trip in Selenium) and iterates all child nodes.
  4. `newDomCollection`, `collect`, and `collectChildren` are duplicated across `DomZipperJsF`, `DomZipperJsoupF`, and `DomZipperSeleniumF`, and `selectedOption` is duplicated identically in `DomZipperJsF` (lines 135–149) and `DomZipperSeleniumF` (lines 136–150).
* **Proposed Change:**
  Have subclasses implement `protected def rawChildren: CssSelResult[Dom]` and `protected def rawChildren(sel: String): CssSelResult[Dom]` (short-circuiting in JS and Selenium when `runCssQuery(sel)` is empty), and centralize `newDomCollection`, `collect`, `collectChildren`, `child`, `count`, and `exists` in `DomZipperBase`.

---

## 19. `DomZipperJsF.classes` returns `Set.empty` on SVG elements, and `dynamicMethod` returns `Some(null)` when a JS property is `null`

* **Location:** `dom-zipper/js/src/main/scala/teststate/domzipper/DomZipperJsF.scala` (lines 123–134, 152–153)
* **Current Code:**
  ```scala
  override def classes: Set[String] =
    dom match {
      case h: html.Element =>
        val c = h.classList
        (0 until c.length).iterator.map(c.apply).toSet
      case _ =>
        Set.empty
    }

  override def value: F[String] =
    dynamicMethod[String](_.value).map(_.toString) orFail s".value failed on $dom."
  ...
  def dynamicMethod[B](f: js.Dynamic => Any): Option[B] =
    f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[B]].toOption
  ```
* **Issue:**
  1. In the DOM standard and in `scalajs-dom`, `classList` is defined directly on `org.scalajs.dom.Element`, not only on `html.Element` (`HTMLElement`). Matching `case h: html.Element` performs an unnecessary runtime `instanceof HTMLElement` check and causes `.classes` to falsely return `Set.empty` for `<svg>` and all SVG elements. Also, in `childIterator` (lines 95–98), `dom.children.iterator` directly iterates child `Element`s without traversing text/comment nodes in `dom.childNodes`.
  2. In Scala.js, `js.UndefOr[B].toOption` only checks for `undefined`; if a JS property evaluates to `null`, `.toOption` returns `Some(null)`, which then throws a `NullPointerException` in `value` (`dynamicMethod[String](_.value).map(_.toString)`) or leaks `Some(null)` to callers.
* **Proposed Change:**
  ```scala
  override def classes: Set[String] = {
    val c = dom.classList
    (0 until c.length).iterator.map(c.apply).toSet
  }

  def dynamicMethod[B](f: js.Dynamic => Any): Option[B] = {
    val raw = f(dom.asInstanceOf[js.Dynamic])
    if (raw == null || js.isUndefined(raw)) None else Some(raw.asInstanceOf[B])
  }
  ```

---

## 20. `HtmlScrub.joinLines` and `HtmlScrub.empty` are `def`s, recompiling regexes on every access

* **Location:** `dom-zipper/shared/src/main/scala/teststate/domzipper/HtmlScrub.scala` (lines 6–31)
* **Current Code:**
  ```scala
  case class HtmlScrub(run: String => String) {
    def >>(that: HtmlScrub): HtmlScrub =
      HtmlScrub(run andThen that.run)
  ...
  object HtmlScrub {
    def empty: HtmlScrub =
      HtmlScrub(identity)
    ...
    def joinLines: HtmlScrub =
      replaceAll(">[\n\r\t ]*\n[\n\r\t ]*<", "><") >> replaceAll("[\n\r\t ]*\n[\n\r\t ]*", " ")
  }
  ```
* **Issue:**
  `joinLines` and `empty` are defined as `def` instead of `val`. When users define `implicit def htmlScrub: HtmlScrub = HtmlScrub.default >> HtmlScrub.joinLines` (as in `DomZipperJsoupTest` and `FastDomZipperSeleniumTest`), every implicit lookup of `HtmlScrub` recompiles two `Regex` patterns (`Pattern.compile`) and allocates 3 `HtmlScrub` objects. Additionally, composing with `HtmlScrub.empty` via `>>` wraps an extra `identity` function call in `andThen`.
* **Proposed Change:**
  Make `empty` and `joinLines` `val`s, and short-circuit `>>` when either operand is `HtmlScrub.empty`.

---

## 21. `DomCollection.vectorTraverse` is not stack-safe and allocates $O(N)$ intermediate `Vector`s and closures

* **Location:** `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipper.scala` (lines 419–428)
* **Current Code:**
  ```scala
  private def vectorTraverse[F[_], A, B](c: Vector[A])(f: A => F[B])(implicit F: ErrorHandler[F]): F[Vector[B]] = {
    def go(i: Int, v: Vector[B]): F[Vector[B]] =
      if (i == c.length)
        F pass v
      else {
        val a = c(i)
        F.flatMap(f(a))(b => go(i + 1, v :+ b))
      }
    go(0, Vector.empty)
  }
  ```
* **Issue:**
  `go` is not tail-recursive because the recursive call `go(i + 1, v :+ b)` is inside the closure passed to `F.flatMap`. For synchronous `ErrorHandler`s (`Throw`, `ReturnEither`, `ReturnOption`), traversing a collection of size $N$ consumes $O(N)$ stack frames, allocates $N$ closure objects, and copies `v :+ b` $N$ times.
* **Proposed Change:**
  Use `foldLeft` with `Vector.newBuilder[B]`:
  ```scala
  private def vectorTraverse[F[_], A, B](c: Vector[A])(f: A => F[B])(implicit F: ErrorHandler[F]): F[Vector[B]] =
    if (c.isEmpty)
      F.pass(Vector.empty)
    else {
      val init = F.pass(Vector.newBuilder[B])
      val r = c.foldLeft(init)((acc, a) => F.flatMap(acc)(bs => F.map(f(a))(bs += _)))
      F.map(r)(_.result())
    }
  ```

---

## 22. `domzipper.ErrorHandler.attempt` passes `null` when `Throwable#getMessage` is `null`, and several `DomZipper` failure paths omit `enrichErr`

* **Locations:**
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/ErrorHandler.scala` (lines 24–27)
  * `dom-zipper/shared/src/main/scala/teststate/domzipper/DomZipper.scala` (lines 151–152)
  * `dom-zipper/js/src/main/scala/teststate/domzipper/DomZipperJsF.scala` (lines 120–121, 132–133, 159–160)
  * `dom-zipper-jsoup/src/main/scala/teststate/domzipper/jsoup/DomZipperJsoupF.scala` (lines 96–97, 114–115)
  * `dom-zipper-selenium/src/main/scala/teststate/domzipper/selenium/DomZipperSeleniumF.scala` (lines 133–134)
* **Current Code:**
  ```scala
  // ErrorHandler.scala
  def attempt[A](a: => A): Result[A] =
    try pass(a) catch {
      case NonFatal(t) => fail(t.getMessage)
    }

  // DomZipper.scala
  final def needAttribute(name: String): F[String] =
    F.option(getAttribute(name), s"$tagName doesn't have attribute $name")
  ```
* **Issue:**
  1. In `ErrorHandler.attempt`, `t.getMessage` returns `null` when an exception is constructed without a message (e.g. `new NullPointerException()`), causing `ReturnEither` to return `Left(null)`.
  2. `DomZipper.needAttribute`, `.value`, `.checked`, `_parent`, and `domAs` fail without wrapping their error message in `enrichErr(...)`, omitting the `DESC: ... \n HTML: ...` context that all other `DomZipper` errors provide. Since `F.option` and `orFail` take the error string by-name (`=> String`), calling `enrichErr(...)` has zero cost when no error occurs.
* **Proposed Change:**
  Use `Option(t.getMessage).getOrElse(t.toString)` in `ErrorHandler.attempt`, and wrap the error messages in `needAttribute`, `.value`, `.checked`, `_parent`, and `domAs` with `enrichErr(...)`.

---

## 23. `DomZipperSeleniumF.matches` queries and serializes all matching elements in the entire document; `SeleniumExt.parent()` fails on `<html>`; `addStyleTag` assumes `<head>` exists

* **Locations:**
  * `dom-zipper-selenium/src/main/scala/teststate/domzipper/selenium/DomZipperSeleniumF.scala` (lines 112–116)
  * `util-selenium/src/main/scala/teststate/selenium/util/SeleniumExt.scala` (lines 48–58, 126–127)
  * `dom-zipper/js/src/main/scala/teststate/domzipper/JsDomExt.scala` (lines 16–22)
* **Current Code:**
  ```scala
  // DomZipperSeleniumF.scala
  override def matches(css: String): F[Boolean] = {
    val all = driver.findElements(By.cssSelector(css)).asScala
    val dom = this.dom()
    F pass all.contains(dom)
  }

  // SeleniumExt.scala
  def parent()(implicit d: WebDriver): WebElement =
    d.executeJsOrThrow("return arguments[0].parentNode", self).asInstanceOf[WebElement]
  ```
* **Issue:**
  1. `DomZipperSeleniumF.matches(css)` executes `driver.findElements(By.cssSelector(css))` across the entire document and transfers every matching `WebElement` over the WebDriver protocol just to check if `this.dom()` is in the list. Executing `return arguments[0].matches(arguments[1])` checks the element directly in the browser in $O(1)$.
  2. In `SeleniumExt.parent()`, when `self` is the root `<html>` element (`document.documentElement`), `arguments[0].parentNode` returns the `Document` node (`#document`), which Selenium deserializes as a `java.util.Map` rather than a `WebElement`, throwing a `ClassCastException`. Using `parentElement` returns `null` instead.
  3. Both `SeleniumExt.addStyleTag` and `JsDomExt.addStyleTag` call `getElementsByTagName("head")(0).appendChild(style)` without fallback if `<head>` is absent.
* **Proposed Change:**
  Use `arguments[0].matches(arguments[1])` in `DomZipperSeleniumF.matches`, `return arguments[0].parentElement` in `SeleniumExt.parent()`, and `(document.head || document.documentElement).appendChild(style)` in `addStyleTag`.

---

## 24. `Tab`: `afterFirstUse` and `aroundFirstUse` execute *before* the first use (wrapping a no-op `doNothing0`), ignore `ExecutionModel[M]` in `useM`, and redundantly re-activate newly opened tabs

* **Location:** `ext-selenium/src/main/scala/teststate/selenium/Tab.scala` (lines 64–114)
* **Current Code:**
  ```scala
  private def prepareWithoutLocking(): D = {
    implicit def d: D = driver

    if (closed)
      throw new TabAlreadyClosed()

    tab match {
      case Some(t) =>
        tabSupport.activate(t)
      case None =>
        val t = tabSupport.open(rootTab)
        tab = Some(t)
        tabSupport.activate(t)
        onFirstUse.foreach { f =>
          val m = f(d)
          val g = m[Id, Unit](doNothing0)
          g()
        }
    }

    d
  }
  ```
* **Issue:**
  1. **`afterFirstUse` and `aroundFirstUse` wrap `doNothing0` instead of the first use `f(d)`:** Because `prepareWithoutLocking()` evaluates `onFirstUse` applied to `doNothing0` (`() => ()`) *before* returning to `use` / `useM`, any callback registered via `afterFirstUse` (or the "after" half of `aroundFirstUse`) executes **before** `beforeEachUse` and **before** the first use `f(d)` itself has even started.
  2. **`useM` forces `onFirstUse` into synchronous `Id`:** In `useM`, `onFirstUse` is run with `m[Id, Unit](doNothing0)` inside `prepareWithoutLocking()` instead of wrapping `() => f(d): M[A]` using `ExecutionModel[M]`.
  3. **Redundant `tabSupport.activate(t)` after `tabSupport.open(rootTab)`:** `TabSupport.open` already activates the newly created tab before returning, so calling `tabSupport.activate(t)` on line 76 immediately makes redundant WebDriver round-trips (`switchTo.window` + `getWindowHandle`).
* **Proposed Change:**
  Have `prepareWithoutLocking()` return `(D, Option[D => ProcMod])` (combining `onFirstUse` with `onEachUse` on the first use) and remove the redundant `tabSupport.activate(t)`.

---

## 25. `MultiBrowser.openTab()`: temp setup tab closed after unlocking `browser.mutex`, and `onNewTab` runs inside `outerMutex` before `onNewDriverWithTempTab`

* **Location:** `ext-selenium/src/main/scala/teststate/selenium/MultiBrowser.scala` (lines 98–163)
* **Current Code:**
  ```scala
            val setup =
              if (onNewDriverT eq doNothing1) None else Some {
                val setupTab = browser.multiTab.openTab()
                val setupFn  = onNewDriverT
                // Lock browser so that no one else can create a new tab in this browser before setup completes
                mutex.lock()
                () => {
                  try
                    setupFn(setupTab)
                  finally {
                    try
                      mutex.unlock()
                    finally {
                      // closeTab() will attempt to acquire the outer lock through the afterClosed callback so the
                      // browser lock must be released beforehand to maintain lock-ordering (i.e. outer before browser)
                      setupTab.closeTab()
                      ()
                    }
                  }
                }
              }
            val tab = createTabWithoutLocking(browserIndex)
            (tab, setup)
  ```
* **Issue:**
  1. **Temp setup tab remains open when waiting threads acquire `browser.mutex`:** In `setup`, `mutex.unlock()` (line 144) is called *before* `setupTab.closeTab()` (line 148). The comment on lines 146–147 is obsolete now that `setupTab` is created via `browser.multiTab.openTab()` without the `removeTab` `afterClosed` callback. Unlocking `mutex` before `setupTab.closeTab()` allows concurrent threads blocked on `browser.mutex` to wake up and interact with the browser while `setupTab` is still open, and then race with `setupTab.closeTab()`.
  2. **`onNewTab` runs inside `outerMutex` and before `onNewDriverWithTempTab`:**
     * On line 138, `mutex.lock()` is acquired before `createTabWithoutLocking(browserIndex)` (line 154), which invokes `onNewTab(tab)` (line 104). If `onNewTab(tab)` throws an exception, `openTab()` aborts before reaching `setup.foreach(_.apply())` on line 160, so `mutex.unlock()` and `setupTab.closeTab()` are never called, permanently deadlocking that browser.
     * For the first tab of a new browser, `onNewTab(tab)` runs *before* `onNewDriverWithTempTab(setupTab)`. If `onNewTab(tab)` calls `tab.use(...)` synchronously, because `mutex` is reentrant and already held by the current thread from line 138, `tab.use(...)` executes in the browser *before* `onNewDriverWithTempTab` has run.
  3. **`instances` is updated before `onNewDriver(driver)`:** On lines 129–131, `browser` is appended to `instances` before `onNewDriver(driver)` runs; if `onNewDriver(driver)` throws, `instances` retains the uninitialized browser.
* **Proposed Change:**
  Close `setupTab` before unlocking `mutex`, run `onNewDriver(driver)` before appending `browser` to `instances`, and run `onNewTab(tab)` outside `outerMutex` after `setup` completes.

---

## 26. `Mutex`: missing thread-owner check in `unlock()` when `owner ne null`, and non-tail-recursive retry in `monadic`

* **Location:** `ext-selenium/src/main/scala/teststate/selenium/Mutex.scala` (lines 45–78)
* **Current Code:**
  ```scala
  def unlock(): Unit = synchronized {
    holds match {
      case 0 => throw new IllegalMonitorStateException()
      case 1 =>
        owner = null
        holds = 0
        semaphore.release()
      case n =>
        holds = n - 1
    }
  }
  ```
* **Issue:**
  1. When `Mutex` is locked synchronously via `lock()` or `tryLock(wait)` (so `owner` is set to the owning `Thread`), `unlock()` does not verify that `Thread.currentThread() eq owner`. Checking `if ((owner ne null) && (owner ne Thread.currentThread())) throw new IllegalMonitorStateException()` enforces ownership for thread-bound locks while still allowing `monadic` locks (`owner eq null`) to unlock across threads.
  2. In `monadic`, `go` recurses directly inside `EM.schedule(go, ...)` instead of using `EM.tailrec`.
* **Proposed Change:**
  Add the owner check in `unlock()` and use `EM.tailrec` in `monadic`.

---

## 27. `ExtNyaya.genActionsBy` generates all `sampleSize` samples upfront from the initial `ROS` instead of per sample step

* **Location:** `ext-nyaya/shared/src/main/scala/teststate/ExtNyaya.scala` (lines 16–31)
* **Current Code:**
  ```scala
  final class DslNyayaOps[F[_], R, O, S, E](private val dsl: Dsl[F, R, O, S, E]) extends AnyVal with Dsl.Types[F, R, O, S, E] {
    def genActions(name: ActionName)(g: Gen[Actions])(implicit s: Settings): Actions =
      ExtNyaya.genActions(g)(name)(s)

    def genActionsBy(name: ActionName)(g: ROS => Gen[Actions])(implicit s: Settings): Actions =
      dsl.chooseAction(name)(i => genActions(name)(g(i))(s))
  }
  ```
* **Issue:**
  `genActionsBy(name)(g: ROS => Gen[Actions])` is meant for stateful action generation where `g` inspects the current observation/state `ROS`. However, `dsl.chooseAction(name)(i => genActions(name)(g(i))(s))` invokes `chooseAction` only **once** before the first sample runs, passing the initial `i: ROS` to `genActions(name)(g(i))(s)`, which immediately draws all `s.sampleSize.value` samples from `g(initialROS)`. As a result, samples `2 .. sampleSize` are generated using the stale initial `ROS` instead of the updated `ROS` after each preceding sample executes.
* **Proposed Change:**
  Evaluate `chooseAction` for each sample step so that each generated action sample receives the current `ROS` at the time that step executes:
  ```scala
    def genActionsBy(name: ActionName)(g: ROS => Gen[Actions])(implicit s: Settings): Actions = {
      val total = s.sampleSize.value
      (1 to total)
        .iterator
        .map { idx =>
          dsl.chooseAction(name) { ros =>
            g(ros)
              .samples(s.genSize)
              .next()
              .groupIfMultipleByLen(_.toString + " generated actions.")
              .nameMod(n => s"[$idx/$total] ${n.value}")
          }
        }
        .combine
        .group(name)
    }
  ```

---

## 28. `ExtScalaJsReact`: unsuspended by-name parameters in `doFinally`, `schedule`, and `recover`, non-implicit `executionModalAsyncCallback`, and bare `.get` in `domZipper`

* **Location:** `ext-scalajs-react/src/main/scala/teststate/ExtScalaJsReact.scala` (lines 54–90, 104–107)
* **Current Code:**
  ```scala
  lazy val executionModalAsyncCallback: ExecutionModel[AsyncCallback] =
    new ExecutionModel[AsyncCallback] {
      ...
      override def recover[E, A](f: => F[Or[Failure[E], A]])(implicit attempt: ErrorHandler[E]): F[Or[Failure[E], A]] =
        attempt.recover(
          f.attempt.map(_.fold(t => teststate.data.Left(attempt(t)), identity)),
          t => AsyncCallback.pure(teststate.data.Left(t)))

      override val now: F[Instant] =
        AsyncCallback.delay(Instant.now())

      override def schedule[A](task: => F[A], startAt: Instant): F[A] =
        now.flatMap { n =>
          val d = startAt.toEpochMilli - n.toEpochMilli
          task.delayMs(d.toDouble)
        }

      override def doFinally[A, B](main: => F[A], last: => F[B]): F[A] =
        main.finallyRun(last)
    }
  ...
  final class ExtScalaJsReactCompExt(private val m: GenericComponent.MountedRaw) extends AnyVal {
    def domZipper(implicit $: CssSelEngine, scrub: HtmlScrub): DomZipperJs =
      DomZipperJs(m.displayName, ReactDOM.findDOMNode(m.raw).get.asElement())($, scrub)
  }
  ```
* **Issue:**
  1. **`doFinally` evaluates `main` and `last` eagerly and misses synchronous throws:** Calling `main.finallyRun(last)` evaluates both by-name expressions `main` and `last` immediately when `doFinally` is called (before the `AsyncCallback` is run). If evaluating `main` throws a synchronous exception, `last` is never run and the exception escapes instead of being captured in `AsyncCallback`.
  2. **`schedule` evaluates `task` before the delay and schedules timers even when `d <= 0`:** Calling `task.delayMs(d.toDouble)` evaluates the by-name `task` *before* waiting `d` ms, and queues a JS `setTimeout` even when `d <= 0`.
  3. **`recover` evaluates `f` when building the `AsyncCallback` rather than when running it:** Wrapping `f` in `AsyncCallback.suspend(f)` ensures `f` is evaluated each time the `AsyncCallback` runs.
  4. **`executionModalAsyncCallback` is not `implicit` (and has a typo `Modal` vs `Model`):** Marking `executionModelAsyncCallback` `implicit` (and keeping `executionModalAsyncCallback` as a deprecated alias) allows `Dsl.full[AsyncCallback, ...]` to resolve `ExecutionModel[AsyncCallback]` automatically.
  5. **`domZipper` calls `.get` on `ReactDOM.findDOMNode(m.raw)`:** If a component renders `EmptyVdom` / `null`, `.get` throws an uninformative `NoSuchElementException: None.get` without naming `m.displayName`.
* **Proposed Change:**
  Use `AsyncCallback.suspend(...)` in `recover`, `schedule`, and `doFinally`, skip `.delayMs` when `d <= 0`, provide `implicit lazy val executionModelAsyncCallback`, and use `.getOrElse(sys.error(...))` in `domZipper`.

---

## 29. `ExecutionModel.scalaFuture`: `schedule` uses `new Date` and goes through `Timer` when `delayMs <= 0`, and `doFinally` drops primary exception when both `main` and `last` fail

* **Location:** `core/shared/src/main/scala/teststate/typeclass/ExecutionModel.scala` (lines 82–87, 109–141)
* **Current Code:**
  ```scala
      override def schedule[A](task: => F[A], startAt: Instant) = {
        ...
        try {
          futureTimer.schedule(timerTask, new Date(startAt.toEpochMilli))
        } catch {
          case t: Throwable => promise.failure(t)
        }
        promise.future
      }

      override def doFinally[A, B](main: => Future[A], last: => Future[B]): Future[A] = {
        val fa = try main catch { case t: Throwable => Future.failed(t) }
        fa.transformWith { ta =>
          val fb = try last catch { case t: Throwable => Future.failed(t) }
          fb.transform {
            case ScalaSuccess(_) => ta
            case ScalaFailure(f) => ScalaFailure(f)
          }
        }
      }
  ```
* **Issue:**
  1. When `Retry.Policy` schedules an immediate retry (`delayMs <= 0`), going through the global synchronized `java.util.Timer` thread and allocating a `Date` and `TimerTask` adds unnecessary thread-hop latency and contention on `futureTimer`. Using a relative delay `val delayMs = startAt.toEpochMilli - System.currentTimeMillis()` (and running directly on `ec` when `delayMs <= 0`) avoids allocating `new Date(...)`. Similarly, `Immediate.schedule` (line 83) allocates `Instant.now()` just to call `.toEpochMilli`.
  2. In `doFinally`, when `main` fails with `ScalaFailure(e)` and `last` also fails with `ScalaFailure(f)`, `e` is dropped without `f.addSuppressed(e)`.
* **Proposed Change:**
  Short-circuit `schedule` when `delayMs <= 0`, pass `delayMs` to `futureTimer.schedule(timerTask, delayMs)`, and call `f.addSuppressed(e)` in `doFinally`.

---

## 30. `RunnableTest.run()` and `Runner.run` evaluate `refFnFn()` and allocate `Runner` (with mutable `stats`) outside `EM.point`, and `RunCallbacks.timed` uses `F.point(Instant.now())` instead of `F.now`

* **Locations:**
  * `core/shared/src/main/scala/teststate/run/Test.scala` (lines 320–321)
  * `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 222–226, 312–326)
  * `core/shared/src/main/scala/teststate/run/RunCallbacks.scala` (lines 72–79)
* **Current Code:**
  ```scala
  // Test.scala
  def run(): F[Report[E]] =
    Runner.run(test)(initialState, refFnFn(), callbacks)

  // RunCallbacks.scala
  def timed(f: (ROS[R, O, S], Name, Option[E], Duration) => F[Unit]): RunCallbacks[F, R, O, S, E] =
    apply((ros, n) => F.point {
      val start = Instant.now()
      oe => F.flatMap(F.point(Instant.now())) { end =>
        val dur = Duration.between(start, end)
        f(ros, n, oe, dur)
      }
    })
  ```
* **Issue:**
  1. For lazy/repeatable effect types `F` (such as `AsyncCallback`): `refFnFn()` in `RunnableTest.run()` and `new Runner` in `Runner.run` are evaluated eagerly when constructing `F[Report[E]]` rather than when `F` executes. If `withLazyRef` is used and the returned `F[Report[E]]` is executed more than once, the same memoized `lazy val r` and mutable `runner.stats` are shared across runs.
  2. In `RunCallbacks.Dsl.aroundActions.timed`, `F.point(Instant.now())` is used instead of `F.now`. For `ExecutionModel.scalaFuture`, `F.now` is `Future.successful(Instant.now())` (immediate), whereas `F.point(Instant.now())` schedules a `Runnable` onto the `ExecutionContext` thread pool *after* the action completes before recording `end`, inflating the measured duration with thread-pool queue wait time.
* **Proposed Change:**
  Suspend `Runner` instantiation and `refFnFn()` inside `EM.point` (making `stats` a `val` on `Runner`), and use `F.now` in `RunCallbacks.timed`.

---

## 31. `RefEq` uses structural `value.hashCode`, and `UniqueListBuilder` / `UnpackChecks` eagerly allocates 7 `LinkedHashSet`s per action

* **Location:** `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 131–209, 458–461, 706–734)
* **Current Code:**
  ```scala
  final class RefEq[+A <: AnyRef](val value: A) {
    override def hashCode = value.hashCode
    override def equals(x: Any) = x match {
      case y: RefEq[AnyRef] => value eq y.value
      case _                => false
    }
  }

  final class UniqueListBuilder[A <: AnyRef] {
    private val set = mutable.LinkedHashSet.empty[RefEq[A]]
    ...
  }
  ```
* **Issue:**
  1. `RefEq.equals` compares by reference identity (`eq`), so `hashCode` should be `System.identityHashCode(value)` rather than structural `value.hashCode`.
  2. Before *every* action in a test, `runPreChecks` calls `UnpackChecks.invariants` (which allocates 3 `UniqueListBuilder`s / `LinkedHashSet`s) and `UnpackChecks.arounds` (which allocates 4 `UniqueListBuilder`s / `LinkedHashSet`s), even when `invariants.isEmpty` or `arounds.isEmpty`, or when `errors`/`befores`/`deltas` are empty.
  3. In `postChecks` (lines 458–461), if only `invariants` has a coproduct, `arounds` is unnecessarily unpacked again, and vice versa.
  4. In `subtest.runInvariants` (lines 706–730), `ps` and `es` are mapped into `Right(_)` and `Left(_)`, concatenated into a `List[CheckNE[...]]`, and immediately pattern-matched back out in `b.addEachNE`.
* **Proposed Change:**
  Use `System.identityHashCode(value)` in `RefEq`, lazily allocate `set` in `UniqueListBuilder` (or short-circuit `UnpackChecks` when the `Sack` is empty), check `coproductFound` for `arounds` and `invariants` independently in `postChecks`, and add `ps` / `es` directly in `runInvariants`.

---

## 32. `History.:+`, `History.++`, `History.failed`, and `History.rootFailurePath` allocations and stack safety

* **Location:** `core/shared/src/main/scala/teststate/run/History.scala` (lines 12–36, 53–56, 119–120)
* **Current Code:**
  ```scala
  def failure = result.failure
  def failed = failure.isDefined
  def isEmpty = steps.isEmpty
  @inline def nonEmpty = !isEmpty

  def :+[e >: E](s: Step[e]) = this ++ vector1(s)
  def ++[e >: E](s: Steps[e]): History[e] = {
    val result2 = s.foldLeft[Result[e]](result)(_ + _.result)
    new History[e](steps ++ s, result2)
  }
  def ++[e >: E](s: History[e]): History[e] =
    new History[e](steps ++ s.steps, result + s.result)
  ```
* **Issue:**
  1. `def :+[e >: E](s: Step[e]) = this ++ vector1(s)` allocates a 1-element `Vector(s)`, a `foldLeft` closure, and calls `Vector.++` instead of `new History[e](steps :+ s, result + s.result)`.
  2. `++(s: History[e])` is called multiple times per action in `Runner` (`pre ++ step ++ post`, `post ++ invariants`), where `pre`, `post`, or `invariants` is frequently `History.empty`. Short-circuiting when either side is empty avoids allocating new `History` and `Vector` objects.
  3. `failed` in `History`, `History.Step`, and `History.Builder` calls `result.failure.isDefined`. In `Result.Fail`, `override def failure = Some(error)` allocates a new `Some` on every call. Checking `result.isInstanceOf[Fail[_]]` (or adding `def isFail: Boolean` on `Result`) avoids allocating `Some` on every `.failed` check in `Runner`'s hot loops.
  4. `rootFailurePath` is not `@tailrec` and uses `_.result eq result` in `resultStep`.
* **Proposed Change:**
  Implement `:+` directly, short-circuit `++` on empty `History`, avoid `Some` allocation in `failed`, and make `rootFailurePath` `@tailrec`.

---

## 33. `ReportFormat.Default.format` quadratic string slicing on multi-line error messages, and repeated allocations in `Equal`, `Display`, and `Report.AssertionSettings`

* **Locations:**
  * `core/shared/src/main/scala/teststate/run/ReportFormat.scala` (lines 75–96)
  * `core/shared/src/main/scala/teststate/typeclass/Equal.scala` (lines 13–14, 41–45)
  * `core/shared/src/main/scala/teststate/typeclass/Display.scala` (lines 40–41, 51–52, 95, 109–110)
  * `core/shared/src/main/scala/teststate/run/Report.scala` (lines 107–115)
* **Current Code:**
  ```scala
  // ReportFormat.scala
  @tailrec
  def go(line: String, remainderWithLine: String): Unit = {
    val remainder = remainderWithLine.drop(line.length + 1)
    sb append s.eol
    appendIndent(indent2)
    sb append s.failureDetail
    sb append line
    if (remainder.nonEmpty)
      go(remainder.takeWhile(_ != '\n'), remainder)
  }
  go(firstLine, errorMsg)
  ```
* **Issue:**
  1. In `ReportFormat.Default.format`, on multi-line error messages (such as stack traces or DOM diffs), `remainderWithLine.drop(...)` and `remainder.takeWhile(_ != '\n')` allocate two new `String`s per line instead of scanning with `errorMsg.indexOf('\n', start)` and `sb.append(errorMsg, start, lineEnd)`.
  2. In `Equal.scala`, `testStateEqualIndexedSeq` calls `x.indices.forall(i => e.equal(x(i), y(i)))`, which allocates a `Range` and boxes every `Int` index into `java.lang.Integer` instead of using `x.corresponds(y)(e.equal)`. Also `Equal.by_==[A]` allocates a new `Equal` on every `testStateEqualByUnivEq` implicit lookup instead of casting a shared private `Equal[Any]` instance.
  3. In `Display.scala`, `testStateDisplayByToString[A]` and `byToString[A]` allocate a new `Display` on every implicit lookup instead of sharing a private `Display[Any]`, and `testStateDisplayName` (line 109) is a `def` instead of a `val`.
  4. In `Report.AssertionSettings`, `uncoloured`, `coloured`, and `default` are `def`s rather than `val`s, so every implicit resolution of `Exports.testStateAssertionSettings` allocates 2 `ReportFormat.Default` instances, a copied `Settings`, and an `AssertionSettings`.
* **Proposed Change:**
  Use `indexOf('\n', start)` in `ReportFormat`, `corresponds` and a shared `anyEqual` in `Equal`, a shared `anyToString` and `val testStateDisplayName` in `Display`, and `val`s in `Report.AssertionSettings`.

---

## 34. Full `ROS` reference retention across async action execution in `ActionOps.mapOS`, `Action.Single.empty`, and `Dsl.ActionB.attempt`, plus `Or` and `Sack` allocations

* **Locations:**
  * `core/shared/src/main/scala/teststate/core/ActionOps.scala` (lines 231–234, 347–373)
  * `core/shared/src/main/scala/teststate/core/Action.scala` (line 29)
  * `core/shared/src/main/scala/teststate/dsl/Dsl.scala` (line 130)
  * `core/shared/src/main/scala/teststate/data/Or.scala` (lines 17–83, 96–105)
  * `core/shared/src/main/scala/teststate/data/Sack.scala` (lines 53–69)
  * `core/shared/src/main/scala/teststate/data/VectorTree.scala` (lines 35–36)
* **Current Code:**
  ```scala
  // ActionOps.scala:231-234
  case Single(r) =>
    Single(i =>
      r(i.mapOS(f, g)).map(fn => () =>
        em.map(fn())(_.map(j => (x: X) => j(f(x)).map(s => h(i.state, s))))))
  ```
* **Issue:**
  1. Referencing `i.state` inside the innermost post-action closure `s => h(i.state, s)` (and similarly `ros.state` in `Action.Single.empty`, `ActionOps.stateMod`, and `Dsl.ActionB.attempt`) causes the nested lambdas to capture the entire `i: ROS` instance, retaining `i.ref` and the pre-action observation `i.obs` (such as a DOM snapshot) in memory throughout the asynchronous action and post-action observation. Extracting `val s0 = i.state` before `.map(fn => ...)` captures only the state.
  2. In `Or.scala`, `map` delegates to `flatMap(b => Right(f(b)))` and `bimap`, `exists`, `leftOption`, `toOption`, `toOptionMap`, `toTri`, `toTriMap`, `toTriFlatMap`, and `toEither` delegate to `fold(...)` (allocating 1–2 closures per call instead of direct pattern matching), and `Or.liftLeft` allocates a new `Right(())` on every `None`.
  3. In `Sack.scala`, `sackInstanceProfunctor` does not override `lmap` (falling back to `dimap(m)(g, identity)`, which re-allocates every `Value(b)`), and `Product(s)` in `Sack`, `CheckOps`, and `ActionOps` does not short-circuit when `s.isEmpty` (`Sack.empty`).
  4. In `VectorTree.Node.iteratorWithLevel`, leaf nodes (`children.elements.isEmpty`) still allocate `children.iteratorWithLevel(currentLevel + 1)` and a `ConcatIterator`.
* **Proposed Change:**
  Extract `val s0 = i.state` before nested closures, use direct pattern matching and a cached `rightUnit` in `Or`, override `lmap` and short-circuit on empty `Product` in `Sack`/`CheckOps`/`ActionOps`, and short-circuit `iteratorWithLevel` on leaf nodes.

---

## 35. Algorithmic and allocation improvements in `CollectionAssertions`

* **Location:** `core/shared/src/main/scala/teststate/dsl/CollectionAssertions.scala` (lines 15–23, 71–80, 256–296, 411–428, 489–507, 525–555)
* **Issues & Proposed Changes:**
  1. **`tallyElements` (line 19):** `m.get(a).fold(n)(_ + n)` allocates a `Some` option and closure on every key hit; replace with `m.getOrElse(a, 0) + n`.
  2. **`Distinct.Neg` (lines 71–80):** Builds a full `mutable.HashMap[A, Int]` of counts for all elements when it only needs to detect the first duplicate; replace with `val seen = mutable.HashSet.empty[A]; if (as.iterator.exists(!seen.add(_))) None else Some(Was)`.
  3. **`EqualIncludingOrder.Pos` and `Neg` (lines 411–428):** `Neg` materializes two `Seq`s before calling `corresponds` instead of using `source.iterator.corresponds(expect.iterator)(eq.equal)` in $O(1)$ memory; `Pos` converts `expect.iterator.toSeq` and then `e.toVector` on failure instead of converting to `Vector` once.
  4. **`Forall.Pos`, `Forall.Failed`, and `Exists.Neg` (lines 525–555, 598–614):** `Forall.Pos` slices `ko.init` and `ko.last`, and `Failed` immediately rebuilds `val values = valuesI :+ valuesL` (and `Exists.Neg` does this a second time).
  5. **`ContainsOnly` (lines 256–296):** `ContainsOnly.missing` is unused dead code copied from `ContainsAll`, forcing an unnecessary `UnivEq[A]` constraint on `ContainsOnly` and `Dsl.FocusColl.AssertOps.containsOnly`.
  6. **`ElemChanges.Matched` (line 504):** Defined as `case class Matched[A]()` instead of `case object Matched extends Failure[Nothing]` like all other parameterless failure types in `CollectionAssertions`.

---

## 36. `StdlibUtil.NamedMap` missing companion object for implicit conversion and key name in `get`, and swapped method names in `ExtCats`

* **Locations:**
  * `util/shared/src/main/scala/teststate/util/StdlibUtil.scala` (lines 138–161)
  * `ext-cats/shared/src/main/scala/teststate/ExtCats.scala` (lines 29–39)
* **Current Code:**
  ```scala
  // StdlibUtil.scala
  final class NamedMap[K, +V](namePlural: String, val underlying: Map[K, V]) {
    ...
    def get(k: K): NamedOption[V] =
      new NamedOption(namePlural, underlying.get(k))
  }
  implicit def NamedMapToMap[K, V](n: NamedMap[K, V]): Map[K, V] = n.underlying

  // ExtCats.scala
  implicit def catsNatTransFromTestState[F[_], G[_]](implicit t: F ~> G): T.~~>[F, G] =
    new T.~~>[F, G] { def apply[A](fa: => F[A]) = t(fa) }

  implicit def catsNatTransToTestState[F[_], G[_]](implicit t: T.~~>[F, G]): F ~> G =
    new (F ~> G) { def apply[A](fa: F[A]) = t(fa) }
  ```
* **Issue:**
  1. `NamedOption` and `NamedVector` place their implicit conversions in `object NamedOption` and `object NamedVector`, whereas `NamedMap` has no companion object and `NamedMapToMap` is only in `object StdlibUtil` (not in `trait StdlibUtil` / `teststate.util.Exports`). Also, `NamedMap.get(k)` passes `namePlural` instead of `s"$namePlural[$k]"` to `NamedOption`, losing the key `k` in error messages, and `TestStateMapExt.named` calls a redundant `self.toMap`.
  2. In `ExtCats`, `catsNatTransFromTestState` and `catsNatTransToTestState` have their names swapped relative to `catsEqualFromTestState` / `catsEqualToTestState` and `catsProfunctorFromTestState`.
* **Proposed Change:**
  Add `object NamedMap { implicit def toMap[K, V](n: NamedMap[K, V]): Map[K, V] = n.underlying }`, pass `s"$namePlural[$k]"` in `NamedMap.get(k)`, and fix/alias the method names in `ExtCats`.

---

## 37. Build configuration (`project/Lib.scala`, `project/Build.scala`), example resource cleanup, and minor dead-code / API polish

* **Locations:**
  * `project/Lib.scala` (lines 47–61)
  * `project/Build.scala` (lines 59–67, 76–77, 147, 179, 218, 274)
  * `example-selenium/src/test/scala/teststate/example/selenium/SeleniumExample.scala` (lines 37–48)
  * `example-selenium/src/test/scala/teststate/example/selenium/SeleniumExample2.scala` (lines 48–51, 73–84)
  * `ext-selenium/src/main/scala/teststate/selenium/TabSupport.scala` (lines 31–93)
  * `core/shared/src/main/scala/teststate/run/Runner.scala` (lines 37–40)
  * `core/shared/src/main/scala/teststate/core/CheckOps.scala` (lines 46–49, 81)
  * `core/shared/src/main/scala/teststate/Exports.scala` (lines 73–78)
  * `core/shared/src/main/scala/teststate/run/Test.scala` (lines 22, 56–59, 114–116, 157–158)
  * `core/js/src/main/scala/teststate/typeclass/Platform.scala` (lines 7–10)
* **Issues & Proposed Changes:**
  1. **Broken Scala.js source map URLs (`project/Lib.scala:55`):** `p.base.toURI.toString.replaceFirst("[^/]+/?$", "")` strips only one trailing path segment from `p.base`. For all `crossProject` JS modules (`coreJS`, `domZipperJS`, `extCatsJS`, `extNyayaJS`, `utilJS`), `p.base` is `<root>/<module>/js` (two levels deep), producing 404 GitHub URLs missing the `<module>/` directory. Use `(LocalRootProject / baseDirectory).value.toURI.toString`.
  2. **Obsolete macro settings & duplicate `JSDOMNodeJSEnv` (`project/Build.scala`):** `addMacroParadisePlugin` (`-Ymacro-annotations`) was only needed for Monocle 2's `@Lenses` and is unused with Monocle 3; `jsEnv := new JSDOMNodeJSEnv` is already set in `testSettings.js` and duplicated in `domZipper`, `domZipperSizzle`, and `extScalaJsReact`.
  3. **Resource leaks and partial match in `example-selenium`:** Wrap `driver.quit()`, `mb.close()`, and `ec.shutdown()` in `try ... finally` blocks, and use `.collect` instead of `.map` on `resultStats` regex matching in `SeleniumExample2.scala:49`.
  4. **Deduplicate `TabSupport.Chrome` and `TabSupport.Firefox` (`TabSupport.scala:71–93`):** Move `_open()` and `_close()` (using `driver.executeJsOrThrow`) into `TabSupport.Typical` as default implementations.
  5. **Minor dead code & API polish:**
     * Remove unused `Runner.notRightRight` (`Runner.scala:37–40`) and `CheckOps.EitherOsExt` (`CheckOps.scala:46–49`).
     * Export `RunnableTest` in `teststate.Exports` (`Exports.scala:73–78`), add `mapO` to `Plan` and `PlanWithInitialState` (`Test.scala`), and change `PlanLike.executionModel` from `val` to `def` (`Test.scala:22`).
     * Extend `Product with Serializable` in `Failure` (`Failure.scala:3`), and use `System.err.println` instead of `System.err.print` in `core/js/.../Platform.scala:9`.
