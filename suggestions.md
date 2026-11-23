# Project Suggestions, Improvements & Optimisations

This document contains numbered suggestions, bug fixes, improvements, and optimisations identified across the `test-state` codebase.

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
