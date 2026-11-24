# Project Suggestions, Improvements & Optimisations

This document contains numbered suggestions, bug fixes, improvements, and optimisations identified across the `test-state` codebase.

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
