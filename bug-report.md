# Bug and Defect Report for `test-state`

This report details bugs, defects, and problems identified across the `test-state` codebase, ordered roughly by severity.

---

## 17. Additional Minor Defects

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

5. **`TodoComponent.scala` (lines 42–46, 82–86) — Wrong item index completed in example React app when `showCompleted` is `false`:**
   In `example-react`, `render` filters `s.items` with `s.items.iterator.filterNot(_.completed)` *before* `renderList` calls `.zipWithIndex` to bind `completeItem(idx)`. When completed items are hidden (`showCompleted == false`) and an earlier item in `s.items` is already completed, `idx` is the index in the filtered pending list rather than in `s.items`, so clicking "Complete" marks the wrong item in `s.items`.
