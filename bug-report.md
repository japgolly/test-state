# Bug and Defect Report for `test-state`

This report details bugs, defects, and problems identified across the `test-state` codebase, ordered roughly by severity.

---

## 17. Additional Minor Defects

5. **`TodoComponent.scala` (lines 42–46, 82–86) — Wrong item index completed in example React app when `showCompleted` is `false`:**
   In `example-react`, `render` filters `s.items` with `s.items.iterator.filterNot(_.completed)` *before* `renderList` calls `.zipWithIndex` to bind `completeItem(idx)`. When completed items are hidden (`showCompleted == false`) and an earlier item in `s.items` is already completed, `idx` is the index in the filtered pending list rather than in `s.items`, so clicking "Complete" marks the wrong item in `s.items`.
