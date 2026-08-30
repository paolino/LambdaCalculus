# Tasks

Issue: #1. Output ceiling: 30 lines. Actual: measured at dispatch.

## S1 — Repair capture avoidance and make it continuously observable

- [x] T001 Add a native regression/conformance surface for the minimal case,
  the 11 known mismatches, and all 282 frozen oracle cases.
- [x] T002 Demonstrate focused RED on the current reducer and demonstrate that
  the comparator's deliberate mutation is rejected.
- [x] T003 Make fresh-name allocation consume selected names and exclude names
  already active in the traversed body while preserving public interfaces.
- [x] T004 Demonstrate focused and complete GREEN with 282 agreements, zero
  mismatches, and zero indeterminate outcomes.
- [x] T005 Replace stub-only CI with invocation of `test/run-conformance` in a
  native Haskell environment.

Final behavior commit trailer: `Tasks: T001, T002, T003, T004, T005`.

## S2 — Forward CI syntax correction

- [ ] T006 Encode the native conformance command as valid workflow YAML and
  observe a real GitHub Actions job pass.
