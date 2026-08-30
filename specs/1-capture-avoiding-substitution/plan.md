# Implementation plan

Issue: #1. Output ceiling: 55 lines. Actual: measured at dispatch.

## Constraints

- Keep the old public API stable and confine reducer behavior changes to
  capture avoidance.
- Reuse the frozen oracle outputs produced by the pinned Lean investigation;
  CI need not rebuild Lean.
- Treat the current test surface as untrusted: the new command must contain
  its own explicit negative control.
- Avoid unrelated formatting or modernization in brittle legacy modules.

## Strategy

1. Add the native conformance proof surface and demonstrate RED on the current
   reducer using the minimal counterexample and the frozen oracle corpus.
2. Replace read-only fresh-name lookup with traversal-state allocation. A
   selected name is consumed, and candidate selection excludes replacement
   free variables plus names already present in the active body.
3. Run the focused counterexample, the deliberate comparator mutation, and
   the complete clean 282-case command.
4. Replace the CI stub with a native GHC environment that invokes the same
   committed command without the wasm toolchain.

## Slice

There is one bisect-safe slice, S1, because the regression proof and freshness
repair describe one observable correction. The local RED and GREEN commits
remain unpushed provenance and are squashed after independent audit.

## Verification

- Focused RED/GREEN: `test/run-conformance --match capture-nested-two-open`.
- Full ticket gate: `./gate.sh`.
- CI wiring is checked mechanically by the gate and then by GitHub Actions.
- One fresh Codex auditor reviews only `origin/master..candidate`, the two
  declared invariants, and the named commands. Build budget: one building
  audit. No generalized coverage census or unrelated legacy audit.

## Live boundaries

GitHub Actions is the only remote boundary. Local acceptance proves workflow
wiring statically and the command dynamically; final readiness also requires
the PR check result.

## Forward slice S2 — workflow syntax correction

The first pushed implementation reached GitHub Actions but the workflow parser
created no job: the native command was encoded as an invalid YAML plain scalar
because its Nix expression contains `p:`. S2 changes only the workflow scalar
encoding. Its RED witness is `actionlint` on pushed commit `df8dfec`; its GREEN
requires `actionlint`, the unchanged native gate, and a real GitHub Actions job
to pass. Reducer and conformance semantics are frozen from S1.
