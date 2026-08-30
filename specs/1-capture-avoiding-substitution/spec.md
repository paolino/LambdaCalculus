# Capture-avoiding substitution conformance

Issue: #1. Parent epic: #2.

Output ceiling: 70 lines. Actual: measured at dispatch.

## User story

When a lambda term is reduced, substitution preserves the original binding
structure even when replacement free variables conflict with several nested
binders.

## Requirements

- R1: `Lambda.beta` and `Lambda.betas` must never reuse a generated binder
  name that is already in scope or was selected earlier in the same traversal.
- R2: The public `withFreshes`, `beta`, `betas`, and tactic interfaces remain
  source-compatible.
- R3: A native command at `test/run-conformance` must exercise the production
  reducer over the frozen 282-case zoo and report exactly 282 agreements,
  zero mismatches, and zero indeterminate outcomes.
- R4: The zoo must include the minimal nested-capture counterexample and all
  11 cases refuted by the pinned Lean oracle evidence.
- R5: The conformance command must prove its comparator can fail by running a
  deliberate output mutation before accepting the clean result.
- R6: GitHub Actions must invoke `test/run-conformance`; a stub-only job is not
  acceptable.
- R7: No wasm toolchain is required by the native conformance command.

## Invariants

- INV-1-CAPTURE (ADVISORY): generated binder names are distinct from every
  name that can capture or be captured at that traversal point; the 282 frozen
  proof-oracle normal forms all agree up to de Bruijn canonicalization.
- INV-1-WIRING (ADVISORY): the committed native conformance command runs both
  a known-failing negative control and the clean production comparison, and CI
  invokes that command.

## Observable success

- `(λx.λy.λz. x y z) (y z)` reduces to a term alpha-equivalent to
  `λa.λb. y z a b`.
- `test/run-conformance` exits zero and prints the clean 282/282 summary plus
  evidence that the deliberate mutation was rejected.
- The workflow no longer contains a stub-only build gate and invokes the same
  native command.

## Rejection behavior

Missing cases, missing oracle rows, mismatched input bridges, reducer/oracle
disagreement, a vacuous negative control, or an indeterminate evaluation all
make the native command exit non-zero.

## Non-goals

- No general rewrite of the 2016-era reducer, parser, UI, or wasm build.
- No broad test-coverage campaign or legacy warning cleanup.
- No Lean toolchain in the browser package or ordinary native test run.
- No changes to tactic selection semantics.
