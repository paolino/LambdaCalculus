# Modules model

Issue: #1. Output ceiling: 35 lines. Actual: measured at dispatch.

| ID | Module / artifact | Responsibility | Depends on |
|---|---|---|---|
| M1 | `src/Lambda.hs` | Allocate fresh binders and perform production substitution/reduction without capture. | D1; F1-F3 |
| M2 | `test/Conformance.hs` and frozen oracle fixture | Construct the deterministic zoo, run the production reducer, canonicalize independently, compare with pinned oracle results, and expose focused/full modes. | D2-D3; F4 |
| M3 | `test/run-conformance` | Provide one native entry point that builds/runs M2 and proves the negative control before the clean result is accepted. | M1-M2 |
| M4 | `.github/workflows/ci.yml` | Provision native Haskell dependencies and invoke M3 on pushes and pull requests. | M3 |

Dependency direction is M4 → M3 → M2 → M1. Production code never depends on
test artifacts or the external Lean toolchain.

The test fixture is a frozen product of the investigation at TAPL Lean commit
`2f5f9e8edae4250e130f2b70c66014afafe0ce3e`; it is data, not executable
authority.
