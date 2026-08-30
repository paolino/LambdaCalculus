# Data model

Issue: #1. Output ceiling: 40 lines. Actual: measured at dispatch.

## D1 — Fresh-name allocation state

- Available names: ordered caller-supplied candidates.
- Consumed names: candidates selected earlier during the traversal.
- Avoided names: substitution target, replacement free variables, and all
  terminal/binder names in the active body.

Invariant: selection returns the first available candidate outside avoided and
consumed names, then makes that candidate unavailable to nested traversal.

## D2 — Conformance case

- Stable case identifier.
- Category.
- Whether a conflicting-binder path is required.
- Input lambda expression.
- Frozen oracle input and normal form in de Bruijn syntax.

Invariant: all 282 expected identifiers occur exactly once and each serialized
input agrees with the frozen oracle bridge input before outputs are compared.

## D3 — Conformance result

- Case identifier.
- Production normal form in de Bruijn syntax.
- Oracle normal form.
- Conflict-path count where required.
- Verdict: agreement, mismatch, or indeterminate.

Invariant: only agreement is accepted; missing data and indeterminate outcomes
are failures. The deliberate mutation must produce at least one mismatch.
