# Functions model

Issue: #1. Output ceiling: 35 lines. Actual: measured at dispatch.

| ID | Function | Signature / interface | Contract |
|---|---|---|---|
| F1 | `withFreshes` | `[a] -> Freshes a b -> b` | Preserve the public runner while initializing traversal-state allocation. |
| F2 | `captures` | `Eq a => Expr a -> Replace a -> Freshes a (Expr a)` | Substitute without changing shadowed targets and allocate collision-free binders. |
| F3 | internal name collector / allocator | Implementation-local, polymorphic over `Eq a` | Expose no new public API; collect the active avoidance set and consume exactly one valid candidate per rename. |
| F4 | conformance entry point | `main :: IO ()` | Support full and focused execution, reject the deliberate mutation, and exit non-zero for every non-agreement. |

Existing signatures for `application`, `reduction`, `beta`, and `betas` remain
unchanged. No test-only hook is added to production exports.
