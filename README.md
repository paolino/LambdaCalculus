# LambdaCalculus

A lambda calculus reducer with an interactive browser client, built on
[Reflex FRP](https://reflex-frp.org/).

Try it: [lambdasistemi.net/lambda-calculus/](https://lambdasistemi.net/lambda-calculus/)

Type or build an expression with the buttons, watch it beta-reduce step by
step under your choice of reduction strategy (aggressive / mild / normal),
and save any expression as a named button for reuse.

## Rebuilt on the GHC WASM backend

The original 2016 version of this app compiled via GHCJS and
[reflex-platform](https://github.com/reflex-frp/reflex-platform). That
toolchain — and reflex-platform's pinned `reflex 0.4` / `reflex-dom 0.3` —
have been retired for a decade; this version targets GHC's own
`wasm32-wasi` backend (stable since 9.6) with its
[JSFFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend)
feature for browser interop, using current
[reflex](https://hackage.haskell.org/package/reflex) /
[reflex-dom-core](https://hackage.haskell.org/package/reflex-dom-core) and
the experimental [jsaddle-wasm](https://github.com/amesgen/jsaddle-wasm)
bridge — following the pattern in
[haskell-wasm/ghc-wasm-reflex-examples](https://github.com/haskell-wasm/ghc-wasm-reflex-examples).

The lambda-calculus core — parsing (`src/Parser.hs`), beta-reduction with
capture-avoiding substitution (`src/Lambda.hs`), and alpha-equivalence-aware
pretty-printing (`src/PPrint.hs`) — is unchanged from 2016. Only the widget
layer (`src/Widgets.hs`, `src/App.hs`) and the build/deploy pipeline are new:
`Dynamic` now has real `Functor`/`Applicative` instances (the old
`combineDyn` shim in `Missing.hs` was a workaround for their absence), and
`reflex-dom-core` is `Text`-based throughout rather than `String`-based, so
`Text` appears at the widget boundary while the core logic stays on
`String`/`Expr Char` as before.

## Building

Toolchain: [`ghc-wasm-meta`](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta), GHC 9.12.

```sh
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12'
cd frontend
wasm32-wasi-cabal --project-dir=.. update
./build.sh                              # dev build -> frontend/dist/
./build.sh --low-memory-unused --converge -O4   # prod build (wizer + wasm-opt + strip)
```

Serve `frontend/dist/` over HTTP (module scripts need a real origin, not
`file://`) and open `index.html`.
