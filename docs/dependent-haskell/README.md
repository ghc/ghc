# Dependent Haskell in GHC

This fork lets a Haskell function name a runtime argument in its result
type. The compiler tracks that argument through Core and generates native
code. Enable `ExperimentalDependentTypes` to write the new syntax. An
ordinary Haskell module can import and call a checked function.

```haskell
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
safeHead        :: Vec a (1 + n) -> a
```

A caller can take `safeHead (replicateNative (1 + n) x)` even when `n`
comes from IO. The compiler knows the vector has a first cell. The same
call on a vector of length zero fails to typecheck. For an ordinary list
of unknown length, `fromListExact 1 xs` checks the length at runtime.
The `Just` branch gets a `Vec a 1` and can use `safeHead` directly.
[Vector library](examples/packed-vector/PackedVecLibrary.hs) and
[working examples](Showcase.hs).

## Run the showcase

From the repository root, run the checked example with this fork’s compiler
and its matching libraries.

```sh
bash docs/dependent-haskell/check-showcase.sh _build/stage1/bin/ghc _build/stage1/lib
```

The runner prints the observed values, compares them with the expected
output, and checks the generated HIE files. Add `--ghci` to compare
the same program in an interpreter-enabled build. The runner uses a
temporary directory for the examples’ objects and interfaces.
[Showcase source](Showcase.hs) and [expected output](Showcase.stdout).

### Try a retained function in GHCi

Run `_build/stage1/bin/ghc --interactive` from the repository root.

```text
:set -fbyte-code -O0 -fomit-interface-pragmas -fno-write-if-simplified-core
:load *docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs
:seti -XExperimentalDependentTypes -XDataKinds -XTypeOperators
let fill :: foreach (n :: Nat) -> Vec Bool (1 + n); fill n = replicateNative (1 + n) True
toList (fill 2)
[True,True,True]
```

## Where to look

| Question | Entry point |
| --- | --- |
| What runs on the fork? Which features are still private? | [Verified checkpoints and current queue](ROADMAP.md#where-this-fork-stands) |
| How far along is the whole program? | [All 50 Serokell cards and their dependencies](ROADMAP.md#upstream-checklist-and-complete-dependency-index) |
| What can I write in Haskell? | [Extension guide](../users_guide/exts/experimental_dependent_types.rst), [indexed vectors](examples/packed-vector/README.md), [higher-order functions](examples/qualified-pi/README.md) |
| What can an ordinary module prove? | [Interface and compile-time evidence](BOUNDARY-EVIDENCE.md) |
| How does the compiler represent a retained value? | [Core contract](CORE.md) and [Eisenberg branch port](PORT.md) |
| Which designs and papers did we use? | [Reference ledger](REFERENCES.md) and [paper notes](papers/README.md) |
| How was an older result checked? | [Implementation log](PLAN.md) and [historical verification records](CHECKPOINTS.md) |
