# Dependent Haskell in GHC

This fork lets a Haskell function name a runtime argument in its result
type. The compiler tracks that argument through Core and generates native
code. Enable `ExperimentalDependentTypes` to write the new syntax. An
ordinary Haskell module can import and call a checked function.

The example library exports `replicateNative`. Its client defines
`safeHead` by matching the first cell.

```haskell
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
safeHead        :: Vec a (1 + n) -> a
safeHead (VCons x _) = x
```

A caller can take `safeHead (replicateNative (1 + n) x)` even when `n`
comes from IO. The compiler knows the vector has a first cell. The same
call on a vector of length zero fails to typecheck. For an ordinary list
of unknown length, `fromListExact 1 xs` checks the length at runtime.
The `Just` branch gets a `Vec a 1` and can use `safeHead` directly.
[Vector library](examples/packed-vector/PackedVecLibrary.hs) and
[working examples](Showcase.hs).

For the current compiler revision, test results and restart procedure, see
the [project handoff](HANDOFF.md).

## Reuse a proof with HaskLib

The `hasklib` Cabal package lives in this directory. Add this directory
to the `packages` field of your `cabal.project`, add `hasklib` to your
component's `build-depends`, and select this fork's compiler. Import
`HaskLib` for the list laws and `Refl`, or import `HaskLib.List`
for just the list API.

| Lemma | Length of the result |
| --- | --- |
| `lengthMap f xs` | `size xs` |
| `lengthReverse xs` | `size xs` |
| `lengthAppend xs ys` | `plus (size xs) (size ys)` |
| `lengthZip xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipWith f xs ys` | `minSize (size xs) (size ys)` |

`HaskLib.Size` exports `plusZero` and `plusSucc`. The
[append-singleton client](hasklib-test/Client.hs) imports both lemmas
and `lengthAppend` to prove that appending one element adds one cell.
It reuses the proofs without recursing over its input list.

An ordinary client can use the zip equality with lists read at runtime.
A dependent client can also name it in its signature.

```haskell
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Client where

import HaskLib

zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> size (zip xs ys) :~: minSize (size xs) (size ys)
zipLaw xs ys = lengthZip xs ys
```

The proofs use the ordinary definitions of `size`, `plus` and `minSize`.
Forcing a proof can traverse the list spines. The empty-left zip case
leaves the right list unevaluated. The list elements and the `zipWith`
callback can remain unevaluated throughout.
The reverse proof checks the accumulator used by this GHC's `reverse`.
HaskLib imports that worker from `ghc-internal`. Clients can use
`lengthReverse` with only `base` and `hasklib` in their dependencies.
[Package](hasklib.cabal), [list API](hasklib/HaskLib/List.hs) and
[proof methods](REFERENCES.md#parametricity-and-the-list-length-laws).

## Run the showcase

From the repository root, run the checked example with this fork’s compiler
and its matching libraries.

```sh
bash docs/dependent-haskell/check-showcase.sh _build/stage1/bin/ghc _build/stage1/lib
```

The runner prints the observed values, compares them with the expected
output, and checks the generated HIE files. Use `--default-o1` or
`--default-o2` to check that optimization level with default interfaces.
Use `-O1` or `-O2` for stripped interfaces. Add `--ghci` to compare the same
program in an interpreter-enabled build at `-O0`. The runner uses a
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
| What can an ordinary module prove? | [Length of a mapped list](examples/open-evaluation/PreludeMapLengthProof.hs) |
| How does the compiler represent a retained value? | [Core contract](CORE.md) and [Eisenberg branch port](PORT.md) |
| Which designs and papers did we use? | [Reference ledger](REFERENCES.md) and [paper notes](papers/README.md) |
| How was an older result checked? | [Implementation log](PLAN.md) and [historical verification records](CHECKPOINTS.md) |
