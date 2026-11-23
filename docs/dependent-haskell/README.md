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

A prefix GADT can take a field before a required type argument.
For example, `ChoosePayload :: Bool -> forall a -> PayloadKey a` is a
`type data` constructor in the Showcase. Applying `ChoosePayload 'True Int`
selects a key for an `Int` payload.
[Ordered constructor example](roadmap/R41-nested-quantification-in-gadts.md#release-check).

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
| `lengthTakeSize n xs` | `minSize n (size xs)` for `takeSize n xs` |
| `lengthAppend xs ys` | `plus (size xs) (size ys)` |
| `lengthZip xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipWith f xs ys` | `minSize (size xs) (size ys)` |
| `lengthZipMap pre combine xs ys` | `minSize (size xs) (size ys)` for `zipWith combine (map pre xs) ys` |

An ordinary client can use the zip equality with lists read at runtime.
A dependent client can also name it in its signature.

```haskell
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Client where

import HaskLib

zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> size (zip xs ys) :~: minSize (size xs) (size ys)
zipLaw xs ys = lengthZip xs ys
```

This proof and the corresponding `zipWith` proof compile at ordinary
`-O2`. The compiler checks the installed zipper fusion rules against
the functions' equations. The proof involving `map` still disables
rewrite rules in its own module. [Zipper proofs](examples/open-evaluation/PreludeZipRuntime.hs)
and [map pipeline](examples/open-evaluation/PreludeZipMapRuntime.hs).

For a bounded prefix the result type records both inputs.

```haskell
lengthTakeSize :: forall a. foreach (n :: Size) -> foreach (xs :: [a])
               -> size (takeSize n xs) :~: minSize n (size xs)
```

At a zero bound an ordinary caller can obtain both the empty prefix
and its length witness without evaluating the input list. At a finite
bound the witness can also terminate on an infinite list.
[Proof and Agda precedent](REFERENCES.md#length-of-a-bounded-list-prefix).

The proofs use the ordinary definitions of `size`, `plus` and `minSize`.
Forcing a proof can traverse the list spines. The empty-left zip case
leaves the right list unevaluated. The list elements and the `zipWith`
callback can remain unevaluated throughout.
The prefix proof stops when it reaches the bound or the end of the list.
It can return a witness for a finite prefix of an infinite list.
The reverse proof checks the accumulator used by this GHC's `reverse`.
HaskLib imports that worker from `ghc-internal`. Clients can use
`lengthReverse` with only `base` and `hasklib` in their dependencies.
[Package](hasklib.cabal), [list API](hasklib/HaskLib/List.hs) and
[proof methods](REFERENCES.md#parametricity-and-the-list-length-laws).

The list laws use the ordinary equations of the functions in their
statements. On the current fork a client can import a rewrite rule that
changes `map` and still use an already compiled `lengthMap` witness.
[R16 example and measured outputs](roadmap/R16-open-term-evaluator.md#an-imported-proof-and-a-client-rewrite-rule).

The stock optimization rules can also distinguish the two sides of
`lengthMap`. On a filtered input, a mapper that captures the producer
kept the mapped count at `Zero`. At an independent call site the plain
count raised an exception. The published proof returned `Refl`. When
both counts shared one filtered list they both returned `Zero`.
[Runnable map example and optimization modes](roadmap/R16-open-term-evaluator.md#map-length-and-sharing-under-stock-fusion).

Stock `-O2` rules can also change the result for append. If a list
producer forces its seed and the right list is undefined, only the
addition side of `lengthAppend` yields an outer `Succ`.
The published proof returns `Refl` on this input.
[Append example and compiler results](roadmap/R16-open-term-evaluator.md#append-fusion-and-an-undefined-right-list).

### Compose equality proofs

Use `trans` from `HaskLib` to join two proofs with a shared endpoint.
The append-singleton client uses `plusSucc` to move the successor
outside `plus`. Then `congSucc` applies that successor to both sides
of `plusZero`. [Append-singleton proof](hasklib-test/Client.hs).

`withEquality proof body` brings the equality into scope for `body`.
The body can return an ordinary value.
In the Showcase, `sameNat count (S Z)` returns a witness when the
runtime count is one. Inside that branch, the vector has the length
that `safeHead` requires. The checker infers the zero-length tail from that equality.

```haskell
Just proof -> withEquality proof (Just (safeHead (replicateNative count 'x')))
```

The branch returns `Just 'x'`. [Value-returning example](Showcase.hs).

In the inductive step of `lengthMap`, the recursive call proves equal
lengths for the tails. Both whole lists have one more element, so
`congSucc` lifts the tail equality through `Succ`.

```haskell
lengthMap f = \case
    []     -> Refl
    _ : xs -> congSucc (lengthMap f xs)
```

Use `congFirst` to lift an equality through the first argument of a
binary size function. In the zip-after-map proof, `@minSize` selects
the function and `@(size ys)` fixes its second argument.

```haskell
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Client where

import HaskLib

zipMapLaw :: forall a b c d. foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
          -> foreach (xs :: [a]) -> foreach (ys :: [c])
          -> size (zipWith combine (map pre xs) ys) :~: minSize (size xs) (size ys)
zipMapLaw pre combine xs ys =
    lengthZipWith combine (map pre xs) ys
        `trans` congFirst @minSize @(size ys) (lengthMap pre xs)
```

HaskLib re-exports `trans` from `Data.Type.Equality`. It forces the
proofs from left to right.
[Congruence and transitivity references](REFERENCES.md#reusing-an-equality-under-a-size-function).

The map proof traverses the left list. For example, if the left list
is `True : undefined` and the right list is empty, forcing
`lengthZipMap` returns `Refl`. Forcing `zipMapLaw` reaches the undefined
tail. Both proofs typecheck. Their types describe the equality each
establishes when it returns. [Client source](hasklib-test/Client.hs)
and [direct proof](examples/open-evaluation/PreludeZipMapRuntime.hs).

### Pass a named measurement

The Showcase passes the ordinary `HaskLib.size` function to a retained
callback. Its premise says the measurement of the input list equals
`HaskLib.size xs`. Passing `size` makes these the same expression, so
`Refl` supplies the premise. The provider uses `lengthReverse` and
returns the measured size.

```haskell
case measureReverseLength HaskLib.size xs Refl of
    (n, Refl) -> n
```

The Showcase reads the list at runtime. It counts two cells and leaves
the elements unevaluated. [Runnable definition](Showcase.hs)
and [checked callback rules](roadmap/R03-dependent-products.md#pass-a-named-function-with-checked-equations).

### What polymorphism can prove

A function with `zipWith`'s type can return `[]` for every input.
HaskLib therefore proves the length law from the equations for the
installed function. Parametricity instead relates different element
types. For a function `r` with `filter`'s polymorphic type, one such
law relates filtering before and after a change of element type.
The standalone example states this law for an abstract function,
checks an induction for Prelude's `filter`, and shows how `seq` can
distinguish the two sides when a hypothesis is missing.
[Example](examples/open-evaluation/ParametricFilter.hs),
[run the controls](examples/open-evaluation/check-parametric-filter.sh)
and [the theorem and its hypotheses](REFERENCES.md#filter-naturality-in-the-presence-of-seq).

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
