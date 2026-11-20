# R24. Constrained type families

R24 is open on this fork. Serokell leaves the card unmarked and gives
it no prerequisites. R07 (promote literals), R12 (promote classes) and
R20 (clean up the language) depend on R24. The first check uses a fully
applied family. R23 becomes relevant when a partial application must
carry its domain requirement to a later call.
[Serokell card](https://ghc.serokell.io/dh#constrained-type-families)
and [local dependencies](../ROADMAP.md#upstream-checklist-and-complete-dependency-index).

## Current behavior

Both declarations compile with ordinary `TypeFamilies`.

```haskell
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Example where

import Data.Proxy (Proxy(..))

class Allowed a
instance Allowed Bool

class Collects c where
    type Elem c

instance Allowed a => Collects [a] where
    type Elem [a] = a

stuck :: Proxy (Elem Int)
stuck = Proxy

reduced :: Proxy (Elem [Int])
reduced = (Proxy :: Proxy Int)
```

`Elem Int` forms a type without an instance or equation. `Elem [Int]`
reduces using the associated equation even though its class instance
requires `Allowed Int`, which is absent. A term helper with signature
`Collects c => Proxy c -> Proxy (Elem c)` checks the class constraint.
It accepts `[Bool]` or a local `Collects c` assumption. Calling it at
`[Int]` gives GHC-39999. That qualification applies only to calls to the helper.

## Evidence and next check

Section 4.2 of *Constrained Type Families* requires the source context
to entail the family’s domain predicate at type formation. Section 6
uses a separate core calculus where erased equality witnesses attest
that a family application evaluates to a proper type. That calculus
omits kinds, classes and partial applications.
[Morris and Eisenberg, 2017](https://richarde.dev/papers/2017/partiality/partiality.pdf).

- [ ] Give `Elem` the declared domain `Collects c`. Compile the provider
  separately and check a client using only its stripped interface.
  The client can form `Elem [Bool]` and use `Elem c` under a local
  `Collects c` assumption. It reports the unsatisfied domain at
  `Proxy (Elem [Int])`, where the current axiom can reduce the family.
  The interface must retain the predicate and its mapping to family
  arguments so the reader can make the same check.
