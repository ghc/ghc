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

## Source syntax and separate compilation

GHC proposal #177, *Simple constrained type families*, keeps the
declaration `type Elem c` inside `class Collects c`. The class supplies
the domain. A client must establish `Collects c` to form `Elem c`, even
when it only mentions the application under `Proxy`. The draft also shows
how a separate family that calls an associated family could state the
constraint in a standalone kind signature.
[Proposal text at the reviewed revision](https://github.com/typedrat/ghc-proposals/blob/faf45d2290f06696627b1f5d60ce92741ae9e38b/proposals/0000-simple-constrained-type-families.rst).

The draft considers a switch for domain checking. It rejects that
approach because a library would need two builds. Its migration plan
applies the domain rule to all producers. A temporary extension would
infer missing constraints in existing signatures and warn. The draft
suggests the name `InferKindConstraints`. On 2026-11-20 the
PR was open with the label *Needs revision*. The latest discussion on
the PR was on 2019-11-05.
[PR #177](https://github.com/ghc-proposals/ghc-proposals/pull/177)
and [Eisenberg's review](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549766671).

Eisenberg asks how a caller would select the missing argument in a
multi-parameter class. For example, if `class C a b` declares `type F a`,
the application `F Int` supplies no `b` for the constraint `C Int b`.
He also asks how an associated `newtype` could store a dictionary
alongside its only field. These are separate tests for extending the
single-parameter `Collects` experiment. Simon Peyton Jones questions the
cost of changing constructor elaboration before the dependent-type
design is settled.
[Eisenberg's questions](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549766671)
and [Peyton Jones's reply](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549934585).

The accepted proposal #547 disallows `=>` in family result kind
annotations and standalone kind signatures. Class constraints were
already forbidden there. It leaves room for dependent types to revisit
this rule. The spelling `type Elem c :: Collects c => Type` would
require a new language decision.
[Proposal #547](https://ghc-proposals.readthedocs.io/en/latest/proposals/0547-no-kind-equalities.html).

The first experiment uses #177's plain declaration in an opted-in
producer. Its interface records whether clients must discharge the
domain constraint. Check both an ordinary client of that producer and
an ordinary producer that exports the same source declaration. This
producer opt-in is a local experiment. Keep the choice of migration
policy separate from the tests of type formation.

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
