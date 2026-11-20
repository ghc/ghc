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

The review of literal promotion in #154 led to the separate proposal
#177. Peyton Jones asks which numeric domain applies to
`Proxy a -> Proxy b -> Proxy (a + b)`. Eisenberg asks how Core would
use a dictionary supplied by a GADT match or a quantified constraint.
An R24 test should therefore form a family application using evidence
that becomes available inside a pattern match.
[Arithmetic example](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-407925931)
and [local evidence](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-408569969).

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

Eisenberg's review of #177 also points to the accepted amendment #259.
In a class body, `type T :: a` already declares a nullary family with
result kind `a`. Using the same spelling for a standalone signature
would make that line ambiguous. The earlier PR #227 considered moving
the signature outside the class. The committee left associated families
out of standalone kind signatures. GHC 9.14.1 and this fork accept
the nullary family header above. Both report GHC-29916 for an additional
signature inside the class and GHC-44432 for one outside it.
[Associated types in #54](https://ghc-proposals.readthedocs.io/en/latest/proposals/0054-kind-signatures.html#associated-types),
[placement discussion in #227](https://github.com/ghc-proposals/ghc-proposals/pull/227#issuecomment-497878018)
and [accepted amendment #259](https://github.com/ghc-proposals/ghc-proposals/pull/259).

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

### Interaction with an existing dependent module

The producer experiment uses the same extension as R23. Consider one
module with both of these declarations:

```haskell
type family First a b where
    First a b = a
data Accepts (f :: Type %U -> Type) = Accepts
type UsesExperimental = Accepts (First Int)

class C a where
    type F a
data Witness a where
    Witness :: F a -> Witness a
```

The pre-R24 compiler accepts this module with
`ExperimentalDependentTypes`. The initial R24 prototype reported
GHC-90647 at the constructor field. GHC now checks the constructor's
own context. The field above reports GHC-39999 for the missing `C a`.
Writing `Witness :: C a => F a -> Witness a` supplies the evidence.
An equality in a GADT context can also determine a known instance.
With the extension disabled, both compilers report GHC-27346 at
`First Int`. A separate producer switch could preserve existing R23
modules during the experiment. Inferring a missing constraint could
also change a constructor's type and runtime representation.
[Eisenberg's compatibility example](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-430274548)
and [review of constructor dictionaries, inference and newtypes](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549766671).

## Evidence and next check

Section 4.2 of *Constrained Type Families* requires the source context
to entail the family’s domain predicate at type formation. Section 6
uses a separate core calculus where erased equality witnesses attest
that a family application evaluates to a proper type. That calculus
omits kinds, classes and partial applications.
[Morris and Eisenberg, 2017](https://richarde.dev/papers/2017/partiality/partiality.pdf).

- [x] Form the fully applied `Elem` under its parent `Collects` predicate.
  A plain declaration in an experimental producer records the domain
  in its interface. An ordinary reader accepts a local `Collects c`
  Given and the ground instance for `Elem [Bool]`. Its written
  `Elem [Int]` fails with the missing `Allowed Int` constraint before
  reduction can erase that obligation. The same client accepts the
  application from an ordinary producer. The producer marker changes
  the ABI and rechecks an existing client.
  [Provider](../examples/open-evaluation/ConstrainedElements.hs),
  [ordinary client](../examples/open-evaluation/ConstrainedElementsClient.hs)
  and [stripped-interface check](../../../testsuite/tests/dependent/should_run/NativeConstrainedFamily.sh).

The experiment covers fully applied associated type families with result
kind `Type` and the same binders as their owner class. It permits a
different binder order. Each instance supplies an equation or a default.
The check uses Givens in the written signature, Givens from enclosing
functions or GADT patterns, and the imported class instance environment.
Formation checks run before inference can add a missing constraint. General kinds, family occurrences
in aliases or visible type arguments, generated newtype or via equations,
and partial applications also need separate formation rules.

### Domain evidence and termination

An opted-in provider with `UndecidableInstances` can define
`type Element Bool = Loop Bool` for its `Collect Bool` instance,
where a closed family has the equation `Loop a = Loop a`. The provider
compiles. An ordinary client has the required dictionary. Checking
`formed = Proxy` against `Proxy (Element Bool)` still reaches GHC-40404
at reduction size 21 with `-freduction-depth=20`. The dictionary checks
membership in the declared domain. Termination of the equations needs a
separate argument. Proposal #177 discusses further changes to GHC's
assumptions about totality under *Effects and Interactions*.
[Proposal text](https://github.com/typedrat/ghc-proposals/blob/faf45d2290f06696627b1f5d60ce92741ae9e38b/proposals/0000-simple-constrained-type-families.rst).

### Deferred errors at a module boundary

An ordinary signature `missing :: Elem [Int] :~: Int` fails with GHC-39999
when `Allowed Int` is absent. It also fails with `AllowAmbiguousTypes`.
With `-fdefer-type-errors`, GHC reports the missing instance as a warning.
The existing family axiom reduces the equality, so `missing = Refl` runs.
A second module compiled with normal flags can read that interface and
evaluate `case missing of Refl -> True`. The producer's successful
compilation with deferred errors gives the consumer no evidence that
the producer discharged the domain obligation.
[GHC deferred type errors](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/defer_type_errors.html).
