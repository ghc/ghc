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

The discussion that led to #177 asks how a client can establish a
domain from a GADT constructor or a quantified constraint. Looking up
a top-level instance alone misses both cases. The ordinary test client
uses both sources. Its local binding can form `Elem (f a)` under
`forall x. Allowed x => Collects (f x)` and `Allowed a`. Removing
the premise reports GHC-39999 at `Elem` for the marked provider.
A complete expression annotation can use the same evidence. GHC also
checks the written family before it infers a wildcard in an expression
or binding signature. Thus `Refl :: Elem [Int] :~: _` reports the
missing `Allowed Int` at `Elem`, including under `-fdefer-type-errors`.
A nested annotation in the body of a partially signed function can still
lack the function's quantified premise during inference.
[Client](../../../testsuite/tests/dependent/should_run/NativeConstrainedFamilyClient.hs).

The same discussion asks whether coverage
and termination should be separate requirements. The authors disagreed
about the consequences for closed family reduction.
[GADT evidence](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-408569969),
[termination](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-409628107)
and [the reply](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-409688377).

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

The discussion that led to #177 asks how a client can establish a
domain from a GADT constructor or a quantified constraint. Looking up
a top-level instance alone misses both cases. The local GADT example
below exercises the first. The same discussion asks whether coverage
and termination should be separate requirements. The authors disagreed
about the consequences for closed family reduction.
[GADT evidence](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-408569969),
[termination](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-409628107)
and [the reply](https://github.com/ghc-proposals/ghc-proposals/pull/154#issuecomment-409688377).

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

A separate proposal, #390, considers declaration-level controls for
termination and coverage checks. In review, Eisenberg objected to using
a pragma whose removal changes whether a program typechecks. The author
revised the draft to use modifiers. This discussion bears on the spelling
of a per-family opt-in if R24 needs one. Proposal #390 is open and
does not prescribe a domain annotation for associated families.
[Review](https://github.com/ghc-proposals/ghc-proposals/pull/390#issuecomment-756446022)
and [the author's revision](https://github.com/ghc-proposals/ghc-proposals/pull/390#issuecomment-756606415).

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

A client can use the marked family in `id @(Elem [Bool]) True` and
in a required argument such as `required (type (Elem [Bool])) False`.
Both check the domain before applying the function. An argument
`Elem [Int]` reports GHC-39999 even when the result discards it. A
wildcard in `Elem [_]` must supply enough information to select its
domain. A GADT match or a quantified constraint can supply local
evidence for a specified argument.
[Ordinary client](../examples/open-evaluation/ConstrainedElementsClient.hs)
and [required type arguments, proposal #281](https://ghc-proposals.readthedocs.io/en/latest/proposals/0281-visible-forall.html).

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

The family must mention every class parameter. A parameter can occur as an
invisible kind argument. The family can take extra arguments and return a
type of any kind. For example, `Echo (x :: k) :: k` in class `KnownIndex k`
requires `KnownIndex (Maybe a)` when applied to a runtime `x :: Maybe a`.
The Showcase uses `Allowed a` to satisfy that instance. It checks a
`Just False`, a `Nothing`, and a proof whose index is left unevaluated.

A partial application in a written value signature can determine the
whole domain. For example, `Echo :: Maybe Bool %U -> Maybe Bool` already
supplies the class parameter. Bare `Elem` still needs the parameter of
`Collects c`. R23 also checks the matchability and the reader's permission
to write a partial application. Each marked instance needs an equation
or default. The draft of #177 gives both a kind-indexed class and an
associated family that omits a class parameter. It also considers
functional dependencies and superclass equalities for recovering a missing
parameter. The fork currently requires the parameter to occur among the
family binders. Eisenberg's review gives an existing signature that an
unconditional domain check would reject. The fork keeps the producer
opt-in for these examples.
[Draft #177, §3](https://github.com/ghc-proposals/ghc-proposals/blob/faf45d2290f06696627b1f5d60ce92741ae9e38b/proposals/0000-simple-constrained-type-families.rst),
[compatibility review](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-430274548)
and [proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html).

A marked family in a written argument must have a domain even if its
enclosing synonym ignores that argument. For
`type Ignore a = forall b. Show b => b -> String`, a signature using
`Ignore (Elem [Int])` reports GHC-39999 for `Allowed Int`.
Generated newtype or via equations still need separate formation rules.
[Source checks](../../../testsuite/tests/dependent/should_run/NativeRetainedFamilyDomain.hs).

### Domains in type synonyms

The compiler checks a marked family when a synonym is defined. For
`type E a = Elem [a]`, it reports the missing `Allowed a` at the
definition. A qualified synonym can state the premise. The Showcase
exports a proof type that uses this rule.

```haskell
type Reflection a = Allowed a => foreach (x :: Maybe a) -> Echo x :~: x
```

An ordinary module can use `forall a. Reflection a` as a callback type.
It also imports `MaybeBoolEcho`, a synonym for the partial family at the
kind `Maybe Bool %U -> Maybe Bool`, and proves the result at `'Nothing`.

The reader substitutes its arguments into the family's domain predicate.
An imported synonym can use a general instance in its defining module.
A more specific instance in the client can require an additional premise.
In the regression test, `KeyElement [Bool]` passes and `KeyElement [Int]`
reports GHC-39999 for `Allowed Int`, even though both family applications
reduce to `Bool`. The producer source is absent during the client check.
This check instantiates the source formation rule from section 4.2 of
Morris and Eisenberg. The partial application also follows the
right-hand-side arity rule in proposal #242.
[Formation rule](https://richarde.dev/papers/2017/partiality/partiality.pdf),
[proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#rhs-of-type-synonyms)
and [separate compilation test](../../../testsuite/tests/dependent/should_run/NativeConstrainedFamily.sh).

### Family domains under a retained binder

```haskell
applyField :: foreach (tag :: Bool) -> Collects (FieldKey tag)
           => (Elem (FieldKey tag) -> String) -> Elem (FieldKey tag) -> String
applyField _ render value = render value
```

The class predicate refers to the runtime argument. A separate ordinary
client reads the flag and matches on `field tag`. A successful match
provides the dictionary, the element and its renderer. The client calls
`applyField tag render value` using that dictionary. The Showcase produces
`(Just "x",Nothing)` for the enabled and disabled flags.

A predicate for flag `a` cannot justify an element for flag `b`. A
predicate to the right of an arrow cannot justify a family application
in its argument. Both signatures report GHC-39999, including with deferred
term errors.

A callback can also bring its own domain. The ordinary client supplies
a formatter to `withField`. The callback accepts any `c :: Type` with a
`Collects c` instance. It receives that field's element and renderer.
Its local `Collects c` checks the family applications inside the callback.
Leaving it out fails at the signature. Moving a use outside the callback
requires independent evidence there.

[Provider](../examples/open-evaluation/ConstrainedElements.hs),
[ordinary client](../examples/open-evaluation/ConstrainedElementsClient.hs)
and [scope regression](../../../testsuite/tests/dependent/should_run/NativeRetainedFamilyDomain.hs).

### Required type arguments

An ordinary module can write this signature with `RequiredTypeArguments`.

```haskell
renderKnown :: forall (c :: Type) -> Collects c => (Elem c -> String) -> Elem c -> String
renderKnown (type _) render value = render value
```

The application `renderKnown (type [Bool]) show True` returns `"True"`.
The class predicate supplies the family domain in the function signature.
GHC checks the class instance at the call. The type argument is erased.
The same scope rule works with multiple required binders, including
`forall k -> forall (c :: k) -> Collects c => Proxy (Elem c)`.
[Ordinary implementation](../examples/open-evaluation/ConstrainedElementsClient.hs)
and [visible forall proposal](https://ghc-proposals.readthedocs.io/en/latest/proposals/0281-visible-forall.html).

### Associated newtype representation

The review of #177 asks where an associated newtype could store the
dictionary for its parent class. The private compiler applies the domain
check to associated type synonyms. The following class contains both a
type synonym family and a data family.

```haskell
class Collect c where
    type Elem c
    data Wrapped c

instance Show a => Collect [a] where
    type Elem [a] = a
    newtype Wrapped [a] = Wrap a
```

An ordinary client imported the stripped interface with the constructor
in scope. Stock GHC 9.14.1 and the private compiler both accepted
`Coercion :: Coercion (Wrapped [a]) a`. The client wrapped and called
a function at `-O0` and `-O2`. On both compilers, matching `Wrap x`
and calling `show x` still requires `Show a` in the client signature.
Stock GHC accepts `Refl :: Elem [a] :~: a` without that constraint.
The private compiler reports GHC-39999 at `Elem` when the provider
opts in. Extending domain evidence to pattern matches on associated
data instances would need a separate representation decision.
[Review question](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549766671).

### Associated applications in type declaration bodies

An ordinary provider allows both `type S = Elem [Int]` and an equation
`F () = Elem [Int]`. A separately compiled term can use either wrapper
to prove equality with `Int`, even though there is no `Allowed Int`
instance. With a marked provider, GHC rejects both right-hand sides
before the wrappers can be exported. Switching an existing provider to marked mode
also rechecks the wrappers. The check still rejects a right-hand side
when the evidence is available, for example for `Elem [Bool]`. The
current experiment checks evidence in term and constructor signatures.

Draft #177 §3.3 gives a forwarding equation `S a = T a` with a
standalone kind signature `forall a -> C a => Type`. Both stock GHC
9.14.1 and the fork reject that kind signature and a qualified family
result kind with GHC-01259. Accepted proposal #547 §1.2 disallows
constraints in both positions. The 2019 review also asks how the draft
accounts for #259's restriction on associated kind signatures. A qualifier
in an ordinary term type
synonym is legal in stock GHC. The fork checks a marked family in
that synonym's right-hand side using its written premises.
[Draft #177](https://github.com/typedrat/ghc-proposals/blob/faf45d2290f06696627b1f5d60ce92741ae9e38b/proposals/0000-simple-constrained-type-families.rst)
[Eisenberg's 2019 review](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549766671),
and [proposal #547](https://ghc-proposals.readthedocs.io/en/latest/proposals/0547-no-kind-equalities.html#proposed-change-specification).

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
at the signature when `Allowed Int` is absent, including under
`AllowAmbiguousTypes` or `-fdefer-type-errors`. Formation discards the
dictionary. If GHC deferred the error, the family axiom could still
reduce the equality, and a downstream module could evaluate `missing`
as `Refl`. A type error in a term with a well-formed signature still
defers normally.
[GHC deferred type errors](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/defer_type_errors.html)
and [stripped-interface check](../../../testsuite/tests/dependent/should_run/NativeConstrainedFamily.sh).
