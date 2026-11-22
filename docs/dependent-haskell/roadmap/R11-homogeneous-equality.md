# R11. Homogeneous equality

R11 changes primitive equality evidence in Core. The upstream design
requires the endpoints of a coercion variable to have the same kind. A
coercion expression may still relate endpoints of different kinds. The
kind coercion for such an expression is computed from its structure.
Source `~`, `~~`, `:~:`, and `:~~:` keep their current meanings.
[Serokell R11](https://ghc.serokell.io/dh#homogeneous-equality),
[GHC phase 2a](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell/phase2#phase-2a-homogeneous-equality),
[open GHC issue #17368](https://gitlab.haskell.org/ghc/ghc/-/work_items/17368).

## An equality with the empty vector

An ordinary `HRefl` pattern can refine the length of a retained vector.
For an unknown length the following function returns the reversed proof.
The first match also refines the element type.

```haskell
reverseZero :: foreach (xs :: Vec a n) -> a :~: Bool ->
    xs :~~: ('VNil :: Vec a 'Z) -> ('VNil :: Vec Bool 'Z) :~~: xs
reverseZero _ Refl HRefl = HRefl
```

If the input already has a successor length, the same `HRefl` pattern
would require `'Z` and `'S n` to be equal. With
`-Werror=inaccessible-code`, GHC reports GHC-40564 at that pattern.
A proof about `xs` cannot establish equality for a second vector `ys`
of the same length. GHC reports GHC-83865 at the attempted `HRefl`.
[Source and diagnostic checks](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh)
and [equality representation](../REFERENCES.md#transport-a-retained-vector-index).

### Two equalities in one retained package

The imported package retains vectors `xs :: Vec Bool n` and
`ys :: Vec Bool m`. The payload carries proofs `xs :~~: 'VNil` and
`ys :~~: xs`, followed by `Carrier ys`. The `Carrier` index has
nominal role.

```haskell
unpack :: Linked n m -> Carrier ('VNil :: Vec Bool 'Z)
unpack (Linked (Pair _xs (Pair _ys (first, second, payload)))) =
    case first of
        HRefl -> case second of
            HRefl -> payload
```

The first match relates the kind of `xs` to `Vec Bool 'Z` and
relates `xs` to `'VNil`. The second relates the kind of `ys` to the
kind of `xs` and relates their indices. The solver can recover a
nominal coercion from each scoped homogeneous Given. It composes
the two expressions when the shared retained index and its kind
match. This lets the client return the payload at `'VNil`.

The previous solver could recover either equality directly. It
rejected this transport at the RHS `payload` with GHC-83865. The
client now compiles from the provider interface at `-O0` and `-O2`
with Core and STG Lint. Omitting either match still fails. Returning
`Carrier zs` for an independent vector of length zero gives
GHC-83865 at the RHS `payload`.
[Retained package harness](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh),
[solver evidence](../../../compiler/GHC/Tc/Solver/Irred.hs),
[Cockx and Devriese, §3.1, Example 14](https://jesper.cx/files/proof-relevant-unification.pdf#page=13),
[Weirich et al., §6.2](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf#page=22).

## Compatibility example

The roadmap card gives no source example. Its linked GHC design uses this
kind-indexed GADT to show why heterogeneous source equality and named kind
coercions are needed. The function and driver exercise both constructors.

```haskell
{-# LANGUAGE GHC2024 #-}
module Main where

import Data.Kind (Type)

data Rep :: forall k. k -> Type where
  RepBool :: Rep Bool
  RepMaybe :: Rep Maybe

name :: Rep a -> String
name RepBool = "Bool"
name RepMaybe = "Maybe"

main :: IO ()
main = putStrLn (name RepBool ++ ", " ++ name RepMaybe)
```

This prints `Bool, Maybe`. The same program must continue to compile
after the change to Core equality. A second check observes the kind
equality recovered by a heterogeneous constraint.

```haskell
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module KindProof where

import Data.Type.Equality

kindFromConstraint
  :: forall k1 k2 (a :: k1) (b :: k2). a ~~ b => k1 :~: k2
kindFromConstraint = Refl
```

The v69 compiler elaborated the second example using `heq_sel` to
bind one primitive coercion and `Kind co` to recover its kind equality.
The v79 worker carries separate evidence for the kind equality and
for the index equality after transport. Rebuild installed native
providers when moving to v79. Even an ordinary exported function can
refer to the old selector in its object code.
[Shared rebuild and upgrade checks](../CHECKPOINTS.md#shared-r11-and-r16-rebuild).

## Related source features

- GHC documents homogeneous lifted `~`, heterogeneous lifted `~~`, and
  internal heterogeneous `~#`. Accepted proposal #371 made `~` an
  importable operator. Its review and #460 discuss the migration cost of
  requiring `TypeOperators` and providing a warning period.
  [User’s Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/equality_constraints.html),
  [#371 review](https://github.com/ghc-proposals/ghc-proposals/pull/371#issuecomment-710682895),
  [#460 review](https://github.com/ghc-proposals/ghc-proposals/pull/460#issuecomment-974044052).
- Accepted proposal #378 adds a design criterion for dependent types. Its
  author kept the detailed sketch out of the specification so features could
  receive separate review. Accepted #547 excludes written equality contexts
  from kinds and from promoted constructors. Its review explicitly preserves
  promotion of a GADT constructor with an indexed result.
  [#378 review](https://github.com/ghc-proposals/ghc-proposals/pull/378#issuecomment-818931447),
  [#547 review](https://github.com/ghc-proposals/ghc-proposals/pull/547#discussion_r1012616139).
- Agda’s `_≡_` takes two terms of one type, with `refl` at the same term
  on both sides. GHC’s `:~:` at an existing promoted kind is the source
  analogue. The 2017 Dependent Haskell calculus motivates homogeneous
  assumptions through substitutivity and reconstructs heterogeneous evidence
  by combining kind equality with equality after transport. The 2019 calculus
  develops the role-sensitive case.
  [Agda equality](https://agda.readthedocs.io/en/latest/language/built-ins.html#equality),
  [ICFP 2017, §6.2](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf#page=21),
  [ICFP 2019](https://richarde.dev/papers/2019/dep-roles/dep-roles.pdf).
- The separate R10 issue #14180 uses `MatchInt ('I# x) = '()`. The public compiler
  rejected it and the wildcard variant with GHC-18872. The source annotation
  `(x :: Int#)` yielded GHC-83865. A concrete `'Packed 3#` reached literal
  promotion and failed with GHC-51819.
  [R10](https://ghc.serokell.io/dh#promote-unboxed-types),
  [#14180](https://gitlab.haskell.org/ghc/ghc/-/work_items/14180).

## Wanted equality predicates

The typechecker stores the role and endpoints when it creates a Wanted
from them. Classification can return an `EqPred` directly from those
fields. The typed entry point still accepts a predicate type and keeps
its exact spelling for diagnostics or callers such as typechecker plugins.
The typed projection and coercion-hole destination still use the current
primitive equality type. Rewriting updates the predicate and that
destination together. Zonking can traverse the endpoints of a structured
equality and leaves the evidence destination for evidence zonking.
[Solver evidence](../../../compiler/GHC/Tc/Types/Constraint.hs),
[!2857 invariant review](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2857#note_258029).

The existing `GivenCt` and `WantedCt` records retain their typed fields.
The bundled `CtGiven` and `CtWanted` patterns expose those records.
The solver reads stored equality endpoints from the enclosing evidence.
The Wanted record reads its predicate type from the coercion hole when
a caller needs it. The typed and structured paths therefore share the
existing evidence and elaborated Core.

## Homogeneous Given predicates

GHC projects a primitive equality from a Given `~` or `Coercible`
dictionary. Both endpoints have the kind quantified by that class. The
solver stores the projected equality's role and endpoints.
The evidence variable keeps the selector's original result type and
binding. Predicate updates keep the structured form while the endpoints
have the same kind. Zonking visits both endpoints.

The `~~` selector still creates a typed Given. The generic solver and
plugin entry points also retain the supplied predicate type. The solver
therefore still has typed Given evidence when it encounters a
heterogeneous primitive equality. The remaining migration must replace
that evidence without binding a heterogeneous coercion variable.
[Equality dictionary solver](../../../compiler/GHC/Tc/Solver/Dict.hs),
[!2857 discussion](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2857#note_258029).

Structured Given construction checks the bound `CoVar` itself. The
variable must have nominal or representational primitive equality type
with both endpoints of the same kind. The typed record and the stored
endpoints use that variable's type. The solver applies this check to the
kind equality it derives from heterogeneous Givens as well as to
projections from homogeneous equality dictionaries. The heterogeneous
source evidence still supplies the proof for deriving its kind equality.
[Equality solver](../../../compiler/GHC/Tc/Solver/Equality.hs).

## Heterogeneous Given binders

This source program uses both parts of the proof from `a ~~ b`.
The indices are independently bound. The nominal role on `Index`
requires an equality between the indices as well as between their kinds.

```haskell
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Transport where

import Data.Type.Equality

type role Index nominal
data Index (a :: k) = Index Int

transport :: forall k l (a :: k) (b :: l). a ~~ b => Index a -> Index b
transport x = x
```

After desugaring, `transport` matches the result of `heq_sel` and binds
`h :: (a :: k) ~# (b :: l)`. Its cast uses both `Kind h` and `h`.
A Core plugin that requires same-kind endpoints on every expression
CoVar binder rejects this program. The ordinary compiler accepts it with
Core Lint. Changing its context to `k ~ l` fails with GHC-25897 because
the index equality remains unsolved.

The `:~~:` constructor already supplies two binders at a pattern match.
The kind binder has type `l ~# k`. The index binder still has type
`(b :: l) ~# (a :: k)`. The solver can derive transported homogeneous
evidence from those binders. The match itself still binds a heterogeneous
CoVar. Two independent `HRefl` matches each need their own kind and
index proof. Cross-using them also fails with GHC-25897.
[Constructor equality construction](../../../compiler/GHC/Tc/TyCl.hs),
[Core constructor representation](../../../compiler/GHC/Core/DataCon.hs).

### Scoped evidence representation

The `HEq#` candidate changes the dictionary producer and consumer
together. The worker binds an existential `ck :: k ~# l` followed by
`cv :: (a |> ck) ~# b`. The old worker had one field of heterogeneous
primitive equality and its selector was `heq_sel`.
[Wired-in `HEq#`](../../../compiler/GHC/Builtin/WiredIn/Types.hs),
[phase 2a design](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell/phase2#problem-of-defining-heterogeneous-equality-based-on-homogeneous-equality).

Each projection matches both fields in its own case. The index
projection returns a proof at the kind cast from the earlier projection.
The inner kind proof and the earlier kind proof have the same type,
so proof irrelevance makes either cast suitable for the index field.
The index evidence type records its dependency on the kind proof.
The evidence desugarer sorts the bindings into scope order.
[Evidence bindings](../../../compiler/GHC/Tc/Types/Evidence.hs),
[evidence desugaring](../../../compiler/GHC/HsToCore/Binds.hs),
[!2857 evidence review](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2857#note_260738).

Boxed Wanteds and heterogeneous pattern synonyms construct the new pair.
A quantified-constraint lookup composes the transport from `ck` with
`cv` when it needs a heterogeneous expression. The solver exposes
each primitive Given through its current typed record. Code using the
exported plugin API to construct
raw heterogeneous primitive predicates still depends on a typed
`PredType` and needs a migration before the primitive type changes.
[Equality boxing](../../../compiler/GHC/Tc/Solver/Dict.hs),
[pattern synonym evidence](../../../compiler/GHC/Tc/TyCl/PatSyn.hs),
[plugin entry points](../../../compiler/GHC/Tc/Plugin.hs).

The relinked candidate accepts `Transport` with Core Lint and a Core pass
that rejects mixed-kind binders. The pass counts three such binders in
the combined dictionary and witness test. All three come from existing
`HRefl` matches. Before the change the same test has six. The relink also
passes an inferred `HRefl` pattern synonym, a mixed-kind quantified
lookup, and deferred-error controls. The real `GHC.Internal.Types` module
emits the new worker without `heq_sel`. These checks used the existing
package interfaces during compiler and library source testing. A clean
normal build is required for the interface version change.

The library declares `HRefl` as an ordinary GADT constructor.
`rejigConRes` records its equalities in `EqSpec`. For a term data
constructor with a supported EqSpec telescope, the worker binds source
existential type variables first. It then binds the proof prefix in dependency
order and extends the prefix through any transported index needed by a
later kind. A later index field uses nominal lifting over the kind
to transport one endpoint. Construction supplies the proofs in worker
order. Matching brings the same proofs into scope before the index
Givens.

This rule covers `RepBool`, `RepMaybe`, the imported `HRefl` after a
library rebuild, and concrete representation refinements such as
`RawInt :: RawEq Int#` with `RawEq :: forall r. TYPE r -> Type`.
An indexed field can use more than one earlier proof. For example,
an index of kind `Either k l` can use separate refinements of `k`
and `l`. A variable right index keeps the cast on the left endpoint.
A constructor application on the right uses the inverse cast on that
endpoint so the solver retains the original left variable.

The source context dictionaries follow the proof fields. Ordinary
fields come last. The wrapper skips the transported equality fields
before handling dictionaries and ordinary field unboxers. The
pattern checker sees the visible fields. GHCi skips the erased
evidence and hides the dictionaries in normal printing.

The interface retains the source EqSpecs and records the worker
binders. Version 73 first gated the ordered layout, including boxed
`HEq#`. Version 74 also covers workers with ordinary fields. Version
75 records any earlier transported proofs needed by later fields.
Version 76 keeps source existential type variables before the worker
proofs. Version 77 extends the same layout to contextual constructors
such as `C :: Measure a => a -> E Bool`. The compiler and its packages
must use matching layouts.
[GADT evidence](../../../compiler/GHC/Core/DataCon.hs),
[interface version](../../../compiler/GHC/Settings/Constants.hs).

Pattern synonym inference restores the source endpoints before
checking the provided signature for escaping coercions. It uses the
instantiated source existentials when an endpoint refers to them.
The matcher can combine its local kind and index proofs to supply the
boxed source equality. The source signature contains neither worker
binder. A lazy pattern supplies no evidence to the continuation.
The v71 normal build exposed this distinction for `pattern Same =
HRefl` with the rebuilt Equality module.
[Pattern synonym inference](../../../compiler/GHC/Tc/TyCl/PatSyn.hs).

A provided `a ~~ Bool` can produce a nominal Given with the cast
on `a`. If the Given has homogeneous endpoints and `a` can be a
canonical left-hand side, the solver moves the inverse cast to
`Bool`. It emits the kind Given supplied by the cast. This avoids
binding an intermediate heterogeneous proof in the use and builder
of an inferred synonym for `RepBool`.

The solver can follow a filled metavariable on the right before
zonking the kind in the cast. If the kinds appear different, it
tries the comparison again with zonked kinds. It keeps the original
cast as a coercion expression in the transported Given. The strict
Core Lint prototype accepts T11711 and its reduced `TRFun` matcher at
`-O0` and `-O2`.

For a casted nominal Given with the same injective type constructor on
both sides, the solver selects arguments from the composed coercion.
It binds each selected argument's kind proof before its transported
index proof. The original `'Tag a ~~ 'Tag Int` case and the reversed
and nested cases have no mixed-kind Core binders in the focused audit.
The solver still uses `KindCo` on the selector expression when needed.
The same transport applies to selectors from ordinary constructor and
function Givens. It preserves either nominal or representational role.
For example, a `Coercible (Box a) (Box b)` can select an equality
between indices at different kinds.

A nominal Given between matchable applications `f a` and `g b`
also transports the selected head and argument proofs before binding
them. The solver continues with the head equality first. The Core
Lint prototype accepts this case and its reversed orientation.
[Equality solver](../../../compiler/GHC/Tc/Solver/Equality.hs).

For `Chain :: Chained ('Pin :: At 'True)`, the representation binds
the kind proof and then the transported proof for the index `a`.
The final proof field uses both to transport its kind `At a`.
Core Lint accepts the generated order and rejects moving a later
proof out of the scope of its dependency.

Constructors outside the supported EqSpec shape still use the
existing lowering. The current `promoteCoercion` falls back to `KindCo` for a mixed-kind `CoVarCo`. General EqSpec
lowering and heterogeneous Wanted evidence must support transport
before every CoVar can require same-kind endpoints.
[Coercion promotion](../../../compiler/GHC/Core/Coercion.hs),
[!2857 invariant review](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2857#note_258029).

### An index with a quantified kind

An ordinary GADT can refine a function index whose kind itself depends on
an earlier index.

```haskell
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, RankNTypes, RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications #-}
module QuantifiedGadtIndex where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type Slot :: forall (a :: Bool). Proxy a -> Type
data Slot p = Slot

type Pick :: forall k. (forall (a :: k). Proxy a -> Type) -> Type
data Pick f where
  PickBool :: Pick @Bool Slot

type Carrier :: forall k. (forall (a :: k). Proxy a -> Type) -> Type
type role Carrier nominal
data Carrier f = Carrier Int

transport :: forall k (f :: forall (a :: k). Proxy a -> Type).
  Pick @k f -> Carrier @k f -> Carrier @Bool Slot
transport PickBool x = x
```

Outside the match, `x` has type `Carrier @k f`. The `PickBool` pattern
first gives `k ~ Bool`. It then identifies `f` with `Slot` at the
transported kind, so the branch can return `x` at `Carrier @Bool Slot`.
The nominal role on `Carrier` requires both equalities. The worker lifts
the kind proof through the `forall` with `ForAllCo`. It uses the inverse
of that lifted proof to cast `Slot` to the kind of `f` before binding the
index equality. The same rule applies when the index kind is written as
a synonym, such as `IndexKind k` below.

```haskell
-- With Modifiers and import GHC.Exts (M).
type IndexKind k = forall (a :: k). @M Proxy a %M -> Type
```

The independent-index check fails at the final `x` in the following file,
line 11, column 18. GHC-25897 reports `Could not deduce 'g ~ Slot'`.

```haskell
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module QuantifiedGadtIndexWrong where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import QuantifiedGadtIndex

bad :: forall k (f :: forall (a :: k). Proxy a -> Type)
                (g :: forall (a :: k). Proxy a -> Type).
  Pick @k f -> Carrier @k g -> Carrier @Bool Slot
bad PickBool x = x
```

The v79 compiler accepted `Pick` at source and printed `41` without
Core Lint. With Core Lint, desugaring reported the mixed-kind coercion
binder `f ~# Slot` in the `PickBool` alternative on the `transport` equation.
A relink with the scoped worker passed Core and STG Lint and printed
`(41,43)` through a separate client at `-O0` and `-O2`. The client
uses the exported `transport` and performs its own `PickBool` match.
Both provider interfaces omit interface pragmas. GHC rejects the
independent index at the same source position with the scoped worker.
[Runnable provider](../examples/r11-heq-pair/QuantifiedGadtIndex.hs),
[Showcase](../Showcase.hs),
[maintained source and diagnostic checks](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

A v79 reader panics in `extendTCvSubst` when the new worker layout has
a v79 header. It reconstructs the index evidence from the old worker
rules. The `IndexKind k` alias remains visible in the stripped
interface, so checking for a `forall` in the written kind would miss
some constructors that need the new layout. Version 80 uses the common
worker-layout gate. GHC-47808 points to the import declaration when a v79
reader opens the v80 interface. The diagnostic names the expected and actual
versions. A v80 provider and client passed both optimization levels
with a component compiler that could read the installed v79 libraries.
A normal build must rebuild the compiler and its libraries for v80.
The [two-compiler check](../examples/r11-heq-pair/check-quantified-gadt-upgrade.sh)
uses each compiler with its own libraries. It checks an import in both
directions and rebuilds an ordinary module from source. The next build must
reuse its interface.
Rebuild providers and clients together with that compiler.
[Worker evidence](../../../compiler/GHC/Core/DataCon.hs),
[interface version](../../../compiler/GHC/Settings/Constants.hs).

Agda's `Square` example shows how a matched constructor refines an outer
index. The Agda unification model treats the equation types as a telescope.
Its vector example makes the vector equality depend on the preceding length
equality. Weirich et al. sequence the kind and index equalities in `Heq`.
Their `An-PiCong` rule transports the bound variable in the right body
when its kind changes.
[Agda manual, Dot patterns](https://agda.readthedocs.io/en/latest/language/function-definitions.html#dot-patterns),
[Cockx and Devriese, §3.1, Example 14](https://jesper.cx/files/proof-relevant-unification.pdf#page=13),
[Weirich et al., Figure 8](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf#page=16),
[Weirich et al., §6.2](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf#page=22).

### Checked constructor fragment

The Core fragment builds a dictionary worker with the same four parameters
as `HEq#` and the two dependent proof fields. Its consumer has this Core
shape, where `GRefl a ck` witnesses the change in kind and `cv` proves
equality at the transported kind.

```text
case dictionary of
  HEqPair# (ck :: k ~# l) (cv :: (a |> ck) ~# b) ->
    x |> Sub (Index ck (GRefl a ck ; cv))
```

The worker representation includes `ck` as an existential CoVar. The
earlier wrapper-free constructor builder used the source wrapper type
and dropped that binder from the worker type. The model uses the
`DCR` worker path. The production `HEq#` worker uses the representation
telescope directly, without a wrapper.
[Data constructor construction](../../../compiler/GHC/Core/DataCon.hs).

Core Lint checks a `Coercion` argument against the worker's CoVar
`ForAllTy`, then substitutes it into the type of the following field.
It makes the same substitution for the first binder of the `DataAlt`.
The producer, consumer and round trip pass those checks. A one-proof
producer, a one-binder case, a proof outside the case scope, and a
representational kind proof fail. See Note [Dependent coercion arguments].
[Core Lint](../../../compiler/GHC/Core/Lint.hs),
[checked fragment](../examples/r11-heq-pair/PairCorePlugin.hs).

Run the fragment from `docs/dependent-haskell/examples/r11-heq-pair` with
the compiler built with the lint changes.

```sh
ghc --make -O0 -dynamic -package ghc -dcore-lint -no-link \
  -outputdir "$(mktemp -d)" TransportPair.hs
```

An old client interface with inline dictionary functions contains a
one-proof `HEq#` application and a call to `heq_sel`. The new worker
requires two proof arguments, and the new case requires two binders.
Rebuild the compiler, its libraries and dependent interfaces together
when changing the wired dictionary. The lint prerequisite alone has the
same module ABI hash. The relinked compiler compiled against the old
interface, linked the old object and ran the resulting program.

The remaining solver work lets a heterogeneous predicate have no
corresponding primitive type. The solver must track the endpoints, role
and scope of a heterogeneous hole without a heterogeneous `CoVar`.
Filling a heterogeneous hole with a deferred error now binds a nominal
kind proof and a homogeneous index proof at the original role. The
hole holds the index proof composed with the inverse cast. Both
bindings are unlifted. An invalid boxed dictionary triggers the
deferred error even when its consumer ignores it. The direct and
compound boxed equality probes pass strict Core Lint at `-O0` and
`-O2`. The hole itself still records the primitive predicate.
Run a matched normal build with the strict Core Lint check before
enforcing homogeneous CoVars globally.
[Coercion holes](https://gitlab.haskell.org/ghc/ghc/-/work_items/17368#note_229123),
[deferred type errors](https://gitlab.haskell.org/ghc/ghc/-/work_items/17368#note_229140).

## Closed evaluation at a known GADT index

The
[closed evaluator's indexed constructor path](R15-closed-term-evaluator.md#m129-constructors-at-a-known-gadt-index)
supports a constructor with two source equality specifications. The
two Boolean indices in `Flat 'True 'False` already have ground kinds.
The evaluator can read back a `FlatTF` with a `Natural` field and can
decode a written promoted `'FlatTF` operand.

```haskell
data Mixed :: forall k. k -> Type where
    MixedTrue :: Natural -> Mixed ('True :: Bool)
```

The source constructor also has two equality specifications.
Construction and matching pass in ordinary code. A nested closed call
can make a `MixedTrue` and extract its scalar field. Reading back the
constructor or decoding a written promoted `'MixedTrue` also works
when the call fixes both arguments. The check substitutes these
arguments before deciding whether the declared parameter kinds are
ground. A polymorphic call still needs to determine the arguments.

The declarations and applied kinds are the same on both compilers.
R11 moves the kind proof into the worker's existential telescope.
The worker arity counts three arguments, including that proof.
`dataConRepArgTys` lists only the transported index proof and value
field. The closed evaluator binds the worker telescope when building
a case. It restores the source equalities before making casts for
indices. For a result indexed by `'Just b`, the source equality can
recover `b` and permit readback of a field of type `Field b`. A field
whose constructor-local index has no path through source equalities
still prevents readback. The wire carries the constructor and fields.

## Verification

The capped GHC 10.1.20261115.1 relink rebuilt the affected compiler
modules in the vanilla and dynamic ways. At `-O0` and `-O2`, the
`Rep`, kind equality, and deferred-error programs passed with Core Lint.
Their desugared Core with suppressed uniques and their diagnostics were
byte-identical to the `c24dcec367` comparison compiler. The negative
cases `'True :~: Int` and `Refl :: 'True :~: 'False` retained their
GHC-83865 diagnostics. The deferred-error program reached its first
output and raised the same exception when it forced the cast.

Ten of twelve stock solver, zonking, deferred-error and plugin checks
passed on both compilers. `T17139` had the same fork-specific
matchability spelling in its diagnostic. `T26395` had the same four
linker warnings in its output. The failure output was identical on both
compilers.

The Given increment also passed Core Lint for functions with `(~)` and
`Coercible` constraints at `-O0` and `-O2`. Their desugared Core retained
`eq_sel` and `coercible_sel` and matched the Wanted increment byte for
byte. The original Core, diagnostic and stock comparisons also matched.
The typed plugin entry points and the representational deferred-error
probe produced byte-identical diagnostics and output throughout.

Three alternating `-O0` compilations of `SuperRecord.hs` with each
compiler measured allocation under the same RTS flags. Median allocation
increased by 34,736 bytes (0.0014%) for the Wanted increment against its
unmodified control. It increased by 259,584 bytes (0.0102%) for the Given
increment against the Wanted increment. Storing the role and endpoints
directly changed the median by −432 bytes (−0.000017%) against the Given
increment. The Core, diagnostic and stock results matched that increment.

The compatibility increment restored compilation of a plugin that
constructs and matches both typed evidence records by field name.
A separate client retained both legacy record updates and their
GHC-02256 warnings. An endpoint probe classified a structured Wanted
after its location and rewriter set changed. Accessing the same
constraint's typed view forced the coercion-hole type.
The bridge increased median allocation by 75,472 bytes (0.0030%)
against the direct endpoint representation.
The full compatible stack increased median allocation by 385,952 bytes
(0.0152%) against the unmodified compiler built from `50c06eab01`.

The CoVar guard accepted homogeneous nominal and representational
variables and rejected heterogeneous variables at both roles and a
term binder. The solver trace confirmed that the `~~` probe derived kind
equality. Core, the `:~~:` witness, legacy and typed plugin checks,
deferred errors, and the stock checks matched the compatible compiler.
The guard increased median allocation by 36,072 bytes (0.0014%) against
the compatible stack.

The matched `552f43bab4` and `f80b6071c3` compilers gave the same results
for the boxed equality, `HRefl`, cyclic Given, and independent-witness
program at `-O0` and `-O2`. The binder audit at `-O0` counted six mixed-kind
nominal binders. Its enforcement mode rejected them. The `(~)` and
`Coercible` control passed enforcement with zero mixed-kind binders and
reported a homogeneous representational binder. The kind-only,
cross-witness, and representational-to-nominal negative controls all
failed with identical GHC-25897 diagnostics.

A typechecker plugin using the legacy `GivenCt` fields observed 28
primitive Given records after canonicalization in the heterogeneous
fixture. All were nominal and homogeneous and agreed with their bound
variable type. The separate homogeneous control also exposed coherent
representational Given records. The Core binder audit inspected the
earlier evidence that supplies those canonical predicates.
