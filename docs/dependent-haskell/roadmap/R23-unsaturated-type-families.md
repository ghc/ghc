# R23: Unsaturated type families

Upstream: [Unsaturated type families](https://ghc.serokell.io/dh#unsaturated-type-families).
Serokell marks the proposal as accepted. The fork records matchability
on ordinary arrows and forall binders. M166 lets a user pass an unapplied
family at a known unmatchable arrow. A receiver can also require the
whole trailing invisible telescope or a nonempty suffix of it.
Kind inference without an expected kind and ordinary-program
compatibility remain open.
Use the [work queue](../ROADMAP.md#current-work-queue) for priorities.

## Matchability on a source arrow

Import `U` from `GHC.Matchability` and enable `Modifiers` to write the
callback kind as `(a %U -> b) -> [a] -> [b]`. The R17 example supplies
a type lambda at that position. A separate module checks its result
from the interface. Changing the callback to `a %M -> b` rejects the
lambda because it is unmatchable. Both checks pass in the isolated
compiler. [Map source](../examples/open-evaluation/TypeLambda.hs) and
[proposal decisions](../REFERENCES.md#matchability-and-multiplicity-on-arrows).

## Compatibility with existing Haskell

In `type family PFrom (x :: a) :: Rep a`, the result kind can depend
on an inferred matchability argument to `Rep`. Fixing that argument
prematurely gives `SDecide (Rep k)` two different meanings in the
same program. Preserve the argument in the receiving family's
telescope. First solve the constraints that can determine its value.
An open family with such a parameter can independently need a
matchability selector for an arrow in its result.
[Original regression](../../../testsuite/tests/dependent/should_compile/T15346.hs)
and [upgrade and independent-index tests](../examples/closed-evaluation/check-readback-upgrade.sh).

An intermediate compiler (`00ba1e8d92`) could write an interface with
one selector for the parameter and none for the result. The producer
itself compiled. A later module that supplied a new open-family instance
then failed with GHC-83865. On upgrade, GHC rechecks open families used
as kinds whose old telescope can refer to an inferred selector. A family
with an explicit kind can have the same interface shape. Its ABI stays
the same when the new compiler infers the same kind. A second build
reuses the new interfaces.

In `T14174a`, the kind of `p` and its use as an argument to `App`
introduce two omitted indices of the same family application. All its
other arguments agree. GHC can choose the same index for these two
uses when it created both metavariables for this declaration. It then
rechecks the original equality. No equality between arbitrary inputs of
a noninjective family follows from equality of its results. Explicit
distinct indices and two unrelated implicit parameters retain their
separate meanings.
[Original program](../../../testsuite/tests/polykinds/T14174a.hs)
and [ordinary client and rigid-index controls](../../../testsuite/tests/dependent/should_run/NativeOpenMatchability.sh).

If a provider gains an inferred matchability parameter, an older client
can still refer to the same family by name. For a bare alias such as
`type Saved = K`, the interface records the kind at which the alias
was checked. The reader uses that kind to distinguish a valid
polymorphic alias from one that needs rebuilding. The diagnostic names
the referring module. [Compiler-upgrade test](../examples/closed-evaluation/check-readback-upgrade.sh).

The normal-way run of `testsuite/tests/dependent` after M112 found 36
unexpected results. Of 213 cases run, 177 passed. The compiler and
`base` rebuilt from the previous fork tip, `496701ecc8`, reproduced all
36 failures. Each test produced the same diagnostic text or golden diff
after removing temporary paths and diff-header timestamps.

Fifteen tests that should compile were rejected.
All fifteen report a mismatch involving arrow matchability. For example,
`T20875` expects the saturated type family `ToKind TRUE` to supply
the arrow kind of `Maybe`. `T12442` checks defunctionalization symbols.
These programs use existing Haskell syntax. Restore their acceptance and
keep rejecting actual incompatible matchability annotations.
[Family returning an arrow kind](../../../testsuite/tests/dependent/should_compile/T20875.hs),
[defunctionalization](../../../testsuite/tests/dependent/should_compile/T12442.hs)
and [reflection](../../../testsuite/tests/dependent/should_compile/RaeJobTalk.hs).

Three of those tests define the `singletons` arrow synonym
`type a ~> b = TyFun a b -> Type`. With the unmodified synonym, the
compiler reports GHC-83865 at the data declarations in `T12442`,
`T13910` and `T13938`. Changing the synonym's arrow to `%M ->`
in its defining module makes all three programs compile with Core
and STG Lint. The two-module `T13938` client needs no edits.
The unchanged sources remain compatibility failures. Proposal #242
predicts this break and gives U as the default for a synonym RHS.
[Source test](../../../testsuite/tests/dependent/should_run/NativePercentArrowOnlyModifiers.hs)
and [proposal rationale](../REFERENCES.md#matchability-and-multiplicity-on-arrows).

`GADTSingletons` has a second dependency on arrow matchability. Stock
GHC 9.14.1 accepts the original module. The fork at `7c873b5f46` already
rejects these three declarations with `DataKinds`, `GADTs`, `PolyKinds`
and `TypeOperators` enabled.

```haskell
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)
```

With the R23 compiler at `55c20e158c`, fixing the synonym to M makes
this reduction compile. Fixing it to U gives GHC-83865 at `TyCon1`.
In the full module, fixing the synonym to M exposes three further
kind errors in the `PromoteX` and `DemoteX` equations for `Either`.
[Full test](../../../testsuite/tests/dependent/should_compile/GADTSingletons.hs)
and [proposal 242 on synonym right-hand sides](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#rhs-of-type-synonyms).

The nested arrow in `DemoteX (a :: Type) = Demote a` needs two
independent matchability choices. One controls the returned `Demote a`.
The other controls its kind, `Demote Type`. With the former compiler,
an ordinary client could pass `Maybe` to the interpreted unary kind,
but it could not pass `Either` to the interpreted binary kind.
The independent parameters let the same interpreter form the two
matchable arrows of `Either` or the two runtime arrows of `(&&)`.
Its separately compiled client prints `(Right 7,False)` and leaves the
second Boolean operand unevaluated.
[Interpreter](../examples/open-evaluation/NestedFunctionKind.hs)
and [ordinary client](../examples/open-evaluation/NestedFunctionClient.hs).

In the full upstream test, two written annotations now suffice. The
`~>` synonym uses `%M`, and the `Promote` equation matches
`a %(m :: Matchability) -> b` for either `m`. The unchanged source
still reports GHC-83865 at its original bare synonym. An upgrade from
the 299-line compiler rebuilds the interpreter once. The next build
reuses its outputs. Its incremental provider and client interfaces
have the same ABI hashes as a clean build.
[Upgrade check, option `--family-forwarding`](../examples/closed-evaluation/check-readback-upgrade.sh).

In `T15874`, the original `type Varianced = forall (var :: Var). Type`
supplies an invisible U forall. The data-family instance requires M.
GHC-45219 names the synonym and both matchabilities, including when the
synonym came from a stripped interface. A provider that needs both uses
can declare the index explicitly:

```haskell
type Varianced :: forall (m :: Matchability). Type
type Varianced @m = forall (var :: Var). @m Type
```

A data-family result kind can then use bare `Varianced` and get M. An
ordinary term signature can use the same spelling and get U. This keeps
the matchabilities independent and preserves the distinct binder rules in
[proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#other-quantifiers).
[Original source](../../../testsuite/tests/polykinds/T15874.hs) and
[diagnostic and compatibility controls](../../../testsuite/tests/dependent/should_run/NativeOpenMatchability.sh).

The remaining seven polykinds failures in the 116/124 normal-way
comparison also compile after explicit source changes. The unmodified
files all report GHC-83865. Proposal #242 assigns U to arrows on a synonym
RHS. It generalizes matchabilities only in inferred kinds and never in
types. A later use cannot select a new index for a fixed synonym.
[Proposal #242 inference rules](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#inference).

| Source | Checked spelling |
| --- | --- |
| [T11523](../../../testsuite/tests/polykinds/T11523.hs), [T12055](../../../testsuite/tests/polykinds/T12055.hs) | `type Cat k = k %M -> k %M -> Type` |
| [T14515](../../../testsuite/tests/polykinds/T14515.hs) | In either synonym, `forall k. @M k %M -> ty` |
| [T14873](../../../testsuite/tests/polykinds/T14873.hs) | `type a ~> b = TyFun a b %M -> Type` |
| [T19093](../../../testsuite/tests/polykinds/T19093.hs) | `type PolyKinded res = forall (k :: Type). @M k %M -> res` |
| [T19094](../../../testsuite/tests/polykinds/T19094.hs) | `type PolyKinded (q :: Matchability) (a :: Matchability) res = forall (k :: Type). @q k %a -> res` |
| [T19250](../../../testsuite/tests/polykinds/T19250.hs) | `type Exp a = a %M -> Type` |

In `T19094`, use `PolyKinded U U (PolyKinded U U Bool)` for the
family and `PolyKinded M M (PolyKinded M M Constraint)` after the
class's initial `Bool ->`. Changing only the family's first visible arrow
to M still rejects, so the forall and arrow indices can be checked
separately. In `T12055`, `Cat Type` accepts `((->) @U)`: the two M
arrows classify the type constructor's arguments, while U describes
the runtime arrow it constructs.

A separate negative control used `type family Erase a :: Bool where
Erase a = 'True`. GHC-25897 rejected `Refl` at type
`(Erase a ~ Erase b) => a :~: b`. The source changes above preserve
that distinction between a type constructor and a family.

In `T20875`, the equation `ToKind TRUE = Type -> Type` checks its right
side as an ordinary type. Its arrow gets U. The type constructor `Maybe`
has an M arrow in its kind. Writing `FUN @M 'Many Type Type` in the
equation makes this example compile. Writing `FUN @U 'Many Type Type`
reproduces the rejection. An unannotated equation with the same right
side can also describe a runtime function, where `keep x = x` needs U.
Changing the default for every family equation would break that program.

Both uses can occur in one module. GHC 9.14.1 accepts the following
declarations together. The fork previously rejected `Maybe` at the
second equation.

```haskell
type family K (b :: Bool) :: Type
type instance K 'True = Type -> Type

type family Interpret (b :: Bool) :: K b
type instance Interpret 'True = Maybe

identity :: K 'True
identity x = x
```

The kind of `Maybe` needs M. The term `identity` needs U.
Both refer to the same `K 'True`. An explicit extra parameter on `K`
can distinguish these uses in the fork. That solution changes the
source interface. The fork now infers the selector and checks both uses.

For the U case, GHC-83865 now reports the disagreement between
`Matchable` and `Unmatchable` at `Maybe`. The original expected family
can hide the arrow from the diagnostic's argument count. A family that
reduces to `Type` can still produce the advice to supply another argument.
[Matchability control](../../../testsuite/tests/dependent/diagnostics/FamilyArrowMatchability.hs),
[arity control](../../../testsuite/tests/dependent/diagnostics/FamilyArrowArity.hs)
and [proposal #242 on annotation-free arrows](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html).

Of the 21 output mismatches, three successful compilations printed
different kinds. In sixteen compile-fail tests the substantive rejection
was preserved. Two other negative tests now fail in an earlier declaration.
`RAE_T32a` fails at the application of `MkStar`. `T13135` rejects `AST`
before it reaches the intended missing `Constr` instance. Fix the earlier
failures before reconsidering those goldens.
[RAE_T32a](../../../testsuite/tests/dependent/should_fail/RAE_T32a.hs)
and [T13135](../../../testsuite/tests/dependent/should_fail/T13135.hs).

### Constructor fields used at runtime and after promotion

Stock GHC 9.14.1 accepts this module. The fork at `73bffa1a15`
reports GHC-83865 at `'Tag`. The field of `Box` has a U arrow, and
the promoted constructor `'Tag` has an M arrow.

```haskell
{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeFamilies #-}
module Legacy where

data Tag = Tag Bool
type Callback a b = a -> b
data Box where
  Box :: Callback Bool Tag -> Box
type FamilyArgument = 'Box 'Tag
makeBox :: Box
makeBox = Box Tag
runCallback :: Box -> Tag
runCallback (Box f) = f True
```

Fixing the synonym arrow to `%M` lets the promoted constructor take
`'Tag`. It rejects `Box Tag` because the term constructor `Tag` has a U
arrow. A compiler prototype inferred a hidden matchability binder for
the synonym and still assigned U to the field at the constructor
declaration. The same field boundary occurs in
[`T12442` at the promoted `MkEff`](../../../testsuite/tests/dependent/should_compile/T12442.hs).

The fork accepts an explicit index on the constructor. The promoted use
chooses M. The term constructor builds a box with a U field.

```haskell
{-# LANGUAGE DataKinds, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions #-}
module Explicit where

import Data.Kind (Type)
import GHC.Exts (M, U, Matchability)

data Tag = Tag Bool
type Callback :: forall (m :: Matchability). Type -> Type -> Type
type Callback @m a b = a %m -> b
data Box (m :: Matchability) where
  Box :: forall (m :: Matchability). Callback @m Bool Tag -> Box m
type FamilyArgument = ('Box @M 'Tag :: Box M)
makeBox :: Box U
makeBox = Box Tag
runCallback :: Box U -> Tag
runCallback (Box f) = f True
```

Restoring the unchanged source would require a policy for preserving a
matchability choice through constructor fields and promotion. Proposal
#242 defaults bare synonym RHS arrows to U and gives an explicit
polymorphic constructor as a migration for promotion.
[Synonym RHS rule](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#rhs-of-type-synonyms),
[inference rules](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#inference)
and [promotion](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#promotion).

### Arrow patterns in family equations

GHC 9.14.1 accepts the legacy spelling of the two-module reduction.
The reproduction script removes the `%M` synonym annotation and the
separate M equation for that check. On `4bb020f702` and the solver
overlay at `f3833dbd90`, the bare `(~>)` gives GHC-83865 at
`data TyCon1`. With `%M` on the synonym, the provider compiles
and its `Refl` proof checks the U case. The separately compiled
ordinary client has only the provider interface. Its equation
`PromoteX Maybe = TyCon1 Maybe` gives GHC-83865 because
`Promote (Type %M -> Type)` stays stuck.
[Provider](../examples/open-evaluation/SingletonsArrowProvider.hs) and
[ordinary client](../examples/open-evaluation/SingletonsArrowClient.hs).

Keep the bare `Promote (a -> b)` equation for U. If the family has
no M case, the provider can add an explicit row with the same RHS.

```haskell
type instance Promote (a %M -> b) = PromoteX a ~> PromoteX b
```

The same compilers accept this split and a single
`(a %(m :: Matchability) -> b)` pattern. Both also accept the full
`GADTSingletons` with either migration. In the unchanged full program,
both reject four data declarations. With just `%M` on the synonym,
both reject the two `PromoteX` equations for `Either`. The nested
result selectors handle the corresponding `DemoteX` equations.
[Full source](../../../testsuite/tests/dependent/should_compile/GADTSingletons.hs) and
[reproduction script](../examples/open-evaluation/check-singletons-arrows.sh).

The reproduction script also compiles a separate module that adds
`Promote (a %M -> b) = Ordering`. This equation coexists with the
bare U equation. With the polymorphic provider pattern, the separate
module gives GHC-34447 because both equations now match M and return
different results. Generalizing bare patterns would break this valid
program even if inference applied only to families used as result kinds.

Proposal #242's M rule for application patterns concerns the kind of
the unknown head `f` in `f x`. The U examples for arrow types in
instance heads are class instances. The inference section restricts
automatic generalization to kinds. A rule that generalizes the
matchability in the written family pattern `(a -> b)` would need
its own specification.
[Application patterns](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#application-in-pattern-position),
[class instances](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#arrow-type-in-type-class-instances)
and [inference](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#inference).

In `T12088sg3`, the two saturated `TyCon2 (->)` patterns select
different arrows. After the synonym and `Promote` migration above,
the type pattern in the `Promote` equation selects U. The kind
annotation in the promoted `MkArr` pattern selects M. The client
reports GHC-83865. Stock GHC 9.14.1 accepts the bare pair. On
`4bb020f702` and `f3833dbd90`, the client compiles when both
selectors are U or both are M. Either mismatched pair still gives
GHC-83865. Changing either default would also make a currently
disjoint open equation conflict with GHC-34447.
[Original program](../../../testsuite/tests/dependent/should_compile/T12088sg3.hs).

### Applying a type at a family-defined arrow kind

`Wrap False` returns `Maybe`. `Wrap True` returns the unsaturated
family `AsList`. The kind family `Arrow` selects a matchable arrow for
the first and an unmatchable arrow for the second. One nominal step
exposes each arrow. GHC applies both results to `Int`. The ordinary
client prints `(Just 7,[7])`.
[Source example](../examples/open-evaluation/InvisibleFamily.hs).

While checking `Wrap True Int ~ [Int]`, the unifier used to demand
`AsList ~ []` before reducing the application. It now sends the whole
equality to the solver when either head can be unmatchable. The solver
can use the equation `AsList a = [a]` at the supplied argument.
Extracting an argument equality from a given equality still requires a
matchable head. [Equality controls](../../../testsuite/tests/dependent/diagnostics/DependentArrowMatchability.sh).

### An open family used after a declaration splice

The ordinary GHC program `KindEqualities2` declares an interpreter for
kinds before a Template Haskell splice. After the splice it declares
a family `I` and an equation `I (TApp a b) = (I a) (I b)`. The unchanged
program now compiles and prints `Nothing`. In the Showcase, two
applications of the optional constructor produce `Just (Just True)`.
[Upstream test](../../../testsuite/tests/dependent/should_compile/KindEqualities2.hs)
and [Showcase source](../examples/open-evaluation/InterpretedKind.hs).

Three checks matter. First, a bare result arrow in a local open-family
instance can determine the telescope published by its owner. The owner
does not need to see the later declaration of `I`. Second, when checking
a recursive equation, two fresh implicit arguments can share a choice
if that makes their otherwise identical family applications agree. GHC
checks the whole equality again. Third, when `I a` is checked as a value
type, an unresolved argument in its kind can default to M. During kind
inference the ordinary default remains U. Defaulting to M in both
positions broke the existing `SDecide (Rep k)` constraint in two
programs. Both now compile unchanged.
[Default signatures](../../../testsuite/tests/dependent/should_compile/T14720.hs)
and [visible type arguments](../../../testsuite/tests/dependent/should_compile/T15346.hs).

The optional code can also return an unsaturated family. Keep the
interpreter's arrow bare and give the same code a second equation.
The expected kind of `Maybe` selects M at its equation, and the
unsaturated callback selects U at its equation. A caller can omit the
annotation on the interpreter's arrow when it supplies the expected
arrow kind. `UseM (Interpret 'OptionalTag)` and
`UseU (Interpret 'OptionalTag)` produce `(Just 3,Right 3)` in the Showcase.
[Callback equation](../examples/open-evaluation/InterpretedKindCallback.hs)
and [ordinary client](../examples/open-evaluation/InterpretedKindUseSite.hs).

The fully applied `Interpret ('ApplyTag 'OptionalTag 'IntTag)` has kind
`Type` under both selectors. A value signature defaults to M here and
rejects `Right 3` at the body. When the interpreter has only the
unsaturated equation, an explicit `%U` on its arrow makes this fully
applied use select U. Giving the intermediate function an expected U
arrow kind also selects U and keeps both equations available.
[Stripped-interface and negative checks](../examples/open-evaluation/check-interpreted-kind.sh).

An upgrade can also change the meaning of a use whose provider is
binary-only. An ordinary module can export `get :: F AtBase`, where the
family's hidden selector occurs in its result kind. The old and new
defaults give this signature different ABIs. The upgrade check uses a
package that re-exports `F`, then compares the incremental client ABI
with a clean one. For a source provider that gains an argument, it
checks that the provider rebuilds, unrelated objects retain their
modification times, and the next build reuses the results.
[Upgrade check, option `--family-stages`](../examples/closed-evaluation/check-readback-upgrade.sh).

#### Associated type defaults

A class can supply the arrow before any instance exists. Its default
`type Callback a = a -> Maybe a` gives the promoted constructor `'Just`
the required kind in one use. In another use, the same default gives a
runtime callback its function type. The ordinary client returns
`Just True`. Fixing the arrow to U rejects the constructor. Fixing it
to M rejects the callback.
[Class default](../examples/open-evaluation/DefaultCallback.hs) and
[ordinary client](../examples/open-evaluation/LegacyKindArgument.hs).

Previous interfaces store the default in the class declaration. A
compiler upgrade must inspect that declaration even when the module
has no local instances. Both ordinary and boot interfaces can record
a signature whose inferred index changes. The upgrade check compares
both client ABIs with clean builds.
If recompiling a provider changes its ABI, the normal dependency check
reports the provider by name. If its ABI stays the same, the compiler
checks whether the client used a family whose inferred arguments can
change. This also covers a library imported through a re-export.
[Upgrade test](../examples/closed-evaluation/check-readback-upgrade.sh).

#### Earlier failures

In `KindEqualities2`, the equation for `IK (Arr k1 k2)` returns
`IK k1 -> IK k2`. After a Template Haskell declaration splice, the
family `I` uses `IK` as its result kind. At `6d16505aaa` the original
program reports GHC-83865 at `Maybe` and at `(I a) (I b)`. Writing
`IK k1 %M -> IK k2` in the equation makes the program print `Nothing`.

Moving only the declaration of `I` before the splice lets GHC check
the `Maybe` equation. In the application equation GHC still expects
`IK @{t} k2` and gets `IK @{M} k2`. The family is non-injective.
At that revision the application equation could not infer its result selector.
[Original program](../../../testsuite/tests/dependent/should_compile/KindEqualities2.hs).

Separate compilation reproduces both failures. A provider that declares
`IK` and `I` together exports an inferred matchability parameter for each
family. A client with only the provider interface can give equations for
`TInt`, `TBool` and `TMaybe`. Its interface records that the first two
equations accept either selector and that `TMaybe` requires M. The
application equation still fails. If the client owns `I`, the provider
exports `IK` without a parameter and the `TMaybe` equation also fails.

At `2da8cf75ec`, removing the `TMaybe` equation isolated a second problem. A recursive
`IK` equation with an explicit `%U` arrow rejected `(I a) (I b)`.
The same equation with `%M` accepted the application. The family-defined
arrow check above now accepts both forms of this isolated application.
The staged program also needs inference for the `TMaybe` equation.

### M141. Inferring an omitted arrow in a dependent argument

In `D (Type -> Type) Maybe`, the declared kind of `D` says that its
second argument has the kind supplied by its first. The kind of
`Maybe` determines M for the written arrow. If the second argument
has an explicitly U kind, the same source arrow gets U. If it only
requires an arbitrary kind, an unresolved omitted arrow keeps the
legacy U default. A user-written `FUN @M` or `FUN @U` stays fixed.

In type declarations and complete value signatures, GHC solves the
equalities before defaulting local matchability variables. The test
compiles the same source with the dependent extension off and on.
It reads the binder from another module's interface and checks a
locally quantified matchability in a GADT constructor field. A
judgment with registered retained indices keeps its existing rules
for immutable kind arguments.
`RAE_T32b` now compiles. Fourteen of the earlier fifteen positive
programs remain rejected. The other 21 diagnostic or output mismatches
are unchanged. A family equation used as both a runtime
arrow and the kind of `Maybe` still presents the dual-use problem above.
[Original test](../../../testsuite/tests/dependent/should_compile/RAE_T32b.hs),
[inference and nominal equality controls](../../../testsuite/tests/dependent/diagnostics/DependentArrowMatchability.sh)
and [ordinary source example](../examples/open-evaluation/LegacyKindArgument.hs).

Run the directory tests with a built in-tree compiler.

```sh
hadrian/build --flavour=default --test-compiler=stage2 --test-speed=normal \
    --test-way=normal --test-root-dirs=testsuite/tests/dependent test
```

### M166. Passing an unsaturated family to an unmatchable binder

With `ExperimentalDependentTypes`, a user type family may omit a suffix
of visible arguments whose arrows are known to be unrestricted and
unmatchable. The type checker and Core Lint preserve this prefix.
Reduction starts when the application reaches the family arity.
The solver provides forward congruence and does not infer argument
equalities from equality of applications at an unmatchable arrow.

The bootstrap checks pass for implicit kind arguments, a partial prefix
saturated after rewriting by a Given, and an ordinary consumer of an
exported parameterized type synonym. Changing the open family equation
invalidates the consumer's proof. Liberal type synonym expansion can
still saturate a partial argument, and eta-reduced data-family newtype
axioms retain their representational behavior. `NativeUnsaturatedFamily`
covers separate compilation, invalidation, a stuck comparison of
distinct prefixes, and rejection of inverse decomposition.

M166 initially excluded generated source-certificate families.
M169 permits a partial application with a checked certificate.
`OrdinaryPrefix = W.each W.signal` supplies the callback and leaves
the list argument open. Both functions come from an ordinary provider.
An ordinary client takes the prefix from the alias interface and
passes it to a type family that supplies the last argument.

Storing the prefix records the generated family name. Reducing the
application checks the source owner and records the dependencies.
Editing the callback invalidates the client proof with the alias
interface unchanged. [Ordinary prefix](../examples/open-evaluation/OpenMapSource.hs)
and [interface and recompilation check](../../../testsuite/tests/dependent/should_run/NativeOpenMap.sh).

M166 also excluded omitted invisible binders. The next two cases
describe the later support for them.

### Passing a family with an invisible argument

`InvisibleFamily` defines a family `Select` with kind
`forall k. @U k` and an equation `Select @Bool = 'True`. A receiver
that expects the entire polymorphic kind can take `Select` as its
argument. The family `Run` supplies `@Bool` later. An ordinary client
proves the result is `'True` through the provider interface. A receiver
that expects `Bool` still gets the saturated application.
[Provider](../examples/open-evaluation/InvisibleFamily.hs)
and [consumer and rejection tests](../../../testsuite/tests/dependent/should_run/NativeUnsaturatedInvisible.sh).

This check requires a known expected kind and an unmatchable binder.
The compiler still rejects inverse equality between two family heads
and a family declaration whose argument is marked matchable. The
compatibility failures above also need a policy for arrows in the
*result* of a family equation.

### Inferring a kind after supplying a visible argument

`RunVisible (PickVisible 'True)` retains the invisible argument of
`PickVisible`. The receiver already requires the full invisible
telescope, so the kind checker can infer its result kind as `Type`.
`RunVisible` supplies `@Bool`. Reduction starts at the family arity.
[Proposal #242 on quantifiers](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#other-quantifiers).

The test reads the retained application from the provider interface.
An ordinary client proves the result is `Int`. Changing the equation
invalidates that proof. An inverse equality and a receiver that requires
`@M` both fail. A receiver can also ask for only the final invisible binder.
In `RunVisible @Bool (ReadAt 'True)`, the receiver expects
`forall input. @U Bool`. GHC infers `Bool` for the first omitted
argument to `ReadAt`. The receiver supplies the second argument.
The same provider yields `Int` when the receiver asks for a result
of kind `Type`. An explicit conflicting argument and a receiver that
demands a matchable binder both give GHC-83865. With no expected
kind the compiler keeps the existing saturation rule.
[Provider](../examples/open-evaluation/InvisibleFamily.hs) and
[ordinary interface client](../../../testsuite/tests/dependent/should_run/NativeUnsaturatedInvisibleConsumer.hs).

## Goal and dependencies

Allow direct higher-order use of type families under explicit matchability,
arity and inference rules. A matchable function permits inverse decomposition
of application equality. An arbitrary type family or runtime function need
not be injective and cannot support that rule.

The upstream graph relates R23 to completed modifier syntax R25. R13 function
promotion and R20 language preparation refer to R23. Our native generic-domain
work needs a matchability invariant before unrestricted function-valued fields.
That local need does not require the whole source-unsaturation feature.

## Prior art and current boundary

[Proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html)
and [Higher-Order Type-Level Programming in Haskell](https://simon.peytonjones.org/assets/pdfs/higher-order-type.pdf)
provide the main design. The proposal is experimentally accepted. Syntax,
defaulting and some quantifier assumptions need review.

The reviewed implementation is
[mmzk1526/ghc at `d65f4c6cc039`](https://gitlab.haskell.org/mmzk1526/ghc/-/commit/d65f4c6cc039385184c6d28ba5420a7a2b3ebc4f),
dated 2024-06-23. It is based on historical GHC 9.9. It is prior art for a port,
not a patch validated on current GHC. Selected source blobs were read; the
branch was not built here.

Current native calls are opaque and deliberately noninjective. That restricted
path does not implement R23. Generic fields add an independent obligation:
a saturated family can expose a function-valued field to ordinary application
and projection. Preserve already-derived coercions when substituting that
field. Rebuilding the resulting type application alone is insufficient.
Current function-occurrence gates reject the starting input. No executed
counterexample against the current closed subset is claimed.

## Next increments

The [M63 contract](../PLAN.md#m63-ordinary-arrow-matchability) selects an
ordinary-arrow stage with complete transport, interface and reflection work.
The [M64 contract](../CORE.md#forall-matchability) adds per-binder metadata,
nominal evidence at every body role, source annotations, ordered constructor
telescopes, three additive TH wrappers and supported Typeable/FFI normalization.
It preserves explicit invisible U. Both compiler stages and the selected
controls pass. The maintained cohort passes Showcase95/14 in both stages.
The standard selection passes seven tests, with 54 other ways skipped.

Runtime abstraction uses U. Analysis does not impose a runtime abstraction.
Existing M values can use identity subsumption, but arbitrary application RHSs
and complete signed expressions retain conservative checking limits.
Constructor-local Givens do not justify later metadata while typed DataCon
forms hoist fixed-U quantifiers. General polytype TypeRep remains unsupported.
Keep generic-domain widening gated until both stages and field/telescope
transport are complete. Do not describe a syntax-only classification refactor
as a matchability implementation.

The [M65 contract](../PLAN.md#m65-function-valued-fields-in-closed-native-index-domains)
selects the first native function-field client. It connects saturated-family
application to the existing opaque call index and preserves the complete
forward proof. Both rebuilt stages pass its selected source, interface and
demand controls. Seven registered tests and Showcase100/16 pass.
This does not implement source family unsaturation or close R23.

- [x] Write the kernel specification before porting. Classify constructor,
      family and runtime-function applications. State which equality rules
      permit decomposition. Preserve generativity of ordinary constructors.
- [x] Port matchability through kinds, quantifiers, `FunTy`, coercions,
      substitution, Lint and native `DepFunTy`/`DepFunCo` handling.
      Preserve requested invisible `U` binders through smart constructors.
      Review inferred/specified helpers and runtime type-lambda recovery.
- [x] Preserve role checks on every added coercion field. The prototype uses
      nominal matchability evidence in `ForAllCo`, including at phantom role.
      Do not infer that evidence from an arbitrary phantom parent coercion.
      Preserve the current representational visibility cast between invisible
      Core type lambdas and required source foralls.
- [x] Update raw `FUN` layout, kind recovery, interfaces, reflection and
      fingerprints. Its specified invisible binder does not consume the
      required visible multiplicity argument. Keep the other three arrow
      constructors' fixed-unmatchable convention under explicit review.
- [x] Add useful diagnostics for hidden matchability differences. Preserve
      multiplicity and quantifier information. An inverse-call explanation
      must not discard independent equality evidence that solves the goal.
- [ ] Check the generic-domain client with saturated families before source
      unsaturation is enabled. An internal flag alone does not establish the
      invariant through all compiler paths.
- [ ] Complete source unsaturation, inference/defaulting, arity rules,
      application, interfaces and the higher-order family examples.

Default diagnostics for an ill-scoped telescope show its binder order without
fixed matchability annotations. The explicit print flag shows the annotations.
A matchability mismatch still shows the expected and actual annotations.
[Display rule in proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#showing-matchabilities).

## Substituting an equal index in a partial family

An ordinary client can prove `Callback a :~: Callback b` from an
argument of type `a :~: b`. Pattern matching on `Refl` identifies the
indices. After rewriting both indices, GHC compares the complete
partial applications for reflexivity. The earlier compiler reported
GHC-83865 at the second `Refl`, even though it printed identical types.

The inverse still requires a separate proof. Two distinct prefixes
can also keep a closed family stuck at a repeated pattern.
[Source and inverse checks](../../../testsuite/tests/dependent/should_run/NativeUnsaturatedFamily.sh)
and [Agda and GHC provenance](../REFERENCES.md#equality-of-a-partial-family-after-rewriting-its-argument).

## Completion boundary

The first increments establish a shared kernel foundation for R05 and R03.
Keep R23 open until direct higher-order family use works under the selected
source design and compatibility checks. Defunctionalization in `singletons`,
accepted proposal status and native `TermAppTy` do not close this card.

## Research records

The ignored cache `_build/verification/native-roadmap-research-2026-11-16/`
contains the pinned implementation review, native integration draft and
compatibility correction:

- `promotion/matchability-implementation-v1/`.
- `promotion/matchability-native-integration-v1/`.
- `promotion/matchability-native-integration-addendum-v1/`.
- `core/matchability-diagnostics-v1/`.
- `core/generic-matchability-v1.md`.
- `promotion/invisible-forall-matchability-v1/`.

Read the addendum with the draft. It corrects the visible-argument analysis for
the new invisible `FUN` binder. Each record states its source pin and reading
scope. Those historical controls were unexecuted research. The selected M63
controls now pass under the separate evidence prefix below. Keep this page and
[REFERENCES.md](../REFERENCES.md) usable without the ignored cache.

M63 selects source commit `95a19a409e`. Bootstrap v13 and selfhost v7 pass.
Focused v3 passes all thirteen wrappers, including TH with both interpreter
paths and selected old interface and TypeRep payloads. Checks v1 passes eight
wrappers with nine measured cohort rows. Both stages pass public19,
parameterized14 and Showcase91/14. The five registered tests pass; 34 other
ways are skipped. Evidence uses `native-arrow-matchability-*`.
The binary dependency still has one documented local decoder-line change.
This is a validated ordinary-arrow stage, not completion of R23.

The invisible-forall review identifies a concrete port obligation. The pinned
prototype's `mkTyCoForAllTy` overwrites requested invisible matchability with
`M`. Copying that shortcut would discard an explicit `U`. Visibility alone
does not establish matchability. M64 runtime erased abstraction uses U. Reusing
an existing M value does not introduce that abstraction or make an arbitrary
runtime function matchable.

Modern GHC also preserves higher-rank invisible binders and uses representational
`ForAllCo` casts for required type arguments. Keep the matchability evidence
nominal through that bridge. Family saturation is an arity rule; a zero-arity
family can retain a forall result kind. That fact alone does not establish a
noninjective inhabitant or a current exploit.
