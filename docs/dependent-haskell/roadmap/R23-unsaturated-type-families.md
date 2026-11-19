# R23: Unsaturated type families

Upstream: [Unsaturated type families](https://ghc.serokell.io/dh#unsaturated-type-families).
Serokell marks the proposal as accepted. The fork records matchability
on ordinary arrows and forall binders. M166 lets a user pass an unapplied
family at a known unmatchable arrow. Full inference, partial applications
with remaining invisible binders, and ordinary-program compatibility
remain open.
Use the [work queue](../ROADMAP.md#current-work-queue) for priorities.

## Matchability on a source arrow

Import `U` from `GHC.Exts` and enable `Modifiers` to write the
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

Omitted invisible binders and generated source-certificate families
remain outside this checkpoint. A reducing use of a generated family
must still establish its source owner and record its rewrite-rule and
interface dependencies.

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
