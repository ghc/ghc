# R23: Unsaturated type families

Upstream: [Unsaturated type families](https://ghc.serokell.io/dh#unsaturated-type-families).
Upstream status: accepted, awaiting implementation. Local status: research and
integration planning. No matchability implementation is claimed in this fork.
Use the [work queue](../ROADMAP.md#current-work-queue) for priorities.

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

- [ ] Write the kernel specification before porting. Classify constructor,
      family and runtime-function applications. State which equality rules
      permit decomposition. Preserve generativity of ordinary constructors.
- [ ] Port matchability through kinds, quantifiers, `FunTy`, coercions,
      substitution, Lint and native `DepFunTy`/`DepFunCo` handling.
      Review the prototype's provisional invisible-forall assumption.
- [ ] Preserve role checks on every added coercion field. The prototype uses
      nominal matchability evidence in `ForAllCo`, including at phantom role.
      Do not infer that evidence from an arbitrary phantom parent coercion.
- [ ] Update raw `FUN` layout, kind recovery, interfaces, reflection and
      fingerprints. Its specified invisible binder does not consume the
      required visible multiplicity argument. Keep the other three arrow
      constructors' fixed-unmatchable convention under explicit review.
- [ ] Add useful diagnostics for hidden matchability differences. Preserve
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

Read the addendum with the draft. It corrects the visible-argument analysis for
the new invisible `FUN` binder. Each record states its source pin and reading
scope. All proposed port controls remain unexecuted. Keep this feature page and
[REFERENCES.md](../REFERENCES.md) usable without the ignored cache.
