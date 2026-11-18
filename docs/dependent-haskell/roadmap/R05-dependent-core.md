# R05: Dependent Core language

Upstream: [Dependent Core language](https://ghc.serokell.io/dh#dependent-core-language).
Local status: restricted native implementation. General transformations remain
open. Use the [work queue](../ROADMAP.md#current-work-queue) for priorities.

## Goal and dependencies

Provide a coherent typed Core representation for dependent types. Define
substitution, conversion, scoped evidence, roles, runtime usage and erasure.
Preserve the compiler transformations and ordinary GHC behavior that depend on
those rules. The upstream graph gives R05 no outgoing feature edge. That does
not remove its research or implementation obligations.

The user selected a native Core extension based on Eisenberg's historical
`nokinds-pi` work. The current representation is specified in
[CORE.md](../CORE.md). Keep `singletons` as a comparison implementation.

## Supported subset and evidence

- `TermVarTy`, `TermAppTy`, `DepFunTy` and scoped native coercions.
- Closed algebraic domains, retained field telescopes, fixed function-valued
  fields and opaque function heads. A head is not a general term expression.
- Scoped constructor refinement, Core Lint, interfaces and checked lowering.
- Bounded beta exposure, alias substitution and dead-local removal.
- M56 closed parameterized domains and M57 nominal scalar-domain conversion.
  M58 source checking uses those Core rules. M59 extends frontend comparison
  beneath unchanged function domains using the existing congruence rule.

The [published M62 checkpoint](../ROADMAP.md#where-this-fork-stands) passes its
selected checks with both compiler stages and its independent artifact audit.
M63/M64 add ordinary-arrow and forall matchability. Their selected source,
Core, reflection and compatibility checks pass in both stages. The foundation
is published. The full R23 scope remains open. M65 adds closed function-value indices.
M66 bounds the selected native family views and preserves diagnostic context.
M67 adds structural generic admission, opaque parameter fields and nominal
later-domain transport. Both builds, the selected API/source controls,
Showcase116/16 and seven registered tests pass. Generic elements include
`Vec Int n` for an earlier runtime index. Source roots retain their structural
or fixed native-call-head policy. See [the checkpoint](../PLAN.md#m67-generic-domains-source-and-self-hosted-validation).
Restricted pipelines and Lint checks do not establish general preservation.
M68 makes the existing application contract available to ordinary importing
modules. Both stages check native dictionary scope and reject escaping or
linear actuals. The owner is R03. This increment adds no Core equality rule.

## Next increments

- [x] Specify and run an opaque whole-function cast probe. Use genuinely
      abstract payload types and a scoped coercion variable. Check type
      recovery, Lint, CorePrep and object execution with full and partial
      applications. Measure bottom demand and ignored arguments.
      Prepare abstract evidence before supplying reflexivity at the call site.
      The native-object check needs a separate abstract `NOINLINE` binding.
      Inspect that binding after simplification, tidying and actual CorePrep.
      The bounded specification is [M61](../PLAN.md#m61-opaque-whole-function-cast-lowering).
      Scratch checks pass after function-head beta substitution is corrected.
      The actual NCG dump retains the abstract cast. The self-hosted build and
      selected regression cohort pass. The independent audit passes.
      Partial-WHNF and demand checks use bytecode. NCG checks a fully applied
      keep caller and the preserved scalar-domain caller.
- [x] Compile retained module and prompt declarations to interpreted bytecode.
      Keep typed statement results across GHCi commands. See [M87](../PLAN.md#m87-persistent-dependent-definitions-in-ghci).
- [x] Preserve nested certified calls and constructor fields through Core
      Lint and erasure. Give each nontrivial operand a lazy STG binding.
      See [M89](../PLAN.md#m89-nested-computations-in-retained-operands).
- [ ] Specify an opaque Pi instantiation rule if a typed transformation needs
      one. M61's measured whole-cast lowering needs no new primitive. Add a proof
      operation only when a measured or calculus obligation requires it.
- [ ] Integrate the [R23 matchability foundation](R23-unsaturated-type-families.md)
      with native types and coercions. Preserve forward congruence and restrict
      inverse projection to the permitted function kinds.
- [x] Implement M67 generic domain metadata for `[a]`. Update binder types,
      occurrence types, multiplicities, substitution and zonking together.
      Constructor fields at arbitrary `a` need an opaque-field rule. Include
      Int, functions, arbitrary lifted types and `a = Vec Int n`. The last
      case also needs later-domain transport through the earlier runtime n.
      Separate structural containers, opaque values and call permission.
      The bounded source, interface and self-hosted checks pass. General R05
      transformations and broader source domains remain open.
- [x] Admit fixed function leaves in closed algebraic indices. Preserve
      canonical call endpoints, forward family proofs and later package fields.
      M65 passes both builds, separate-module demand controls, Showcase100/16
      and seven registered tests. Generic `[a]` and higher-order endpoints
      remain open. See [the contract](../CORE.md#function-values-in-algebraic-indices).
- [x] Bound the inherited family views in native shape and argument inference.
      Use the solver's reduction-depth policy. Preserve the inverse nominal
      proof for shape matching and the original cached argument usage.
      The bounded contract is [M66](../PLAN.md#m66-bounded-family-views-in-native-frontend-inference).
      Both stages, exact diagnostic spans/contexts, finite controls,
      Showcase100/16 and the seven selected registered tests pass.
- [ ] Specify transport through later runtime-dependent telescope entries.
      Freshen binders and transport remaining fields, targets and evidence
      together. Preserve the distinction between lexical scope and active
      retained-body comparison scope. M67 supplies nominal transport for its
      generic retained-domain contract. General expression and proof
      abstraction remain separate obligations.
- [x] Preserve retained-binder visibility in Core, proof operations and
      interfaces. Check a representational visibility cast through native
      object execution. See [M71](../PLAN.md#m71-visibility-of-retained-binders-in-core).
- [ ] Extend one ordinary Core transformation with a measured benefit.
      Preserve deep type dependencies, index identity, demand and sharing.
      Keep other unsupported transformations gated.
- [ ] Complete native unfolding and coercion-interface handling. Broaden
      optimizer and ordinary GHC compatibility checks before release.

Do not derive nominal domain equality from arbitrary representational Pi
equality. `DepFunCo` can have the role of its body proof; its domain evidence
is nominal. `TermAppCo` has a separate nominal restriction. Ordinary `InstCo`
assumes erased quantification and needs a rule review before reuse here.

## Completion boundary

The historical C1 package is the representation and calculus contract.
C2 is the transformation and compatibility work. Both belong to R05 and remain
open. A paper correspondence is not an implementation proof. A passing probe
closes only its stated behavior. Complete scope, metadata, roles, interfaces,
normal optimization, demand and compatibility obligations before closing R05.

For generic domains, preserve ordinary instantiation through constructor
refinement. Metadata substitution and acyclic zonking come first. Opaque
parameter fields must stay opaque after their type becomes known. Nominal
later-domain transport must retain earlier runtime indices. A preparatory
runtime-independent gate does not complete this generic target. See the
[updated research](../REFERENCES.md#generic-domains-after-matchability-and-function-values).

## Research

- [A Specification for Dependently-Typed Haskell (2017)](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf):
  explicit equality, substitution and erasure with nontermination.
- [A Role for Dependent Types in Haskell (2019)](https://richarde.dev/papers/2019/dep-roles/dep-roles.pdf):
  nominal and representational dependent equality.
- [A Graded Dependent Type System with a Usage-Aware Semantics (2021)](https://richarde.dev/papers/2021/grad/grad-extended.pdf):
  usage and dependent typing. Its assumptions do not combine automatically with
  the other calculi or prove this implementation.
- [System FC as implemented in GHC](https://gitlab.haskell.org/ghc/ghc/-/blob/master/docs/core-spec/core-spec.pdf):
  compare native rules with the current compiler contract.

See [the reference ledger](../REFERENCES.md) and [paper records](../papers/README.md)
for reading scope. The dated ignored research cache has
`core/opaque-pi-elimination-v1.md`, `core/generic-domain-obligations-v1.md`,
`core/generic-matchability-v1.md`, and the Agda telescope review. These are
research records; proposed controls remain unexecuted.

The historical opaque-cast preparation is in
`_build/verification/native-opaque-pi-whole-cast-probe-preparation-v2/`.
M61 completed nine quiet API checks for bytecode and demand, plus separate
object-path checks. Its object-path notes distinguish actual backend preparation
from a preview call. Runtime closure with reflexivity alone cannot establish
that opaque evidence survived an earlier typed phase.
