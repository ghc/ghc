# Native dependent Core implementation plan

Implement dependent types in Haskell through a native extension to Core. Base the
work on Richard Eisenberg's historical `nokinds-pi` branch. Port its useful changes
to current GHC and complete the missing rules. The initial retained Boolean
experiment is implemented. The current work follows the roadmap below.

Last updated: 2026-11-16. Upstream compiler base:
`19ff36f2aede0ca2e554016ff22c71e73aa9803b`.

See [current status](README.md) and [primary references](REFERENCES.md).

## Current priority: roadmap-aligned increments

Use [ROADMAP.md](ROADMAP.md#current-work-queue) as the main work queue. These
specifications record bounded increments and their historical checkpoints.
Each new increment must name one owning roadmap card, related cards, required
prerequisites, acceptance checks and the behavior that remains unsupported.

The current increment is [M62](#m62-complete-retained-function-type-arguments),
owned by [R03 dependent products](roadmap/R03-dependent-products.md). It admits
complete retained-function types in explicit type arguments. M61 is audited
and published.
The user
resumed GHC execution on 2026-11-16 at 17:53 CET.

The next substantial foundation is [R23 matchability](roadmap/R23-unsaturated-type-families.md), followed by
[R05 generic-domain metadata](roadmap/R05-dependent-core.md). The roadmap
separates this chosen order from actual feature prerequisites. Keep ordinary
evaluation, function promotion, first-class sums and the remaining promotion
domains visible there.

## Historical source checkpoints

The source constructor checkpoint is verified at `8e90e0386b`; the source filter
is verified at `c9aa3545c0`. Both compiler stages run the
[source example](examples/packed-vector/README.md) through a separate producer
interface and object file. It declares Nat, Vec and PackedVec in source. It
constructs, opens, repackages and filters vectors. Its output length is computed
with `Z` and `S` from runtime predicate results.

All 36 package, filter and section observations pass. Sixteen located rejection
checks, the five focused registered regressions, Core API and bootstrap runtime
also pass. See [the recorded evidence](README.md#source-vector-filter-checkpoint).
M12's length-bound proof is verified at `7eae40be91`, with the nested-context
instantiation fix at `4ad47fdeca`. Both stages pass all 58 source observations;
the runtime fixtures also check 19 package/bound rejections. Preserve the
existing signature, proof, laziness and ordinary Haskell controls.

M13 is verified at `7a98362197`. Its source run passes 81 observations in both
stages. Ordinary package fields admit nested vector/proof patterns and wildcards.
Separate per-equation cases preserve retained field identity and source clause
order. The API and runtime controls pass with 20 package/bound rejection checks.
All five focused registered regressions pass.

M14 is verified at `ed5b0f56e4`. Both compiler stages pass 102 source observations
after the Core Tidy alias fix. Source uncons packages a vector tail with its
actual runtime predecessor. Repeated uncons preserves finite element order and
produces a bounded prefix from the tested cyclic input. The Core API and runtime
fixture pass with 22 package/bound rejections. All five registered regressions
pass. The original Core Lint failure remains in the verification artifacts.

M15's source workers are verified at `f3350988c3`, with application checking
at `78460ce4f4`. Both compiler stages pass all 136 source observations. The
bootstrap runtime fixture and registered runtime test pass 27 package/bound
rejections. Four standalone equality-scope errors have pinned codes and source
locations in both stages. All five focused registered regressions pass.
The new source uses ordinary `:~:` evidence to
align independently computed runtime lengths before zipping their vectors.

M16 source workers are committed at `fba00f8f0c`, with compiler changes at
`491c5fb0c9`. Both compiler stages pass 180 source observations. The bootstrap
and registered runtime fixtures pass 34 package/bound rejections. Seven
standalone errors have pinned codes and primary source tokens in both stages.
A declaration-only producer passes separate imports, partial type applications
and an unused retained field. All five focused registered regressions pass.

M17's compiler change is `ab985a150e`, with import cleanup at `f423a0ce97` and
source/fixture at `e54802bcfd`. Both stages pass 209 source observations and
14 standalone rejection checks. The runtime fixtures pass 40 package/bound
rejections. All five focused registered regressions pass. Source uncons now
matches the stored Nat directly in its package pattern.

M18's source reverse is committed at `9fefa1a496`, with signed-argument
inference at `12bfbef0b3`. Both compiler stages pass 249 source observations
and seven standalone reverse rejections. The runtime fixtures pass 47
package/bound rejections. All five registered regressions pass. The source
wrapper stores the original runtime length with its reversed vector. Next,
M19's compiler is `fdf23acaea`, with comment cleanup at `a367bd0a7a`.
Both compiler stages pass 269 source observations, and the runtime fixtures
pass 55 package/bound rejections. All five registered regressions pass.
Constructor equations accept later retained variables and preserve contexts.
M20 verifies four constructor equations for `sameNat`; M21 verifies a final
named fallback. M22 verifies anonymous fallback columns with 331 source
observations, 76 API package/bound rejections and 42 standalone located errors
in both stages. All five registered regressions pass. The original 316 source
outputs remain unchanged. M23 implements complete local payload signatures
that refer to an already-bound runtime index. Both stages pass 343 source
observations, 53 standalone located errors and four ordinary controls. The
API/runtime fixture passes 87 package/bound rejections. All five registered
regressions pass. M24 verifies ordinary native action binds with 360 source
observations, 60 located errors, four ordinary controls and 94 API rejections.
All five registered regressions pass. M25 verifies scoped native source
inference with 377 observations, 69 located rejection cases, four ordinary
controls and 103 API rejections in both-stage validation. All five registered
regressions pass. M26 verifies boxed tuple inference with 395 observations,
79 located rejection cases, four ordinary controls and 113 API rejections.
All five registered regressions pass. M27 verifies native tuple sections with
417 source observations, 88 located rejections, four ordinary controls and
122 API rejections in both stages. All five registered regressions pass.
M28 verifies native generic result overapplication with 439 source observations,
99 located rejection cases, four ordinary controls and 133 API rejection cases.
Both stages pass the failed-match cache trace. All five registered regressions
pass. M29 verifies native generic inference after leading visible type applications
with 461 observations, 112 located rejections, four ordinary controls and 146
API rejections. Both stages pass the cache trace and all five registered tests.
M30 verifies native-index-kind inference after leading VTA with 479 source
observations, 126 located rejections, four ordinary controls and 160 API
rejections. Both stages and all five registered regressions pass. M31 verifies
joint native-index/payload inference and result overapplication with 503 source
observations, 139 located rejections, four ordinary controls and 173 API
rejections. Both stages and all five registered regressions pass. M32 verifies
outer equality evidence under fixed-domain retained Pi types, with 532 source
observations and 156 located rejections. M33 verifies checked Pi equality
operands and immutable rewriting beneath retained binders, with 562 source
observations and 177 located rejections. The API fixture checks 210 package
and 291 total rejections. Both stages and all five registered regressions pass.

## Direction and boundaries

The implementation extends Core. It does not require singleton elaboration or
explicit Boolean reification at ordinary runtime call sites. Keep the validated
singleton translation as a comparison implementation for the overlapping subset.
It supplies expected results, but it does not prove the native extension.

The historical starting point is
[`nokinds-pi` at `b64ef80f8002727422c05092f429c38d0527ed0b`](https://github.com/goldfirere/ghc/tree/b64ef80f8002727422c05092f429c38d0527ed0b),
dated 2014-08-19. It introduces `PiTy`, independent binder attributes, and a sketch
of dependent application checking. It is unfinished. Its type-computation utility
still discards runtime arguments, and its case checker uses one fixed result type.
Updating this branch requires semantic work as well as API changes.

Use the later D/DC, roles, and graded-usage research to evaluate the port. Their
formal results do not automatically apply to the historical branch or this GHC
implementation. Keep existing FC coercion checks. Do not add trusted universal
coercions to make incomplete dependent rules pass.

Start with visible, retained, unrestricted Boolean binders. Results have fixed
lifted runtime representations. Source index expressions initially use Boolean
constructors and named runtime Boolean variables. A producer can supply the value
of such a variable. The type checker does not evaluate that producer or equate
separate variables because their definitions look equal.

The internal index representation must remain valid under compiler substitution.
A source restriction to variables and constructors is insufficient: inlining can
replace a variable with an arbitrary expression. Either represent the substituted
expression safely or preserve its binding and restrict the relevant transformation.
Settle this rule before enabling dependent optimization.

Defer general term evaluation in types, automatic promotion, retained linear
binders, dependent runtime representations, and general dependent inference.
Keep arithmetic and SMT integration separate from this Core work.

## First source target

These examples describe the M7 target. The `foreach` spelling follows the design
proposal. Restricted source `keep`, constructor-based `choose`, and their shared
runtime composition now check and execute through the native bytecode entry.

```haskell
type family Select (b :: Bool) where
  Select True = Int
  Select False = Bool

choose :: foreach (b :: Bool) -> Select b
choose True = 1
choose False = False

keep :: foreach (b :: Bool) -> Select b -> Select b
keep b x = x

again :: foreach (b :: Bool) -> Select b
again b = keep b (choose b)
```

The native target must also accept an existing runtime Boolean as an index.
Multiple dependent calls must refer to the same binding. Branches can consume the
selected value after checked Boolean refinement. A locally bound index cannot
escape its scope without a specified dependent result or existential package.
Passing a runtime Boolean must not insert a strict singleton conversion.

## Task sequence

M0 through M5 provide the internal Core and bytecode path. After the first
restricted M6 step passes, implement source `keep` through that path in M7.
This gives an early parse-to-execution check. Complete the remaining M6 work
before enabling the ordinary optimizer. M8 still controls interface and program
integration.

### M0: completed preparation

- [x] Update and build the current GHC baseline.
- [x] Inspect the proposals, roadmap, and implementation research.
- [x] Validate a singleton comparison implementation on the baseline compiler.
- [x] Record the native Core direction and initial tasks.

### M1: audit and pin the historical port

- [x] Retain the historical branch and exact commit as a local reference.
- [x] Inventory the relevant commits. Separate work already incorporated into GHC
      from retained-dependent changes. Map old modules to current equivalents.
- [x] Record incomplete rules and unavailable validation. Preserve commit provenance
      when porting changes. Keep current buildable GHC as the development base.
- [x] Retain the temporary singleton comparison program in a reproducible location.

Gate: a port inventory identifies reusable code, required adaptations, and new
semantic work. We have inspected the historical source but have not built it.
See [the port inventory](PORT.md) and [comparison programs](reference/README.md).

### M2: specify the restricted Core extension

- [x] Define retained Boolean binders, term indices, lambda, application, and case
      typing. State scope, substitution, erasure, and non-strictness rules.
- [x] Choose how runtime terms occur in types. Compare historical promotion with
      a checked index-expression representation. Require substitution closure.
- [x] Keep dependence, visibility, retention, and multiplicity distinct. Prefer a
      dedicated retained-dependent form that preserves existing `FunTy` and
      `ForAllTy` fast paths, unless the audit gives a concrete reason to change them.
- [x] Define nominal equality, constructor discrimination, and checked reductions.
      State which index comparisons remain opaque.
- [x] Specify a dependent motive or explicit branch-equality mechanism for cases.
      The existing case annotation lies outside the case binder's scope.
- [x] Explain how the rules fit FC, roles, multiplicities, and fixed runtime
      representations. Record the remaining formal proof obligations.

Gate: written rules and positive and negative Core examples. Resolve representation
choices before implementation. Cover bottom, partial application, and `seq`.
The first increment follows [the restricted Core specification](CORE.md).

### M3: port binders and implement dependent lambda and application

- [x] Add the retained binder and index representation to current type code.
- [x] Update free variables, capture avoidance, term/type substitution, equality,
      kinding, pretty-printing, and relevant traversals together.
- [x] Update `exprType`, function-result calculation, and Core Lint consistently.
      Runtime dependent application substitutes its argument into the result.
- [x] Check binder renaming, aliases, shadowing, supported index substitution, and
      scoped lets. Ordinary optimizer reduction remains gated until M6.
- [x] Keep ordinary GHC programs building while the feature remains gated.

Gate: hand-built Core for identity and retained application passes Lint. Lint and
type computation agree before and after supported substitutions. No frontend
syntax is required for this milestone.

### M4: implement dependent Boolean elimination and evidence

- [x] Implement the M2 case motive or branch-equality design.
- [x] Instantiate branch results under checked `True` and `False` refinements.
- [x] Update case scope in substitution, free variables, and Lint.
- [x] Preserve nominal index equality. Reject invalid casts and escaping binders.
- [x] Restrict refinement to strict constructor discrimination. Reject unsupported
      evidence producers and incomplete proof-bearing cases. Runtime bottom
      checks remain part of M5's execution gate.

Gate: native Core for `choose`, `keep`, and shared-index composition passes Lint.
Mismatched branches and forged refinements fail. Evidence follows checked rules;
it does not rely on `unsafeCoerce` or plugin axioms.

### M5: execute through the existing backend

The [lowering audit](CORE.md#lowering-audit-for-m5) specifies the restricted
CorePrep-to-STG path. It preserves native type metadata and checks each runtime
argument's representation before bytecode generation.

- [x] Carry the restricted extension through the minimum mandatory Core pipeline.
- [x] Update CorePrep and CoreToStg to preserve native typing and erase virtual
      proof binders. The retained Boolean remains one ordinary runtime argument.
- [x] Reject unsupported forms explicitly. Do not silently pass them to the backend
      or enable transformations that violate their invariants.
- [x] Inspect Core and STG for duplicate witnesses and inserted forcing.

Gate: native examples execute with the specified values and termination behavior
through a restricted pipeline. This does not establish optimizer safety. Even an
unoptimized build runs mandatory transformations that need an audit.

The restricted entry is `hscCompileCoreExpr` through bytecode. It skips ordinary
simplification and forces Core Lint before and after CorePrep, then STG Lint.
CorePrep disables speculative evaluation and optional argument arity analysis.
It keeps a lazy local index when its RHS cannot replace an index. The boundary
scanner includes synonym definitions and original arguments. M5 retains the
normal program, optimizer, and interface gates. JavaScript and native machine-code
execution are outside M5. M8 adds a separate checked module entry.

`NativeDependentCoreRun` supplies both Boolean values through one compiled worker.
It covers `choose`, shared `keep`/`choose`, retained eta expansion, partial
`keepSelect bottom`, fully applied dictionary-index `keep bottom`, a lazy local
index from a global thunk, and strict `choose bottom`. See [validation](README.md#native-execution).

### M6: preserve dependencies through optimization

The first increment uses an explicit restricted SimpleOpt entry. It omits
occurrence analysis, general inlining, dead-binding removal, and eta reduction.
These passes do not yet preserve runtime indices. Beta reduction must substitute
the same supported Boolean argument in terms and types. A literal Boolean case
can replace its branch proof with checked nominal reflexivity. Constructor
knowledge from an unfolding cannot justify that replacement for an opaque index.
Keep variable cases and lazy producer bindings. Require Core Lint before and
after this step in the bytecode entry.

This restricted entry is implemented at `0bbc32d396`. The registered Core API
and bytecode regressions passed. The default rebuild and ordinary smoke checks
also passed. The broader optimizer tasks below remain open.

PMC now recognizes a known retained function head without rewriting its domain
and codomain. It also preserves an ordinary function head that contains a
retained type. The immutable matcher handles whole-type templates with its
existing scope checks and a retained-aware occurs check. Unsupported retained
comparisons remain `MaybeApart`; this matcher does not construct Pi congruence.
Source regressions check strict partial applications and located incomplete
empty-case diagnostics.

The fixed-domain congruence increment adds `DepFunCo Id Coercion`. It binds one
shared `Many` runtime index under an unchanged supported closed domain. Both
body endpoints have kind `Type`. The proof inherits its body's FC role and keeps
the binder scoped through intermediate proof types. Each endpoint separately
canonicalizes a vacuous binder to an ordinary arrow. Keep value casts instead
of eta-expanding a partial function. See
[the Core rule](CORE.md#fixed-domain-retained-congruence) and
[the Agda and D/DC provenance](REFERENCES.md#agda-provenance).

- [x] Add binder-aware coercion substitution, lifting, free variables, zonking,
      tidying, and Lint checks. Preserve proof-only index dependencies.
- [x] Transport constructed arguments through a remaining fixed-domain retained
      function result in CorePrep. Preserve function WHNF and delayed payloads.
- [x] Check signed source `appendAfterSuccessor n = appendNative @Int (S n)`
      with one source pattern, then apply its remaining arguments.
- [x] Pass bootstrap kernel and runtime regressions. The interface-only consumer
      checks 26 observations. Unsigned aliases still report located GHC-83605.
- [x] Complete both-stage, registered regression, and separate CLI verification.
      Five registered regressions pass. Separate CLI checks cover 26 interface
      observations, eight partial/application observations, and nine standalone
      observations. Strictness, diagnostics, format rejection, guide, and ordinary
      compiler smoke checks pass. See the [checkpoint](README.md#fixed-domain-retained-congruence).

General Pi matching, ambient-evidence normalization, changed domains or multiplicities,
dependent erased binder kinds, and native coercion unfolding serialization
remain gated. Non-reflexive eta-cast handling also remains unsupported.

The next increment normalizes family axioms under the fixed-domain retained
binder. See [the Core contract](CORE.md#family-axiom-normalization-under-retained-binders)
and [the pinned Agda sources](REFERENCES.md#agda-provenance).

- [x] Normalize the Core body under a scoped lifting context. Permit only shared
      valid same-domain variable alpha-renaming for runtime indices.
- [x] Add `mkDepFunRedn` and canonicalize both endpoints independently. Preserve
      intermediate proof dependencies beneath the runtime binder.
- [x] Add solver `re_native_binder` mode. Keep global family axioms, ordinary
      meta-variable reading, roles, reduction depth, and locations. Suppress
      plugins, inert lookup, family-cache reads and writes, and coercion zapping.
- [x] Compile the bootstrap compiler in 2m10s. Logs use `native-pinorm` in
      `_build/verification/`.
- [x] Verify the kernel normalization checks and signed aliases with no source
      argument patterns: `leftUnit = nativeId`, `leftUnitAppend = appendNative @Int`,
      and `constantAlias = ordinaryNat` after reducing `ConstantNat`.
- [x] Verify 17 dedicated source observations and 38 imported observations
      (26 previous observations plus 12 new normalization observations).
- [x] Reject opaque `Add n Z` right-unit transport with GHC-83865 and report
      reduction-depth failure with GHC-40404. Check roles and binder freshening.
- [x] Complete both-stage, registered, CLI, guide, and ordinary smoke validation.
      The CLI checks the actual RHS lines for GHC-83865 and GHC-40404.
      All five registered regressions pass. See the
      [verified checkpoint](README.md#family-axiom-normalization).

This increment leaves ordinary `ForAllTy` binder-kind handling unchanged.
Family axioms reduce at nominal and representational roles; phantom family
heads remain unreduced. Ordinary forall binder kinds normalize at nominal role.
After rewriting both equality endpoints, recheck same-Id index reflexivity and
`tcEqType` equality of retained functions. The existing rewriting evidence
justifies this step. See Note [Reflexivity after retained normalization].
This adds neither structural retained decomposition nor meta-variable assignment.
Full scoped rewriting with ambient Given or plugin evidence remains future
work. Do not replace actual family reductions with a reflexive bypass.

The audit found separate blockers in the full simplifier. Its type substitution
can omit runtime Id mappings. Its case reconstruction treats virtual proofs as
constructor fields. Occurrence analysis does not track runtime index scope.
Keep the ordinary optimizer gated until those rules are implemented. Agda supplies
the application and constructor rules; Haskell's bottom and `seq` constrain which
reductions preserve behavior. See [decision provenance](REFERENCES.md).

- [ ] Audit simplification, beta/eta transformations, inlining, unfoldings, and
      substitution into types and coercions.
- [ ] Audit case simplification, case-of-case, join points, occurrence analysis, and
      let floating. A binder used only in a type still has a scope dependency.
- [ ] Audit demand analysis, worker/wrapper, specialization, and arity changes.
      Preserve laziness and the fixed runtime representation restriction.
- [ ] Enable transformations in bounded steps. Run Core Lint after each relevant
      pass and compare optimized and unoptimized behavior.

Gate: the Boolean examples pass at `-O0` and `-O2` with Core Lint. Record which
transformations were exercised and which remain gated. A passing executable does
not establish safety for untested forms.

### M7: expose the restricted source language

Start this task with the local experimental flag
`ExperimentalDependentTypes`. It is disabled by default and implies no other
extensions. Reserve `foreach` only when the flag is enabled. Parse its telescope
into a distinct `HsForEachTy` node. Do not reuse an erased `HsForAllTy` node.
Preserve source annotations, printing, and AST traversal. The initial syntax
increment rejected all retained types. Keep GHC-43582 for source positions that
remain unsupported: "Retained quantification is not supported in this type position yet."
Keep `foreach` available as an ordinary identifier when the flag is disabled.
Check parsing and printing through the existing native Core API regression.
Add source regressions for the diagnostic and the flag-disabled identifier.

The checking increment implements one complete monomorphic Boolean signature,
one bare retained variable pattern, and value application. It uses the existing
checked bytecode entry. The constructor increment adds source `choose`, a
constructor-based consumer, and composition under one runtime Boolean.

The constructor increment checks exactly one unguarded equation for each Boolean
constructor. It specialises later argument types and the result, following
Agda's pattern-context substitution. It uses explicit argument and result casts,
with branch proofs in the existing typed constructor pattern. A generated
`AsPat` preserves one runtime Id across the equations. Later arguments require
bare variable patterns and unrestricted multiplicities. The source target
is `observe b (keep b (choose b))`, with one shared runtime Boolean. Source
dependent `case` expressions use the context-refinement rule below.

- [x] Add an experimental opt-in flag, syntax, AST representation, and diagnostics.
- [x] Support native zonking, rigid signature generalization, scoped validity,
      opaque index rewriting, and same-Id equality for the first source rule.
- [x] Extend renaming and checking for explicit retained Boolean signatures.
      Track one runtime binder when it occurs in both terms and types.
- [x] Start with one equation and a bare retained variable pattern. Reuse its
      renamed `Name` in the expected type and existing pattern/desugaring path.
      Support native zonking and require an identity result wrapper.
- [x] Elaborate constructor and retained-variable applications into native Core.
- [x] Elaborate dependent Boolean constructor equations and source `choose`.
- [x] Accept ordinary runtime Booleans under shared bindings. Reject unsupported
      index expressions and inferred aliases with precise diagnostics.
- [x] Update source printers and extension documentation.

Gate: the source target and a runtime-unknown Boolean example compile and execute
directly. Their implementation uses no singleton ABI or reification step. These
checks pass through the bootstrap API and rebuilt compiler at `dfe9637939`.
The default both-stage rebuild and all five registered normal-way checks pass.
The source `case` increment derives a motive from a checked result type.
It generalizes referenced dependent locals as later arguments of a fresh
retained pattern function, then applies that function to the original values.
Constructor substitution refines both the result and those argument types.
This follows Agda's dependent case combinator and context generalization.
See [the provenance](REFERENCES.md) and Note [Source retained case refinement].
The current rule requires a named local scrutinee, complete unguarded constructor
clauses, bare fields, unrestricted captures, and a lifted checked result.
Parentheses are accepted.
Quotations, guards, defaults, nested patterns, and general motive inference
remain outside this rule. Ordinary cases retain their existing checker.

### M8: support interfaces and validate the compiler change

Start with an ordinary single-module executable at `-O0`. Keep native workers
local to an ordinary `main :: IO ()`. Omit interface pragmas and simplified Core
while native interface serialization remains gated. This checks machine-code
execution before adding cross-module transport; it does not satisfy the interface
gate below.

The module path uses a checked native branch before `core2core`, native
SimpleOpt over RHSs with top-level identities preserved, and Core Lint around
module tidying. Program CorePrep needs an explicit native opt-in with speculation
and optional arity analysis disabled. Force Core Lint before and after CorePrep,
then STG Lint. Program CorePrep skips ordinary occurrence analysis in
`deFloatTop`. Keep profiling, plugins, and optional STG transformations outside
this first path. Gate native-containing unfolding construction before its
ordinary optimizer calls. Reuse the existing CoreToStg erasure rule. The
bootstrap API and self-hosted CLI now compile, link, and run the ordinary source
executable with the shared Boolean and bottom observations. The default
both-stage build and all five registered normal-way tests pass at compiler
source commit `300eefffd1`. Eight negative cases require the expected unsupported
phase instead of accepting a generic failed load or panic. See the validation
details in [the status](README.md#ordinary-executable-integration).

Handle native interface signatures and expression unfoldings separately. Encode
runtime binders and occurrences through the interface's local Id environment,
not its TyVar environment. Preserve existing DataKinds constructor encoding.
Reconstruct Boolean alternative proofs from the scrutinee and constructor, then
check the resulting Core. New type tags require an explicit format distinction:
the original decoder treated unknown tags as literals, and the header version does
not identify a source commit. Check real file round trips in fresh processes,
fingerprints, malformed scope/proof rejection, and cross-module execution.

Use the existing whole-file version check before enabling the new wire tags.
Give the fork a deliberately distinct configured snapshot version and rebuild
matching target libraries. Record the exact chosen version and old-reader
rejection. Snapshot dates do not distinguish incompatible builds made on the
same day. Do not use profile tags as schema versions: `IgnoreHiWay` bypasses
that check. Reject unknown type tags explicitly in the new reader as well.

The signature increment uses `VERSION_DATE` set to `20261112.1`. Configure the
fork as `10.1.20261112.1`; its existing header version becomes `1001202611121`.
The final component distinguishes this native format from an ordinary dated
snapshot. Rebuild matching libraries. Preserve an old reader and interface to
check both version-mismatch directions, including `IgnoreHiWay`.

Encode a retained binder as `IfaceIdBndr` and its body. Encode index occurrences
as local Id names. Keep raw free Ids in a diagnostic-only form. Decode the domain
and multiplicity outside the binder scope, then allocate a fresh Bool/Many Id
for the body. Require result kind `Type`, not `Constraint`. Test real binary
round trips with sharing, fresh identities, tidied nested binders, malformed
scope and annotations, and unknown tags. Keep native unfoldings gated while
enabling signature-only cross-module calls through the ordinary object ABI.

Signature transport now passes the binary-table checks, actual `.hi` imports,
cross-module execution in fresh contexts and separate CLI processes, concrete
constructor applications, and old/new header rejection. Both compiler stages
build with the new version. The ordinary compatibility smoke and all five
registered normal-way tests pass. See
[the validation results](README.md#native-interface-signatures).

Use the existing declaration dependency fingerprints for retained signatures.
The consumer imports `choose` and `keep` without naming `Select`. Rebuild the
producer with fresh runtime Ids, then change only a function body with unfoldings
omitted. Require stable module and declaration ABI hashes and skipped consumer
code generation. Next, change `Select True` from `Int` to `Bool` without changing
the text of the retained signatures. Require changed `choose` and `keep` hashes,
an unchanged independent-function hash, and a located `GHC-83865` when the
unforced consumer compile rechecks its stale `Int` application. Use fresh
one-shot contexts and separate producer sources and interface search paths.
The bootstrap and all five registered normal-way tests pass. See the
[fingerprint validation](README.md#signature-fingerprints-and-recompilation).

For native unfoldings, first extend expression reconstruction under the existing
module gate. Reconstruct a Boolean alternative's virtual CoVar from its
scrutinee index and promoted constructor. Preserve ordinary binder-free Boolean
alternatives. Check malformed binder counts, unsupported scrutinees, scope, and
coverage with precise interface errors. Reject branch-local result escape before
`coreAltsType` can panic. Require Core Lint for each native imported unfolding,
including nested unfoldings, even when optional interface linting is disabled.
Compare its recovered type with the declared signature. Classify native forms
in either the RHS or the declared signature.

Do not enable the unfolding writer after changing only the alternative codec.
`mkCoreUnfolding` runs ordinary occurrence analysis on its template. Introduce a
checked construction path that preserves runtime index dependencies. Its scan
must include stable unfolding/rule/dictionary metadata that occurrence analysis
would traverse. Leave unused unstable unfolding templates deferred. Use a
separate own-RHS scan for deferred native validation. Gate native rule and
dictionary payloads before their ordinary occurrence-analysis paths. Preserve
the lazy RHS contract from `Note [Lazily checking Unfoldings]` and the cached
fields from `Note [Tying the 'CoreUnfolding' knot]` in `GHC.IfaceToCore`.
Validate expression round trips and malformed proofs before enabling module
unfoldings. Then test actual imported templates and cross-module execution.

This expression-table and declaration-reconstruction increment adds no wire
constructors. Normal module unfolding generation remains gated. Low-level
`writeIface` and `writeBinIface` APIs can construct files outside that gate.
Before enabling module generation, give proof-bearing expression interfaces a
new format distinction and verify rejection by the signature-only fork reader.
Do not claim compatibility with the old `.1` reader for such expression bodies.

- [x] Compile and run the source target as an ordinary vanilla executable through
      checked module simplification, tidying, CorePrep, and machine-code lowering.
- [x] Encode retained signature binders and indices with scoped local Id names.
- [x] Check signature round trips, disk imports, cross-module calls, and version rejection.
- [x] Check signature fingerprints and recompilation after a producer signature change.
- [x] Check expression-table reconstruction, deferred template validation, and lazy
      native unfolding construction through the GHC API.
- [ ] Emit native expression unfoldings in actual module interface files with a
      distinct format version.
- [ ] Check imported unfoldings and malformed proof rejection.
- [ ] Add focused compiler regression cases. Reuse existing testsuite conventions
      and golden tests where appropriate.
- [ ] Run relevant type-checker, simplifier, interface, and code-generation tests.
      Rebuild and bootstrap the compiler with the feature disabled.
- [ ] Record exact revisions, commands, results, and proof boundaries.
- [ ] Before publication, add the required changelog fragment with actual issue
      and merge-request references. Follow the
      [changelog guide](../../utils/changelog-d/README.md).

Gate: the experiment works across modules, ordinary programs remain compatible,
and the modified compiler builds itself. General dependent Haskell remains further
work.

### M9: runtime natural numbers and indexed vectors

Source checkpoint `714689a2fa` extends the verified Core checkpoint
`df623104fd`. It adds structural domains, outer erased quantification,
retained telescopes, actual constructor fields, and immutable instantiation.
The bootstrap API now passes source replication, append, polymorphic head,
retained telescopes, and the partial/cyclic/bottom observations. It also compiles
and links a fresh one-shot Nat/Vec consumer from actual producer interfaces and
objects. The configured signature format is now `10.1.20261113.1`. Both-stage
builds, registered self-hosted tests, separate CLI processes, and both directions
of format rejection pass. Checked CorePrep constructor-argument transport now
passes bootstrap execution, including nested fields, dependent payloads, and
two retained indices. Native unfolding bodies remain separately gated.

The next source target goes beyond Boolean selection. Support first-order
algebraic runtime indices, with a user-defined Peano `Nat` and length-indexed
`Vec` as the first program. Use ordinary promoted constructors for constructor
indices. A runtime input remains a retained value. Do not add singleton conversion.
Keep fixed lifted runtime representations and the checked `-O0` pipeline while
the broader optimizer work in M6 remains open.

```haskell
data Nat = Z | S Nat

data Vec a (n :: Nat) where
  VNil  :: Vec a Z
  VCons :: a -> Vec a n -> Vec a (S n)

type family Add (n :: Nat) (m :: Nat) where
  Add Z m = m
  Add (S n) m = S (Add n m)

replicate :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicate Z x = VNil
replicate (S n) x = VCons x (replicate n x)

append :: forall a.
          foreach (n :: Nat) -> foreach (m :: Nat)
       -> Vec a n -> Vec a m -> Vec a (Add n m)
append n m xs ys = appendVec xs ys

appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
appendVec VNil ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

head :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
head n (VCons x _) = x
```

These signatures and definitions now pass the bootstrap source runtime checks.
The result uses the caller's actual runtime index. A number read from input can
select a vector length without an explicit existential package. `Add` uses
ordinary closed-family equations; this does not require arithmetic SMT solving.
The ordinary GADT helper keeps append independent of runtime length evaluation.
Its constructor pattern exposes an erased predecessor index, not a runtime Nat.
The retained wrapper must instantiate the helper with the caller's runtime
indices. `replicate` supplies the actual predecessor value through its Nat
pattern. See [the Agda Nat/Vec references](REFERENCES.md#agda-provenance).

- [x] Generalize native index kinding and identifier validation from `Bool` to
      supported lifted algebraic domains. Keep scope and multiplicity checks.
- [x] Represent saturated constructor indices such as `S n`. Preserve their
      runtime field identities through substitution, tidying, and type recovery.
- [x] Refine constructor alternatives with both real fields and a virtual proof.
      Preserve the fields at runtime. Erase only the checked proof.
- [x] Check recursive Core for vector replication with an unknown runtime Nat.
      This is an intermediate gate; it does not complete the source target.
- [x] Extend frontend checking to scoped constructor-field indices, outer type
      quantification, and multiple retained binders. Support safe GADT constructor
      instantiation without allowing a runtime index to escape through a metavariable.
- [x] Compile and run the source `replicate` target, then source `append`, with
      actual input values. Preserve the same index across producer and consumer.
- [x] Reject inconsistent vector lengths and escaping predecessor indices with
      located diagnostics. Check lazy recursive fields and unused index arguments.
- [x] Transport the resulting signatures and programs across modules. Keep format
      compatibility and any unfinished M8 unfolding work explicit. Pin a new
      configured format before emitting Nat signatures and check `.1` rejection.
- [x] Transport constructor-tree arguments with fields through CorePrep naming.
      Keep later payload types and the final result at the original index.
      Preserve proof scope and laziness. Check prepared Core and STG. Keep
      changed-domain congruence and unsupported expression codecs gated.

Gate: a source program reads runtime lengths, constructs and appends vectors,
and consumes the statically indexed results. Check empty and successor inputs.
Check that observing the first element of a replication at `S undefined` does
not force its predecessor. A hand-built Core example alone does not satisfy M9.

Resolve these preservation rules in the implementation:

- In source pattern lowering for `S k`, put the actual field binder before the
  virtual proof that mentions it. The current `cpt_dicts` insertion precedes
  runtime fields and is insufficient. Preserve ordinary GADT evidence positions.
- Apply the structural Core domain check in the frontend. Start with closed,
  promotable, first-order data domains. Preserve lazy fields.
  Exclude existential, constrained, newtype, and unboxed index constructors in
  this increment. Do not give the user-defined `Nat` a wired compiler identity.
- Introduce the complete retained telescope before transporting an equation's
  ordinary argument and result types. Variable equations walk their supplied
  patterns and may leave a retained function result. Constructor equations still
  split one leading retained index. Transport across a remaining retained
  function now uses `DepFunCo` under its unchanged closed domain and `Many`
  binder. Changed domains and dependent erased binder kinds remain unsupported.
- Give GADT instantiation an explicit runtime-index scope rule. The current
  metavariable guards are intentional. Removing them without a scope check can
  let a predecessor escape. Evaluate expected-result constructor instantiation
  and scoped meta-assignment against Agda's context rules.
- Check CorePrep naming of constructor arguments. Naming `f (S k)` as
  `let t = S k in f t` changes its recovered index to `TermVarTy t`. Specify and
  check alias transport before claiming end-to-end constructor-argument support.
  Do not use an unchecked cast or equality from an unfolding. The implemented
  DEFAULT rule binds an equality between a fresh case binder and the exact
  scrutinee index. CorePrep emits it only for constructor WHNF. It names nested
  fields recursively and composes their proofs. It transports later payloads
  and the result across the supplied telescope. CorePrep preserves proof scope,
  and CoreToStg erases the virtual proof explicitly. A result-only cast is
  insufficient. A remaining fixed-domain retained function now uses `DepFunCo`.
  Preserve a cast on the partial function value; eta expansion can hide bottom.
  The `lazy`/`noinline`/`nospec` stripping path can still bypass preprocessing.
  Known partial-function heads now pass PMC without component rewriting.
  Source tests require a located incomplete-pattern diagnostic for an empty
  match on such a function. Check saved constructed partial results at unknown,
  bottom, and cyclic indices, then apply their later payloads. Do not catch
  panics or disable PMC in tests.
  The Agda let and dependent ANF references describe the preservation problem;
  they do not prove this proposed FC rule or its Haskell strictness behavior.
- Preserve partial and cyclic Haskell values. Check finite vector prefixes for
  `let n = S n`, a head from `replicate (S bottomNat) x`, and failure when its
  remaining spine needs `bottomNat`. Check `head bottomNat (VCons x undefined)`
  returns `x`. Agda's termination assumptions do not establish these properties.

Use one compiled worker with unknown `n` and `m` from IORefs. For lengths two
and three, `append n m (replicate n 42) (replicate m 7)` must observe
`[42,42,7,7,7]`. Also check zero and successor inputs. Reject a constant `VNil`
at unknown `n`, a vector at `n` where `S n` is required, and an unrelated runtime
index. Keep `Add n m` distinct from `Add m n` without a supplied proof.

### Arithmetic proof increment

Use the existing lifted `:~:` datatype to prove `Add n Z :~: n` by induction
on a retained runtime Nat. Transport a vector only after matching `Refl`.
Follow [the pinned Agda and singletons sources](REFERENCES.md#arithmetic-equality-proofs).

- [x] Select immutable constructor type arguments from the expected result.
      Keep full ordinary result checking, kind checks, and runtime-index scope.
- [x] Run finite proofs and vector transport at unknown lengths 0, 2, and 3.
      Check direct source execution and interface-only imported execution.
- [x] Preserve ignored proof and index thunks, including bottom and cyclic input.
      Check proof failure before vector-payload failure. Keep partial transport lazy.
- [x] Reject unproved equalities, unused proof arguments, and branch-evidence escape.
- [x] Complete both-stage build, existing registered regressions, CLI, and guide checks.

See [the Core contract](CORE.md#arithmetic-equality-proofs). This increment adds
no equality axiom, Core form, singleton wrapper, or interface format change.
Ambient-evidence normalization under Pi and general retained inference remain open.

### Generic consumer increment

Infer ordinary erased type arguments from values whose types contain retained
indices. Start with named argument spines and preserve ordinary expected-type
checking. Follow [the scoped Agda application sources](REFERENCES.md#generic-retained-argument-inference).

- [x] Extend the existing cache with read-only native argument classification.
      Keep complete immutable matching, kind and scope checks, and subsumption.
- [x] Check proof and vector consumers, nested `id` and `const`, and retained
      function values. Preserve ignored bottom and cyclic arguments.
- [x] Check linear proof use once and reject duplicated use with GHC-18872.
- [x] Complete the self-hosted regressions and interface-only CLI execution.
- [x] Check ordinary bidirectional typing and one Template Haskell expansion.
- [x] Compile the guide and finish ordinary smoke and formatting checks.

See [the contract](CORE.md#generic-retained-argument-inference). Native
meta-variable assignment and broader application inference remain open.

### Class consumer increment

Use ordinary class instances for retained proof and vector types. Follow the
[Agda instance-context sources](REFERENCES.md#retained-class-consumers).

- [x] Match the ordinary argument and result types after the constraint prefix.
      Keep the substituted prefix for ordinary dictionary instantiation.
- [x] Put evidence bindings at the application, below its runtime index binders.
      Keep enclosing Givens and report residual native constraints in scope.
- [x] Support native type-map keys with scoped equality. Check singleton and
      trie representations, alpha-equivalence, distinct indices, and map operations.
- [x] Check proof and vector `Show`, multiple native dictionary keys, and lazy
      consumers at bottom and cyclic indices. Keep Core and STG Lint enabled.
- [x] Reject missing instances and transport without equality evidence at the
      source location. Check ordinary class and inference controls.
- [x] Complete registered regressions, interface-only execution, guide, smoke,
      formatting, and the checkpoint.

Native maps are implemented at `1b37399cd5`; scoped class consumers at `17740f7135`.
See [the contract](CORE.md#retained-class-consumers) and the
[verified results](README.md#ordinary-class-consumers). Source-written native
class contexts are covered by the next increment. General meta-variable scope
and broader inference remain open.

### Source context increment

Check ordinary class and equality contexts after a retained binder. For example,
`foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String` must use an
incoming dictionary under the runtime `n` binder. Follow the
[Agda telescope and instance-pattern sources](REFERENCES.md#source-retained-constraints).

- [x] Admit class and equality predicates through the existing validity checks.
      Check constraint-tuple aliases too. Keep native quantified and irreducible
      predicates gated with located errors.
- [x] Split a variable-prefix equation at its first context. Check its remaining
      patterns, guards, and local bindings under the ordinary evidence lambdas.
- [x] Keep constructor equations at full value and dictionary arity before the
      case. Transport incoming dictionaries with the existing constructor proof.
      Include trailing contexts. Scope evidence over the whole match and local bindings.
- [x] Check incoming Givens without global instances, equality transport, and
      partial application at bottom and cyclic indices. Check rejection locations.
- [x] Complete both-stage builds, registered regressions, interface-only execution,
      ordinary controls, guide, smoke, formatting, and the checkpoint.

Use the existing Core forms and dictionary ABI. Keep the current native signature
interface format. Inner retained lambdas and general retained inference remain
separate work. The split requires nonforcing prefix patterns when value patterns
remain. This case also excludes `Strict`. A trailing context keeps the old prefix
shapes. Constructor equations keep their existing coverage and pattern limits.
See [the contract](CORE.md#source-retained-constraints).
Implementation: `670bcef166`. See the
[verified results](README.md#source-retained-constraints).

### Source case evidence follow-up

Native source cases generalize captured values and incoming index-dependent
Givens over a fresh index. The no-instance `OnlyGiven n` control now passes when
its case specializes a captured `Vec Int n`. It previously failed with located
GHC-39999, while equivalent constructor equations passed.

- [x] Pass index-dependent Givens with the generalized case captures. Transport
      them with each branch proof and preserve the full function arity.
- [x] Record lexical native evidence parameters at their scope entry. Preserve
      enclosing parameters in nested scopes. Exclude them inside static forms.
      Save and restore the evidence at an arrow scope boundary.
      Restore bracket-entry evidence when checking a nested typed splice.
- [x] Generalize predicates with native type substitution. Pass the original
      evidence after the index and before captured values. Handle CoVars through
      evidence applications and coercion transport.
- [x] Check no-instance dictionaries, derived tuple evidence, equality, nested
      cases, local bindings, and partial application through explicit source cases.
- [x] Complete both-stage builds, registered regressions, interface-only
      execution, ordinary controls, formatting, and the checkpoint.

Follow Agda's dependent-context generalization and instance-binder scope. Use
GHC's existing evidence representation and checked branch proofs. See
[the case contract](CORE.md#explicit-case-evidence) and
[the pinned compiler sources](REFERENCES.md#source-case-evidence).
Implementation: `fe7329d1b6`. See the
[verified results](README.md#source-case-evidence). Broader case patterns,
general retained inference, structural Pi unification, native coercion unfolding
codecs, and ordinary optimization remain open.

### Checked retained lambdas

A complete native signature checks named equations and anonymous lambdas through
one retained match dispatcher. Previously, the lambda form failed with GHC-83865.

- [x] Share the retained telescope and constructor dispatcher. Preserve invisible
      patterns, source match contexts, and named-binding usage scaling.
- [x] Check anonymous lambdas under their ambient usage. Keep ordinary lambda
      checking and inference unchanged.
- [x] Check outer erased binders, nested lambdas, later retained binders, source
      contexts, and both constructor lambda forms.
- [x] Check source arity at bottom and cyclic indices. Preserve linear usage and
      located diagnostics for invalid branches and unsupported inference.
- [x] Preserve the runtime binder and strictness for empty-domain lambda-cases.
      Keep empty source cases in their original context.
- [x] Complete both-stage builds, registered regressions, separate module
      execution, ordinary controls, formatting, and the checkpoint.

Follow [the contract](CORE.md#checked-source-lambdas) and
[Agda's expected-Pi and extended-lambda rules](REFERENCES.md#checked-retained-lambdas).
This step does not enable retained syntax in new type positions or general native
metavariable assignment. Keep broader inference and optimizer gates.
Implementation: `a0c493d5f9`. See the
[verified checkpoint](README.md#checked-retained-lambdas).

### Explicit higher-order arguments follow-up

Admit a retained function type in an ordinary argument position:
`applyAt :: foreach (n :: Nat) -> (foreach (m :: Nat) -> Vec Int m -> Vec Int m)
-> Vec Int n -> Vec Int n`, with `applyAt n f xs = f n xs`.
The parameter `f` is an ordinary closure argument. It is not a runtime index.

- [x] Admit this nested type in complete value signatures. Preserve each nested
      binder's scope. Keep type-level declaration and index-domain restrictions.
- [x] Admit ordinary-leading arrow signatures through the same traversal.
      Reject partial signatures even when their wildcard is outside the Pi.
- [x] Project both ordinary arrow arguments and results for ambiguity checking.
      Keep erased payload polymorphism and real signature types intact.
- [x] Check named functions and lambdas against the supplied argument type.
      Use the existing application checker and checked-lambda rule.
- [x] Instantiate ordinary helpers from a known retained function result with
      immutable type arguments. Preserve a genuine bottom closure. Check `id`
      around a lambda under a complete expected type.
- [x] Lift checked constructor proofs through fixed-domain retained closure
      types. Check constructor clauses and explicit case with captured indices.
      Preserve binder scope and transport with casts instead of eta expansion.
- [x] Verify separate-module execution with unknown runtime indices. Check that
      ignoring a bottom function stays lazy. Check unused bottom indices too.
- [x] Reject wrong vector lengths, escaping indices, unsupported domains, and
      function-valued runtime indices with located diagnostics.
- [x] Keep inferred aliases, general native meta assignment, Quick Look, and
      structural Pi unification gated. Test explicit outer erased polymorphism.
- [x] Complete both-stage builds, registered regressions, separate CLI checks,
      formatting, and the verified checkpoint.

The renamer and signature checker traverse ordinary arrow arguments and results.
Core Lint and validity check each contained retained telescope recursively.
Agda's expected-argument and scoped-Pi lambda rules provide the checking
precedent. See [the pinned sources](REFERENCES.md#explicit-higher-order-arguments).
Follow [the higher-order contract](CORE.md#explicit-higher-order-closure-arguments).
Implementation `64044341e8` and `94d7d8f2f3` pass the
[verified checkpoint](README.md#higher-order-retained-closures).

### Expected-type instantiation of retained workers

Use a complete expected retained function type to infer the outer erased
parameters of a known worker. Accept `applyAt @Int n lambdaId xs` and a fully
signed `alias = lambdaId` without requiring `lambdaId @Int`.

- [x] Add an opt-in immutable Core matcher. Preserve existing entry points,
      unification, and apartness behavior.
- [x] Compare valid same-domain retained binders under paired renaming and
      bound-variable escape tracking. Preserve family and multiplicity rules.
- [x] Add a separate known-native-worker expected-result route. Require NoQL,
      complete kind/scope-valid type arguments, exact result equality, actual
      type applications, preserved constraints, and final subsumption.
- [x] Check alpha-renamed/nested binders, rigid outer indices, repeated payload
      agreement, contexts, and a genuine bottom worker without eta expansion.
- [x] Reject captured inner indices, mismatched domains, and inconsistent
      payloads. Keep inferred aliases and general native meta assignment gated.
- [x] Verify runtime and interface-only clients, located diagnostics, both-stage
      builds, registered regressions, compatibility, formatting, and source hashes.

Follow Agda's expected-result implicit insertion and shared-context Pi comparison.
Use an immutable substitution in this GHC increment. Agda's contextual mutable
metas require separate scope machinery. See
[the pinned sources](REFERENCES.md#expected-type-instantiation-of-retained-workers).
Implementation `dae037074c` passes the
[verified checkpoint](README.md#expected-type-instantiation-of-retained-workers).

### M10: retained constructor field telescopes

Package a runtime Nat with a vector indexed by that same field. Follow Agda's
ordinary dependent constructor telescope. Reuse native Core Pi and index forms.
The full source target is:

```haskell
-- Compiles and runs in the source package example.
data PackedVec a where
  PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a

withPacked :: PackedVec a ->
              (foreach (n :: Nat) -> Vec a n -> r) -> r
withPacked (PackVec n xs) k = k n xs
```

- [x] Inspect Agda Sigma, record/constructor checking, application and LHS
  substitution. Record pinned sources and the historical DataCon difference.
- [x] Add optional retained field binders to DataCon metadata. Preserve the old
  builder API and ordinary knot laziness. Rebuild worker and wrapper Pi types.
  Validate ordered scope, fixed domains, multiplicity, representation restrictions
  and result nonescape. Keep runtime fields out of erased existentials.
- [x] Instantiate Core pattern fields sequentially. Substitute actual field Ids
  in later field types. Check fresh identities, field count, dependent payload
  identity and result nonescape in Core Lint. Add existing-fixture API coverage.
  Flat instantiated-field APIs reject retained metadata. Interface declarations
  now preserve field scope in the codec step below. Even a vacuous retained constructor
  stays outside native index domains. The API fixture checks nested Pi capture,
  scoped consumption and repackaging with a dictionary-function payload.
- [x] Verify the Core checkpoint in both compiler stages and the bootstrap API,
  registered tests, runtime fixture and standalone compatibility controls.
  Record the corrected source-gate harness separately from the first combined
  CLI invocation. See [the verified checkpoint](README.md#retained-constructor-field-checkpoint).
- [x] Preserve the telescope in interface declarations. Reject malformed scope
  and unsupported promotion with interface diagnostics. Version the format if
  the serialized declaration changes. Keep existing archives immutable.
  Encode aligned optional local names. Decode with fresh sequential Ids and
  deferred field checks. Expose metadata before recursive domain validation.
  Check duplicate/forward/self/inherited references, vacuous binders, distinct
  names after tidying, sibling scope and repeated imports. Preserve an immutable
  version-1 reader before configuring version 2. Verified binary declaration
  round trips, recursive-domain rejection and both header-version directions.
  See [the interface checkpoint](README.md#retained-constructor-interface-checkpoint).
The first source checkpoint uses one package constructor, prefix constructor
syntax, bare variable patterns for retained fields, and one eliminator equation.
Reject unsupported forms with located diagnostics. Keep pattern coverage checks
active. Multiple equations need coherent substitution of their distinct runtime
Ids; an ordinary value alias does not establish index equality.

- [x] Check source constructor declarations in order. `mkGadtDecl` keeps
  the retained suffix in `con_res_ty`. Rename this suffix with runtime Names;
  preserve ordinary constructor renaming. Check it as a telescope and apply
  `rejigConRes` only to its terminal package result. Preserve the source field
  metadata directly: `mkDepFunTy` can erase a vacuous binder. Keep the complete
  scoped telescope through generalization and zonking. Split the fields before
  result substitution. Substitute each field lazily to preserve the parent
  datatype knot. Validate the result shape after this knot is complete.
  The earlier kind pass must not inspect unfinished Nat constructor graphs.
  Check domain kinds there and validate supported domains in the final pass.
- [x] Traverse retained field scope in role inference and role validity. A
  scoped runtime index has a fixed closed domain. Continue checking ordinary
  payload parameters and their roles. Reject source promotion before requesting
  the cached promoted constructor. Add located diagnostics for unsupported
  declaration shapes, invalid domains and escaping fields. Automatic Typeable
  generation and validity recovery skip promotion of retained constructors.
- [x] Check source constructor patterns sequentially. Substitute each checked
  retained field Id into the later field types before checking those patterns.
  Keep runtime fields out of erased existentials. Use ordinary constructor
  pattern checking for `withPacked`; its retained callback argument does not
  make the outer function a native-index equation.
- [x] Preserve actual field identities during desugaring. `matchOneConLike`
  and PMC's `desugarConPatOut` reuse the checked field Ids instead of the gated
  flat `conLikeInstOrigArgTys`. PMC's speculative `instCon` instantiates fresh
  fields sequentially.
  `selectMatchVar` can retain a bare source field Id for one clause.
  An ordinary `bindMatchId`/`wrapBind` alias does not prove equality of runtime
  indices. Multiple equations require coherent substitution through each typed
  RHS, pattern and evidence, or checked alias transport. Do not route package
  elimination through the native-index constructor equation checker.
- [x] Count retained field Pi binders as runtime arguments in worker LFInfo and
  constructor layout checks. `RepType.countFunRepArgs` and `countConRepArgs`
  handle retained binders. Use each fixed lifted field domain;
  do not force its runtime value to determine its representation.
  Constructor impossibility checks must return conservative results before
  calling flat signature APIs. Decline product unpacking for retained packages.
  Nonreflexive constructor-cast pushing also needs scoped field substitution;
  preserve the cast until that rule exists. Profiling remains outside the subset.
- [x] Execute packaging, scoped elimination and repackaging of real Nat/Vec
  values through separate module interfaces. Inspect CorePrep/STG field layout.
  Check constructor WHNF, unused bottom/cyclic index, bottom vector, and demanded
  payload behavior. Do not count the API's dictionary-function payload as Vec
  execution or as proof of backend support.
- [x] Validate both stages, focused regressions and existing compatibility
  controls. Record exact commands, peaks, artifact hashes and remaining gates.

The source path starts at `mkGadtDecl`, `rnConDecl`, `kcConDecl` and `tcConDecl`.
`tcRetainedDataConPat` replaces each template field Id before checking the next
field. Erased existential freshening stays separate. The source package example
now compiles unchanged through a separate producer interface and object file.
Ordinary polymorphic payloads use the existing immutable result matcher. Read
resolved expected-type metas before this match; do not put a runtime index in
an ordinary mutable type meta.

Core metadata, interface declarations and the first source package execution
are implemented. General projections, records, strictness transformations, native
unfoldings and optimizer support remain separate work.

### M11: source filter with a runtime result length

Follow Agda's `Vec≤` filter for the stored length and vector. Keep the bound
proof separate. See [the pinned sources](REFERENCES.md#source-vector-filter).

- [x] Implement `filterVec` and `filterPacked` in the source example. Construct
  `S n` with `VCons` when the predicate retains an element. Return the recursive
  package when it rejects an element. Reuse existing Core and interface forms.
- [x] Accept direct package `case` patterns with a fresh multiplicity meta.
  Use GHC's ordinary `tcSubMult` check. Keep linear and rigid rejection checks.
- [x] Expand only the compiler-owned section helper compulsory unfoldings in
  native simplification. Reuse GHC's templates and substitution. Check both
  partial section WHNF and later application demand.
- [x] Check bootstrap separate-module execution with Core and STG Lint. Observe
  lengths 3, 2, and 0 for runtime cutoffs. Check element order, empty input,
  bottom elements, bottom predicate, partial spine and unused packages.
  Reject a wrong retained-branch length with a located GHC-05617 diagnostic.
- [x] Build the self-hosted compiler and run the same source program. Run the
  focused registered regressions, Core API and bootstrap runtime fixture.
  Record commands, resource peaks, artifact hashes and remaining source limits.

The current filter scans a finite input spine before returning a package. Its
stored elements and length fields remain lazy. Productive infinite filtering,
the output-length bound proof and broader package patterns remain later work.

### M12: source filter with a checked length bound

The source example implements the bound component of Agda's `Vec≤` result.
It uses the current constructor and proof forms. Both compiler stages pass
all 58 source observations. The runtime fixtures check 19 package/bound
rejections. The focused registered regressions and Core API also pass. See
[the checkpoint](README.md#source-filter-bound-proof-checkpoint).

```haskell
data LE (n :: Nat) (m :: Nat) where
  LEZ :: LE Z m
  LES :: LE n m -> LE (S n) (S m)

data BoundedVec a (limit :: Nat) where
  PackBounded :: foreach (len :: Nat) ->
                 Vec a len -> LE len limit -> BoundedVec a limit

filterBounded :: forall a limit.
                 (a -> Bool) -> Vec a limit -> BoundedVec a limit
```

- [x] Probe these declarations from source. Check all three runtime fields and
  their ordered scope through a separate interface. Reuse ordinary GADTs for
  `LE`; add no primitive dependent-pair form.
- [x] Construct `LEZ` for empty input and `LES proof` for a retained element.
  For a rejected element, use structural right weakening
  `LE n m -> LE n (S m)`. General transitivity and an external solver are not
  required for this proof.
- [x] Observe the vector, computed length and finite proof separately. Reject
  an incorrectly reconstructed bound with a located diagnostic. Check that
  package WHNF and vector observation can leave the proof unevaluated.
- [x] Validate source execution and the existing compatibility controls. Record
  any newly exposed source gate before changing compiler rules.

Agda's bound field is irrelevant. The Haskell `LE` field is an ordinary
lazy runtime field. General proof erasure remains separate work.
A bound interpretation requires terminating indices and a terminating `LE`
proof. Bottom and cyclic proofs remain valid
Haskell expressions. This exercise does not establish totality, predicate
selection correctness or productivity on infinite input. See
[the Agda filter provenance](REFERENCES.md#source-vector-filter).

## Acceptance checks

| ID | Required result |
| --- | --- |
| B01 | `choose True` returns an `Int`; `choose False` returns a `Bool`. |
| B02 | A runtime-unknown Boolean selects and consumes the correct result. |
| B03 | `keep b (choose b)` reuses one runtime binding and one index. |
| B04 | Boolean branches supply checked nominal refinement. |
| B05 | Mismatched arguments and branch result types are rejected. |
| B06 | Separate opaque Boolean bindings do not become equal from equal-looking definitions. |
| B07 | Aliases and shadowing preserve the specified binder identity and scope. |
| B08 | An index cannot escape a local or existential binding. |
| B09 | An ignored bottom Boolean stays non-strict, including partial application and `seq`. |
| B10 | Inspecting a bottom Boolean diverges. Index knowledge does not replace its runtime value. |
| B11 | Substitution and beta reduction preserve types without capture. |
| B12 | Case transformation and let floating preserve dependency scope. |
| B13 | Invalid coercions and representational substitutions cannot forge nominal index equality. |
| B14 | Unoptimized and optimized compilation pass Core Lint and agree on defined results and tested termination behavior. |
| B15 | Interfaces round-trip and cross-module execution passes; feature-disabled programs remain compatible. |

Distinguish Core construction, frontend behavior, optimization, runtime execution,
and module integration. M3 through M5 validate native Core construction and the
restricted bytecode path. The singleton probe supplies comparison evidence for
some values and laziness cases. Restricted source `keep`, constructor equations,
and their composition now execute through bytecode and machine-code module
compilation. Expected-type-directed source `case` refinement now reuses the
constructor rule. Ordinary optimization, broader case refinement, and native
coercion unfolding serialization remain open. Fixed-domain retained congruence
passes the bootstrap kernel and runtime checks; its broader verification is
pending. Restricted native signature interfaces
and interface-only consumers pass as described in M8.

### M13: nested source package patterns

Use a complete source consumer before extending the match compiler:

```haskell
headPacked :: PackedVec a -> Maybe a
headPacked (PackVec n VNil) = Nothing
headPacked (PackVec n (VCons x xs)) = Just x
```

Follow Agda's constructor telescope and source clause discrimination. Preserve
Haskell's clause order and field demand. See
[the primary sources](REFERENCES.md#source-package-patterns).

- [x] Keep retained index fields as bare source variables. Check ordinary
  fields with `tcConArg`, including nested GADT patterns and wildcards. Keep
  the one-constructor, prefix, unrestricted and strict-package restrictions.
- [x] Compile each retained package equation with its own actual field Ids.
  Match the original shared scrutinee and combine failure continuations in
  source order. Do not merge clause-local index Ids with ordinary let aliases.
  Keep ordinary constructor-family compilation unchanged.
- [x] Use checked outer field pattern types in PMC desugaring. Retain the
  existing sequential speculative constructor instantiation. Add no new Core
  or interface representation.
- [x] Execute nested vector and proof patterns, wildcard proof fields and a
  runtime failed guard. Check partial tails/elements, unused bottom indices,
  demanded payload/proof, catchall order and bottom packages. Reject an index
  escape through a nested pattern with a located diagnostic.
- [x] Validate both compiler stages, separate source interfaces, Core/STG Lint,
  focused registered regressions and the existing API/runtime controls. Record
  source restrictions, commands, resource peaks and artifact provenance.

Separate cases avoid rewriting typed patterns, evidence and RHSs to a shared
index binder. A later row reuses the already shared scrutinee. This can produce
more cases than a decision tree with shared fields. Optimization remains
separate work. General projections and constructor patterns in retained index
fields remain outside this increment.

The implementation and validation are recorded in
[the checkpoint](README.md#nested-source-package-pattern-checkpoint).
Next, probe `unconsPacked :: PackedVec a -> Maybe (a, PackedVec a)`. Match the
stored length with `Z` and `S k`. In the successor branch, match the vector
and package its tail at actual runtime `k`. Keep the predecessor, head and
tail lazy until a consumer demands them. This source target is verified in
M14 below. It uses the existing checked case rule.

### M14: source vector uncons at an actual runtime predecessor

The initial source probe compiles with Core and STG Lint in the existing
self-hosted compiler. No compiler change is required by that probe.

```haskell
unconsPacked :: forall a. PackedVec a -> Maybe (a, PackedVec a)
unconsPacked (PackVec n xs) = case n of
  Z -> Nothing
  S k -> case xs of
    VCons x rest -> Just (x, PackVec k rest)
```

Follow Agda's indexed vector `uncons` relation. Use the actual runtime `k`
from `S k`, rather than reconstructing it from the erased vector index.
The existing checked case rule transports captured `xs` into the successor
branch. Ordinary GADT evidence then relates the vector tail to `k`.
See [the pinned sources](REFERENCES.md#source-vector-uncons).

- [x] Add the source worker and a source consumer that repeatedly opens its
  tail packages. Keep Core, interface formats and compiler rules unchanged
  unless a concrete source failure requires a change.
- [x] Check IORef inputs, empty and nonempty results, finite element order and
  tail lengths. Observe the result, pair and tail package separately. Check
  predecessor, head and tail demand independently. The zero branch must not
  force its vector; a successor branch must inspect the vector constructor.
- [x] Reject a tail packaged with an incorrect or independent runtime index
  using a located diagnostic. Probe the diagnostic before pinning it.
- [x] Run both compiler stages through separate source interfaces with Core
  and STG Lint. Update the existing runtime regression and run its registered
  normal way. Record exact commands, resource peaks and artifact hashes.

The full source observer exposed a later boundary: Tidy globalises top-level
Ids that typechecking accepted while they were local. `S bottomLength` then
fails Core Lint. Before tidying a native RHS, create lazy local aliases for
its free top-level index Ids and substitute the complete RHS, including types
and coercions. Keep the alias RHSs as references to the original top bindings.
Use GHC's existing deterministic FV traversal, Core substitution and CorePrep
alias guard. Keep ordinary RHSs and the local-index grammar unchanged.
Preserve the failing source expression as a regression observation.

This exercise does not infer a runtime predecessor from an erased type. It
does not add general projections, retained index patterns or proof erasure.
Finite results and bounded partial/cyclic observations do not prove totality.

The next source probe compares two independent runtime lengths and returns
`Maybe (n :~: m)`. Reuse ordinary `Data.Type.Equality` evidence. A matching
`Refl` branch should permit a source consumer to zip the two vectors and
package the result. This target is not verified. `Nothing` would carry no
refutation proof. Preserve payload laziness on a length mismatch. Probe the
current equality-Given path before proposing a compiler rule. See the
[pinned comparison, congruence and zip sources](REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe).

### M15: source runtime equality and aligned vector zip

Source workers: `f3350988c3`. Application checking: `78460ce4f4`.
Both compiler stages pass 136 source observations with Core and STG Lint.
The bootstrap and registered runtime fixtures pass 27 package/bound
rejections. Four standalone equality-scope errors have pinned codes and source
locations in both stages. All five focused registered regressions pass.

```haskell
sameNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)
zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipPacked :: PackedVec a -> PackedVec b -> Maybe (PackedVec (a, b))
```

`sameNat` uses one variable equation with nested cases. Its recursive
`Just Refl` branch proves successor equality by ordinary congruence.
`zipPacked` matches that proof before passing two captured vectors to
`zipVec`. The solver keeps runtime indices opaque. Core and interface rules
are unchanged. Follow the [pinned Agda equality and vector relations](REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe).
The Haskell result carries positive evidence only. `Nothing` carries no
refutation proof. Equal-length success compares both finite Nat spines before
returning `Just`. Vector fields, tuple components and tails remain lazy until
the consumer demands them.

- [x] Add source workers, exports, Haddock comments and a GHC Note with proof
  scope, Haskell demand and Agda provenance.
- [x] Read both lengths independently through IORefs. Check equal and unequal
  finite inputs, empty vectors and element order after runtime filtering.
- [x] Observe comparison, result and package WHNF, vector tags, pair components
  and tails separately. Mismatches ignore both vectors. Partial predecessors
  remain lazy until comparison needs them. Preserve package-pattern order.
  Do not force an equal cyclic comparison.
- [x] Pin located errors for independent `Refl`, zip without equality,
  unmatched proof values, branch-local equality escape and a linear index.
- [x] Complete both compiler stages through separate source interfaces with
  Core/STG Lint. Run the updated runtime fixture and focused registered normal
  way. Record commands, resource peaks and artifact hashes. Preserve the
  previous-format reader and failed probes.

The full consumer required three bounded application-checking changes:

1. Normalize Id type and multiplicity metadata with the within-TC `zonkId`.
   Validate immutable index choices against the authoritative local or
   same-module binding. Preserve Name, Unique, local identity, equal closed
   domain and Many multiplicity. Reject imported GlobalIds, unresolved or
   rigid domains, linear indices and missing bindings. Do not use the final
   zonker's recursive Id environment.
2. Reuse algebraic result-candidate matching for known ordinary Ids. Select a
   complete index substitution while an ordinary payload type is unresolved.
   Preserve the actual instantiated type. Final subsumption solves ordinary
   payload equalities and checks every index equality. Expected matching can
   retain an ordinary tau arrow after the written arguments. Argument
   inference still requires saturation.
3. Specialize a fully applied wired-in `($)` only when a complete result is
   known and its named right operand has a native anchor. A result independent
   of the named first operand's invisible binders can supply the result for a
   discarded `do` statement. Read that signature without elaborating operands
   or expanding splices. Infer and cache the right operand once. Match kinds
   before types to determine both representation binders. Require complete
   choices, kind agreement, exact result/domain types, valid runtime scope
   and concrete original `idConcreteTvs` choices. Apply the choices to the
   original operator and use NoQL through instantiation, result checking and
   cache resumption. Final checking retains the original expected type.

Decide dollar eligibility before right-operand inference. Higher-rank
callback domains retain their existing expected-checking path. In particular,
`withBounded` needs an expected result before checking its retained callback.
If an attempted immutable match fails after inference, keep the cached
operand and use ordinary NoQL checking. Do not elaborate that operand again.
Emit its saved usage once under the final argument multiplicity. Other Quick
Look applications keep their current path.

The original source observer is unchanged. These routes do not put native
syntax in mutable type or Quick Look variables. They add no equality axiom,
refutation proof, proof erasure, native optimization or totality claim.

### M16: source append with a computed result length

Both compiler stages pass 180 source observations at `fba00f8f0c`, with
compiler changes at `491c5fb0c9`. The runtime fixtures pass 34 package/bound
rejections. Seven standalone errors have pinned codes and source tokens in
both stages. All five focused registered regressions pass. Return a runtime
sum with ordinary equality evidence, then use that evidence to package an
appended vector. Follow
the [pinned addition, vector append and congruence definitions](REFERENCES.md#computed-length-append-next-source-probe).

Native module classification includes local retained constructor worker
types before simplification, tidying and code generation. This covers a
declaration-only producer before implicit workers are generated. Existing
native option restrictions remain. Constructor result projection applies
only to a fully applied real constructor with validated retained metadata.
It traverses the current signature in metadata order. A vacuous retained
binder can use its canonical ordinary Many arrow with the same closed
domain. Only outer erased parameters receive immutable choices. The actual
instantiated telescope and normal field checks remain. Arbitrary functions
and partial constructors keep their previous path.

```haskell
data Sum (n :: Nat) (m :: Nat) where
  Sum :: foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m

sumNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Sum n m
appendPacked :: PackedVec a -> PackedVec a -> PackedVec a
```

- [x] Probe the declaration and `Sum m Refl :: Sum Z m` separately.
  Construction must instantiate the erased `m` with the captured runtime
  index before allocating mutable instantiation variables. Preserve the
  original failed probes. Use the constructor-only result match with
  existing kind, scope and final subsumption checks. Check partial VTA
  against the current signature and an unused retained field.
- [x] Define `Add` and source append by recursion on the first length.
  Define `sumNat` with the same recursion. In the successor case, match
  recursive `Refl` before constructing `Sum (S p) Refl`. Reuse family
  reduction and ordinary equality congruence.
- [x] Open both packages, match `Sum p Refl`, and repackage the appended
  vector at `p`. Reject forged sums, missing equality elimination, swapped
  indices without a lemma, and an escaping result index.
- [x] Check independent runtime inputs and finite element order. Observe
  package, equality, length and vector demand separately. This
  `sumNat` forces the entire first Nat spine before returning a constructor.
  Do not claim productivity for a cyclic first length. Check that a finite
  first length can leave the second length and vector fields lazy.
- [x] Validate both compiler stages, existing controls and resource limits.
  Record provenance, located errors and reproducible artifacts.

### M17: constructor patterns on retained package fields

Compiler implementation: `ab985a150e`; import cleanup: `f423a0ce97`.
Source and runtime fixture: `e54802bcfd`. Both compiler stages pass 209 source
observations, including the previous 180. The bootstrap and registered runtime
fixtures pass 40 package/bound rejections. Seven new pattern errors and the
seven existing sum/append errors have pinned codes and primary source tokens
in both stages. All five focused registered regressions pass.

Source uncons uses `PackVec Z _` and `PackVec (S k) (VCons x rest)` equations.
The zero row keeps ignoring the vector field. The separate `PackVec Z VNil`
probe demands its constructor. The implementation reuses the checked native
constructor equality. Each equation keeps its runtime Ids and source-order
failure continuation.

The initial source probe rejected the index constructor at its original span
with GHC-83605. Pattern checking now shares the existing closed constructor
resolver. An outer generated as-pattern binds a fresh whole-field Id. The
inner constructor alternative carries its nominal constructor proof. Later
field types use the existing native coercion checker. Coercion patterns keep
their original outer types. Each package equation keeps its existing match
compilation and failure path. The backend already accepts this proof and an
ordinary DEFAULT continuation.

- [x] Probe a constructor head with bare variable subfields. Preserve a fresh
  Id for the whole stored index, then scope its nominal constructor equality
  over subsequent field patterns and the RHS. Use the actual predecessor.
- [x] Reuse native coercion checking. Preserve original outer field types
  through checked pattern coercions. Follow the pinned Agda later-telescope
  substitution in [source package patterns](REFERENCES.md#source-package-patterns).
- [x] Preserve matching order across other arguments, guards and views.
  Moving fields into RHS cases can change that order. Keep ordinary row
  failure instead of requiring complete source-case alternatives.
- [x] Check bottom indices, lazy elements, first-row failure, repeated
  equations and scope errors. Keep diagnostics on the original patterns.
  Preserve the existing rejection of lazy package patterns.
- [x] Run the final self-hosted source checks and all five registered
  regressions. Freeze the build inputs, source, provenance and output evidence.

### M18: source reverse with the original stored length

Compiler implementation: `12bfbef0b3`. Source and runtime fixture:
`9fefa1a496`. Both compiler stages pass 249 source observations and seven
standalone reverse errors with pinned codes and primary source tokens. The
runtime fixtures pass 47 package/bound rejections. Signed argument controls
and all five registered regressions pass. Exact inputs and source evidence
are frozen under `_build/verification/native-reverse-source-`.

`reverseNative :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n` preserves
the worker's input index. The wrapper stores that original runtime `n`.
The reverse-append accumulator has result `Vec a (Add n m)`. It follows the
[pinned Agda reverse and addition proofs](REFERENCES.md#length-preserving-reverse-next-source-probe).
No new Core rule was added.

- [x] Admit complete expression signatures during native-anchored argument
  inference. The full consumer exposes `const (42 :: Int) (rightZero n)`:
  nested inference has no expected result to determine the first parameter.
  Keep classification read-only. Exclude wildcards, outer quantified
  variables, written inner quantifiers, retained binders, contexts and type
  splices. Infer and cache the signed expression once.
  Keep complete immutable matching and final argument/result checks.
- [x] Probe `Add n Z :~: n` and `Add n (S m) :~: S (Add n m)` by runtime
  Nat induction. Match the proofs before changing the accumulator's index.
  Reuse the existing right-zero proof precedent in the runtime fixture.
- [x] Run finite lengths through IORefs and a separate producer interface.
  Check result lengths and reversed element order independently. The native
  worker's signature relates the lengths. It does not specify element order.
- [x] Preserve package WHNF and lazy elements. Check demanded partial indices,
  input spines and proofs with named errors. Do not claim productivity for
  cyclic lengths or erasure for ordinary `:~:` values.
- [x] Reject a wrong result index and a missing arithmetic transport at their
  source expressions. Preserve the existing source and ordinary Haskell checks.

The first proof implementation can traverse the remaining Nat spine at each
accumulator step. Do not claim linear total runtime. Ordinary GADTs can also
express length-preserving reverse. This probe tests composition with retained
runtime indices and the existing source proof path.

- [x] Run the final self-hosted source checks, annotation controls and all
  five registered regressions. Freeze the compiler inputs and source evidence.

### M19: constructor equations with later retained arguments

Compiler implementation: `fdf23acaea`. Source/runtime fixture: `dc44d9d25c`.
Both compiler stages pass 269 source observations and eight new pinned source
errors. The previous seven reverse errors pass too. The runtime fixtures pass
55 package/bound rejections. All five registered regressions pass. Local-comment
cleanup is `a367bd0a7a`; the missing-proof rejection is `6f7d5f7e5f`. Keep the complete M18 source program as the
reference. Convert `addSucc` to constructor equations only after the compiler
accepts them. Record ordinary value, dictionary and retained index positions
separately. Keep original source equations for error contexts after renaming.
Reject quotations at their expression spans before a resolved-name rewrite.
The context-order probe also requires the signature selector, renamer and kind
checker to continue through qualified bodies. Keep ordinary constraint checks.
Check a leading class context as well as evidence between retained binders.

The first M18 probe used `addSucc Z m` and
`addSucc (S k) m`. It failed with GHC-83605 at the first clause because
`tcRetainedConArgs` does not consume a later `DepFunTy`. The result-wrapper
message describes the wrong limitation. The saved probe is
`_build/verification/native-reverse-source-probes-v1/`.

- [x] Choose a shared runtime Id for each later retained argument and
  instantiate its codomain in telescope order. Require bare variable patterns
  initially. Do not represent distinct source Ids as equal through aliases.
- [x] Preserve each row's resolved source names through a checked renaming
  step. Cover RHSs, guards, local bindings and stored free-name sets. Inspect
  the existing `rewriteRetainedCase` precedent. Keep quotation exclusions and
  accurate source spelling and spans.
- [x] Keep all source argument lambdas before constructor discrimination.
  Preserve dictionary order, later field transport and primitive proof scope.
  A bare later retained argument must keep its actual shared Id at the lambda.
- [x] Compare constructor equations with the existing explicit-case source
  worker. Check partial applications, bottom indices, payload order, two rows
  with different variable names, scope escape and wrong proofs. Use located
  diagnostics for unsupported later patterns and quotation cases.

Follow the pinned Agda constructor telescope and specialized RHS checking in
[source package patterns](REFERENCES.md#source-package-patterns). This requires
frontend identity handling. No new Core equality rule is proposed.

- [x] Run the self-hosted source program and exact-location errors. Run all
  five registered regressions. Freeze final input, source and output evidence.

### M20: constructor equations over two runtime indices

Both compiler stages and all five registered regressions pass. Rewrite the
existing nested-case `sameNat` as four
constructor equations for `Z/Z`, `Z/S`, `S/Z` and `S/S`. Its result remains
`Maybe (n :~: m)`. The recursive success branch eliminates the predecessor
proof before it returns `Just Refl`. Keep the case implementation as a reference.
The second constructor column can follow ordinary value arguments. Other
retained positions keep M19's bare-variable rule. Keep the one-column checker
as the existing path. Compose the first nominal transport with a second
transport over its result type. Locate duplicate pairs at their later row,
missing pairs at the group, and unsupported patterns at that pattern.

- [x] Admit an initial complete matrix of two retained constructor columns.
  Require bare constructor subfields, unique constructor pairs and no overlap.
  Keep contexts outside this initial matrix subset until all transported
  bindings can sit beneath every proof they reference.
- [x] Keep shared whole-argument Ids. Bind each row's actual subfield Ids and
  one checked nominal constructor proof per index. Compose refinements in
  telescope order for later types and the expected RHS.
- [x] Preserve each row through desugaring. Ordinary constructor grouping can
  alias distinct native field/proof Ids. Extend the existing separate-row case
  strategy for proof-bearing native alternatives. Keep each actual row
  field/proof Id. Do not assume aliases give equality.
- [x] Preserve all source argument lambdas and left-before-right demand.
  Compare finite equality results, partial WHNF, error markers and mismatches
  with lazy successor predecessors. Equal cyclic inputs have no termination
  claim. Reject incomplete/duplicate matrices and false `Refl` branches.

Follow [the pinned four-clause Agda example and case-tree compiler](REFERENCES.md#two-index-constructor-equations).
No new equality rule is proposed.

- [x] Run self-hosted source and located errors, then all five registered
  regressions. Freeze final inputs, source, harness and outputs.

An unused retained binder becomes an ordinary arrow. Keep both indices free
in rejection result types. A proof result such as `Maybe (n :~: m)` checks the
dependent path even when every RHS returns `Nothing`.


### M21: ordered trailing variable fallback

Both compiler stages and all five registered regressions pass. Express the same worker with `Z/Z`, recursive
`S/S`, and a final both-variable row returning `Nothing`. Keep the four-clause
and nested-case versions as references. This initial probe does not add
arbitrary wildcard matrices, guards or interior contexts.

Represent each row as an optional constructor pair. A constructor row keeps
M20's two proofs. The final variable row uses zero proofs and reflexive
transports. Rename both variable Names to the shared whole-index Names before
checking. Keep those typed VarPats unwrapped. Use GHC's existing `PgCon` and
`PgAny` blocks and failure continuation. No backend or Core form is needed.
Reject an earlier or repeated variable fallback at a source pattern. Retain
constructor-pair uniqueness and require complete pairs when no fallback exists.
Test a missing first constructor that reaches fallback without demanding the
second index. Repackage fallback payloads with their original indices to check
identity, not aliases. Reject a forged fallback `Refl` and wrong-index payloads.

- [x] Follow the pinned Agda coverage split-tree and LHS substitution code in
  [two-index constructor equations](REFERENCES.md#two-index-constructor-equations).
  Inspect GHC's ordered match groups and failure continuations before choosing
  a row representation. A variable row needs no constructor equality proof.
- [x] Preserve each constructor row's actual fields and each variable row's
  shared whole-index identities. Check its unspecialized RHS without assuming
  equality from aliases. Keep fallback execution outside failed row proofs.
- [x] Check coverage with the terminal fallback and reject unsupported row
  arrangements at source positions. Preserve all argument lambdas and row
  order. Compare finite equality results, partial WHNF and bottom demand with
  both source references. Re-run the existing false-proof rejections.
- [x] Run both compiler stages and the five registered regressions. Keep M20
  evidence frozen and collect separate M21 inputs and outputs.

### M22: anonymous terminal fallback

Verified with frontend `7b7586bfe0` and source/fixture `2435b1a7df`. Both
compiler stages and all five registered regressions pass. Permit `VarPat` or `WildPat` in each selected
index column of the one terminal fallback. This includes `_ _`, `left _`
and `_ right`. Keep constructor/variable and constructor/wildcard rows,
guards and interior contexts outside this probe. Preserve M21 evidence.

Before TC, replace each selected wildcard with a `VarPat` carrying that
column's existing shared whole-index Name. Do not allocate a fresh index Id
or add a let alias. Named columns keep M21's resolved-name substitution.
Anonymous columns introduce no source-visible name. Keep both typed index
VarPats outermost and unwrapped. This lets the existing pattern checker and
match-variable selector preserve the Ids used by later types and the result.

The fallback uses no constructor proofs. Its transports remain reflexive.
Check its RHS against the incoming unspecialized result type. Failed prefix
rows supply no evidence to it. Preserve source argument lambda order and
ordinary match failure and demand. Check quotations on every fallback row,
even when both selected columns are anonymous and no name substitution is
needed. Use `tcMatchWithSource` to retain the original Match for errors.
Keep wildcard and RHS locations. Existing constructor, ordinary argument and
unselected retained-pattern restrictions remain in force.

Follow [the pinned anonymous-context and GHC wildcard rules](REFERENCES.md#anonymous-terminal-fallback-equations).
No new Core, backend, interface or equality rule is proposed.

- [x] Inspect Agda's anonymous variable handling and GHC's wildcard tidying.
  Record the provenance before selecting generated index binders. A wildcard
  must preserve the shared whole-index identity without introducing equality.
- [x] Check final variable/wildcard combinations against the original result
  type. Keep diagnostics at the source wildcard or RHS. Reject a forged
  fallback `Refl` and preserve payload scope.
- [x] Compare the wildcard worker with named fallback, four-clause and case
  references. Preserve partial WHNF, missing-constructor demand and the order
  of failures. Run both compiler stages and the five registered regressions.

### M23: checked local payload signatures

Implemented with frontend `d45eac2a85` and lexical validity metadata fix
`da16ec16e6`. Source/API checks are in `58fe0a1e10`, with formatting at
`e4766d99ac`. Both stages pass all 343 source observations, preserving the
previous 331. Each passes 53 standalone located errors and four ordinary
controls. The API/runtime fixture passes 87 package/bound rejections. All five
registered regressions pass. Use a complete local value signature that mentions an
already-bound eligible runtime index under `ExperimentalDependentTypes`:

```haskell
n <- readIORef cell
let xs :: Vec Int n
    xs = replicateNative @Int n 7
```

Agda checks the declared type in the current context, then checks the value
against it. Follow [the pinned checked-let rules](REFERENCES.md#checked-local-payload-signatures).
GHC must use the actual local index Id. Do not unfold the index's RHS or derive
index equality from a let alias. Unsigned inference remains unsupported by M23.

Restrict capture to complete local value signatures under the experimental
flag. The binding renamer supplies `LocalBindCtxt` and the current group Names;
`TypeSigCtx` alone does not identify a local binding group. Do not enable this
capture for top-level, class, declaration, expression or pattern signatures.
Preserve all existing type-position, forall, context and staging restrictions.

An existing erased type-variable binding takes precedence. Otherwise, consider
only unqualified free type-variable spellings with a local term Name in the
current lexical reader environment. Install a signature-local lookup alias
before collecting implicit forall variables. Keep the actual Name in the value
namespace and record it as a free dependency of the signature. Explicit and
nested type binders keep their normal scope. Names with no local capture
candidate keep ordinary type-variable handling.

Exclude current-group Names using the binding context's group set. A recursive
local group already places these Names in the reader environment. Do not treat
them as earlier checked indices. Do not fall back to an outer same-spelling
term binder hidden by a current binder. Globals and future bindings are not
capture sources. Preserve the selected Name through TC; do not invent an Id
or silently replace failed runtime capture with erased quantification.

TC must find the actual already-bound local Id in its local environment.
Read assigned type and multiplicity metadata with the existing zonker, keeping
the Name and Unique. Require a non-CoVar local Id, a supported closed index
domain and unrestricted multiplicity before constructing `TermVarTy`. Scope
and metadata checks supply no equality and perform no runtime evaluation.
Validity also zonks the raw lexical Id before comparing it with the captured
occurrence. Assigned IO-bound metadata must not cause a false scope failure.
Keep the same Unique and require both domains to agree.

Compatibility is intentional under the experimental flag. If runtime `m` is
in scope, free `m` in `depth :: LE n m -> Int` captures that index. Write
`depth :: forall n m. LE n m -> Int` to retain erased polymorphic intent.
The explicit binders take precedence. RequiredTypeArguments already selects
term-variable capture in upstream GHC; preserve that independent behavior.

- [x] Capture signature-local Names before implicit forall collection, with
  erased-variable precedence, lexical shadowing and current-group exclusion.
  Preserve ordinary behavior with the flag off and outside local signatures.
- [x] Admit captured indices only for a complete renamed signature. Reject
  named and anonymous signature holes when capture is requested. Validate
  actual local Ids without native meta assignment or unsigned inference.
  Report unavailable or invalid captured indices at their source occurrence;
  preserve ordinary type errors for wrong-index payloads. Add a GHC Note
  stating these scope rules and the distinction from definitional equality.
- [x] Use the signed local payload twice after reading its index from IORef.
  Check wrong-index payloads, shadowed Names, bottom and cyclic indices,
  payload sharing and signature errors. Cover unavailable same-group indices,
  partial signatures and unsigned aliases with located errors. Check an
  ordinary implicit forall, erased-variable shadowing and flag-off behavior.
  Bootstrap checks preserve the source demand controls. No Core change was
  needed.
- [x] Run self-hosted source, located errors and ordinary controls, then the
  existing registered regressions. Keep frozen M22 evidence unchanged.

### M24: direct do bind of a saved native action

Verified in both compiler stages with 360 source observations, 60 standalone
located errors and four ordinary controls. The API/runtime fixture passes 94
package/bound rejections. All five registered regressions pass. The ordinary
source program retains its actual index without a conversion or package:

```haskell
n <- readIORef cell
let action :: IO (Vec Int n)
    action = pure (replicateNative @Int n 7)
xs <- action
pure (toList xs)
```

The signed action checks. Nine isolated baseline probes at `0e979288f8` fail
with GHC-83865. They cover saved and direct actions, do and explicit bind,
checked and inferred ordinary results, ignored payloads, and a native-result
inference boundary. The full target keeps the known action's actual native
payload index through ordinary do expansion and the continuation binder.
Evidence uses `_build/verification/native-action-bind-*`.

At fork commit `da16ec16e6`, `GHC.Tc.Gen.Expr` at line 582 expands ordinary
`HsDo` through `expandDoStmts`.
`GHC.Tc.Gen.Do` at line 101 translates a bind to the ordinary operator applied
to the action and a generated continuation. Inspect the application checker,
not the legacy syntax-operator path. In `tcRetainedArgInstEnabled`, line 1872
zonks cached actual types. Lines 1903–1908 seed `result_subst` from the raw
expected type. In the isolated `SavedChecked` trace, result and monad metas
are assigned `[Int]` and `IO` at lines 1274 and 1303 before the action bind at
line 2304. The action's exact native type is cached, but matching falls back
to fresh mutable input parameters. Read existing expected assignments before
immutable matching. This does not require evaluating the index.

- [x] Isolate direct action binding and preserve its current source failure.
  Inspect the expanded `(>>=) action (\xs -> rest)` and immutable candidates.
- [x] Read an available expected type with `readExpType_maybe`, then zonk it
  before seeding immutable matching. Leave an empty infer cell untouched.
  Check this increment against the saved baseline probes before extending it.
- [x] For a successful partial match anchored by a known native input, retain
  its fixed choices and instantiate undetermined ordinary result parameters
  with standard fresh TauTvs in telescope order. Preserve substituted kinds.
  A failed match is not a partial match. Native index parameters still require
  immutable choices, and native results cannot fill ordinary metas. Check a
  local do expression whose ordinary result has no complete expected type.
- [x] If result-seeded generic matching fails, try a separate exact joint
  argument match from the empty substitution. Inline `pure` can infer
  `f (Vec Int n)` before ordinary checking assigns `f := IO`. Preserve that
  actual head and let final result subsumption solve ordinary head/result
  metas. Do not reuse a failed argument match or weaken index equality.
  Do not freshen a missing binder whose substituted kind is a native domain.
- [x] Use a matched native expected-result choice as an anchor, including a
  partial result seed. Check polymorphic bottom payloads at their known native
  domains instead of inferring fresh indices. Match result kinds before result
  types to determine representation parameters immutably for `error`. Preserve
  its original HasCallStack evidence and all concreteness checks.
- [x] Preserve kind, scope, exact-domain,
  concreteness, NoQL, once-only usage and local evidence safeguards. Keep final
  argument checking and result subsumption authoritative.
- [x] Keep the ordinary bind operator, generated continuation and dictionary
  order. Add no special runtime index witness, equality rule or do syntax.
  Preserve unsupported unsigned native inference and general Quick Look gates.
- [x] Compare direct and saved actions with explicit bind syntax. Check finite
  IORef inputs, shared payloads, wrong-index errors and delayed index/payload
  demand. Run both compiler stages and the existing registered regressions.

Follow [the pinned Agda do translation](REFERENCES.md#direct-do-bind-of-a-saved-native-action).
It supplies syntax and context prior art. Haskell IO sequencing, laziness and
failure behavior need separate source tests. No Core change is proposed.

### M25: scoped native source inference

This target is verified in both compiler stages. M24 preserved two separate
failures: `ActionMissingSignature` rejected an unsigned local native action
with GHC-83605; `ActionNativeResultInference` rejected `pure xs` in an inferred
local computation with GHC-83865. Both source forms now compile without native
meta assignment or runtime-index escape. The source example passes 377
observations; see [M25 evidence](README.md#m25-scoped-native-source-inference).

- [x] Reuse `tcMonoBinds`' existing single nonrecursive FunBind path at
  `Tc.Gen.Bind:1330`. It calls `runInferRhoFRR` before it allocates the Id.
  Admit a local NoGen binding with one zero-argument equation and an exact
  native monotype. Use the existing inferred-signature validity context to
  validate each free runtime index against its actual outer binding. Keep
  ordinary head metas, FRR and multiplicity checks. No DepFunTy is inferred.
  Validate deep free runtime Ids too. The structural validity walk skips
  variable kinds and coercions; use its existing deep free-variable list and
  lexical index check. Exclude bound indices and erased CoVars.
- [x] Keep the separate InferGen guard at `Tc.Gen.Bind:731`. Do not remove
  inferred-type guards globally. Recursive groups, pattern bindings, partial
  signatures and inferred native function telescopes need separate designs.
  Preserve infer-cell promotion and branch-joining checks.
- [x] With an action's input type fixed, use the existing match checker to
  synthesize the continuation body through an infer cell. Start with one
  ordinary unrestricted lambda argument, a variable pattern and one
  unguarded equation. This includes the generated do continuation.
  Determine its native result choice before creating an ordinary result meta.
  A synthesized ordinary monad head can remain unknown; extract the payload
  and retain final subsumption to resolve that head. Whole-type immutable
  matching against a fixed IO head is not sufficient by itself.
- [x] Preserve unsigned effectful actions such as `effect >> pure vector`.
  Matching the first action fixes IO; the second can still infer a head meta.
  In the generic route only, unify ordinary head types that contain neither
  native syntax nor outer templates, then repeat exact whole-argument
  matching. Keep native choices immutable and keep the native-index route.
- [x] Reject a synthesized result that mentions the continuation's own input
  Id or an inner local Id. Returning `IO (Vec Int outerN)` is in scope;
  returning `IO (Vec Int n)` from an ordinary inner Nat binder would escape.
  Validate against the outer call-site environment and report the actual
  source occurrence. Do not infer a dependent arrow or package implicitly.
- [x] Turn both preserved failures into positive source/runtime cases. Keep
  sharing, action counts and bottom behavior. Add independent-index,
  callback-index escape and GADT escape rejections. Verify both stages and
  the registered regressions.

The source pointers above use `75c628aa8d`. See
[scoped source inference provenance](REFERENCES.md#scoped-native-source-inference).
Agda's pinned let checker and lexical context insertion supply prior art.
Its transparent lets do not justify equating runtime aliases or bypassing
Haskell scope checks. Agda's dependent lambda codomain is explicit; synthesis
of an ordinary GHC callback must instead reject a free callback-local index.

### M26: boxed tuple source inference

This target is verified. The M25 tuple baseline checked three versions of a
callback that returns `(42 :: Int, xs)` at an outer runtime index. Tuple syntax
rejected with GHC-83865. Constructor and helper forms compiled. M26 runs all
three forms at finite runtime inputs, with demand, effects and sharing checks.

The implementation expands only an `Infer` result under
`ExperimentalDependentTypes` when a field has a native anchor. It
preserves `checkTupSize`. The read-only classifier admits a complete
boxed tuple like a named application. Its ordinary fields can use the
existing fresh ordinary parameters. Known expected types keep
component checking. `mkExpandedExpr` and generated application spans
retain the original tuple context and field locations. Note
`[Inference of native tuple fields]` owns this rule in `GHC.Tc.Gen.Expr`.

- [x] Inspect `Tc.Gen.Expr`'s complete `ExplicitTuple` path at the M25
  checkpoint. It calls `expTypeToType` and allocates component metas before
  checking fields. The boxed `tcInferTupArgs` path also allocates metas.
  Reuse the existing ordinary tuple-constructor application machinery for an
  anchored boxed tuple that needs inference. Keep the original tuple syntax
  and source spans in diagnostics, using the existing expansion wrapper.
- [x] Extend the read-only native-argument classifier to recognize complete
  boxed tuple syntax with a native anchor. Do not execute splices or elaborate
  components during classification. Preserve checking when an expected tuple
  shape is available. Keep tuple sections, unboxed fields and representations
  on their existing paths until their inference rules are established.
- [x] Preserve exact field types, outer runtime Id scope, ordinary field
  multiplicities and once-only usage. A tuple does not bind an existential
  runtime index. Returning a callback-local index with its vector still needs
  an explicit package. Do not infer a Sigma type for an ordinary tuple.
- [x] Run the syntax, constructor and helper forms at finite IORef inputs.
  Check effect counts, bottom fields, partial/cyclic values and sharing. Reject
  independent indices and callback/inner-index escape with located errors.
  Verify both compiler stages and the existing registered regressions.

Use [Agda's constant product family](REFERENCES.md#boxed-tuple-inference-prior-art)
as the product precedent. Existing GHC constructor checking supplies the
implementation path. No Core or interface representation change was needed.

### M27: native tuple sections

This target is verified. The M26 baseline compiled an inferred callback
whose ordinary unit case returned `pure ((42 :: Int), xs)`. It also compiled
the fully applied constructor `((,) xs) (42 :: Int)`. Applying the section
`(xs,) (42 :: Int)` rejected with GHC-83865. The known expected native section
control also rejected. M27 admits both section forms and verifies their runtime
behavior. The original failures remain as compile/Lint evidence.

Keep `ExplicitTuple` and its existing desugaring. Present expressions remain
inside the missing-field lambdas, in source order. Do not add a sharing let.
Infer an anchored native present field once with its exact type and require
a lifted kind. Keep ordinary missing-field metas for inference. For a known
native expected visible-arrow function with a boxed tuple result, check
fields against the tuple components and use the expected hole multiplicities.
Store each hole's tuple component type, then check the reconstructed whole
section function against the original expected type. This retains required
domain/result coercions. Other expected shapes keep their existing path.
Generalize the existing tuple checker rather than add a second traversal.

- [x] Inspect `tcInferTupArgs` and the complete-tuple classifier guard.
  The boxed present-field path checks against a fresh whole-field meta.
  Establish section rules for both inference and known expected functions.
  Prefer reuse of the existing tuple AST and constructor checking. Partial
  constructor applications also need to satisfy existing saturation rules.
- [x] Keep missing-field types, multiplicities and runtime representations
  under ordinary GHC checks. Use exact native present-field types without
  assigning them to metas. Preserve expected higher-rank checking. Do not
  expand to a lambda without checking the existing section desugaring and
  its treatment of present fields.
- [x] Preserve outer index scope, once-only usage and lazy present fields.
  A section cannot hide an inner index or duplicate a linear capture.
  Keep ordinary case inference separate from branch-local index and GADT
  escape. Reuse the owning tuple Note for shared rules.
- [x] Compare sections with constructor controls at runtime IORef inputs.
  Check repeated application, field demand, partial/cyclic prefixes and
  original vector/payload sharing. Pin scope and usage diagnostics in both
  stages and rerun the existing registered regressions.

The source client passes 417 observations in each stage. The strict compile
matrix has ten positive and ten negative section cases. Existing checks bring
the rejection total to 88 per stage. The API fixture checks 122 package/bound
rejections. All five registered regressions pass. Native forall under a
retained binder remains gated; the higher-rank control is ordinary Haskell.
The lifted-kind negative reaches the field check through an ordinary unboxed
helper applied to a native argument. It does not stop at a signature gate.

### M28: native generic result overapplication

This target is verified. Both M27 stages compiled a named projection of a
stored section, then its application. Direct `(fst pair) (42 :: Int)` rejected
with located GHC-83865. The argument-instantiation route required all written
value arguments to exactly saturate the declared domains. Here `fst` declares
one domain; its chosen result must become a function before checking the extra
argument.

- [x] Admit a nonempty declared argument prefix under the existing native,
  NoQL, VTA, template and concreteness guards. Require a native anchor in that
  prefix. Preserve the exactly saturated route. Keep native-index-kind
  instantiation exactly saturated.
- [x] Infer and cache prefix arguments once. Start prefix matching with an
  empty substitution. Do not seed its intermediate result from the final
  expected result after extra arguments. Keep exact domain, kind and scope
  validation. Reuse ordinary completion of unchosen erased parameters.
- [x] Instantiate the original head and pass the full argument list to the
  existing application checker. Let that checker consume extra arguments and
  enforce evidence order, saturation, multiplicities and final subsumption.
- [x] Compare direct and named projections at runtime indices. Preserve wrong
  extra argument/result, conflicting prefix, scope, VTA and higher-rank gates.
  Test an anchor only after the prefix and once-only elaboration/usage.
  Record one owning Note and reuse the existing application-spine prior art.

Both source clients pass 439 observations. Nine new positive compile controls
and eleven located rejections pass in both stages. The retained cases bring
the rejection total to 99 per stage. A separate failed joint-prefix match has
one `prefixProbe` head elaboration in the TC trace. The API fixture passes
133 source rejections. All five registered regressions pass. The owning
`[Generic retained argument inference]` Note and its cache reference record
the saturation distinction. No Core or interface form changed.

### M29: native generic inference after leading VTA

Verified in both stages. The original M28 baseline rejected partial `@Bool`
with located GHC-83865. M29 accepts this generic route and preserves omitted
and complete ordinary VTA. The original native-index-kind prepass is unchanged.

- [x] Use the existing ITYARG step to check leading written type arguments
  once. Before IALL creates metas for the remaining binders, attempt native
  argument-directed inference from the substituted signature. Keep NoQL,
  extension, known-head/concreteness, prefix-anchor, kind and scope guards.
  Start before any value argument and require no remaining written VTA.
- [x] Preserve the recorded checked type arguments and original head. Feed
  immutable choices through IALL's existing plan and evidence order. Do not
  prepend remaining type applications ahead of the written `@Bool`.
- [x] Return inferred argument caches to `tcApp` and `finishAppWithCache`,
  including failed-match caches. Preserve the public `tcInstFun` interface
  where possible. Separate cache value positions from diagnostic source
  positions. `RetainedArgCache` keys count only value arguments. Generated
  type arguments advance neither cache keys nor source diagnostic positions.
- [x] Compare omitted, partial and complete VTA from source. Check wrong tags
  and kinds, extra arguments/results, conflicting prefix choices, contexts,
  scope and linear usage. Trace once-only elaboration on failure. Preserve
  generated/written argument positions and located diagnostics. Leave VTA
  interleaved after values outside this increment. Reuse the pinned spine
  precedent and give shared rules one owning Note.


### M30: native-index-kind inference after leading VTA

Verified in both stages. The M29 baseline compiled `observe vector` and
`observe @Int @Z VNil`, but rejected the following partial-VTA form with
GHC-83865. M30 accepts it without evaluating its inferred runtime index:

```haskell
observe :: forall a (index :: Nat). Vec a index -> Int
observe _ = 42
bad n = pure (observe @Int (localVector n))
```

- [x] Reuse the native-index-kind matcher after ITYARG substitutes leading
  arguments. Keep its exact-saturation, kind, native-meta and lexical-scope
  guards. Require NoQL, the extension, the original eligible head, no consumed
  value arguments and no remaining type arguments. Generic overapplication
  stays separate.
- [x] Return immutable choices for the remaining telescope through IALL.
  Preserve written type arguments and dictionary order. Retain successful
  and failed-match caches. Use the existing separate value/source counters
  and preserve the public cache-discarding wrapper.
- [x] Compare omitted, partial and complete ordinary VTA from source. Cover
  runtime Nat indices, nested contexts, multiple remaining parameters, scope,
  kinds, mismatched indices and linear usage. Check that ignored native index
  arguments stay lazy. Preserve original payload sharing and action counts.
- [x] Update the owning Note and pinned Agda/GHC provenance. Pin emitted
  diagnostics and primary source tokens in both stages. Rebuild both stages,
  run the focused registered regressions, and preserve the frozen M29 evidence.


### M31: joint native-index argument inference and result overapplication

M30 passed a named exact-saturated selection from `(Vec a index, value)`,
where `value` is a native function. Direct application rejected. A helper
with separate vector and payload arguments also rejected, including an
ordinary function payload. M31 accepts these forms:

```haskell
selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value
selectIndex _ value = value
good n = pure (selectIndex @Int (localVector n) ((+1) :: Int -> Int) 42)
```

Infer index-bearing arguments first. Their immutable match determines the
expected domains of ordinary arguments such as predicates. Then infer eligible
independent domains for parameters that remain unknown. Match the nonempty
declared prefix before checking extra arguments. The former strict baseline
had two accepted and four rejected forms; all six now compile in both stages.
Both stages pass 503 source observations, 139 located rejections, four ordinary
controls and both failed-match cache traces. The API fixture passes 173
rejections. All five registered regressions pass. The full GHC suite was not run.

- [x] Match native-index and ordinary payload choices jointly from the
  nonempty declared argument prefix. Infer independent parameters needed by
  that prefix, while preserving known expected domains. Require complete
  immutable native-index choices. Preserve kind, head/concreteness, exact
  cached-domain and lexical scope checks; keep native mutable fills forbidden.
- [x] Allow ordinary extra arguments after that prefix. Start overapplication
  matching with an empty substitution, since the final expected result comes
  after those arguments. Reuse IALL, final subsumption and dictionary order.
  Keep both leading-VTA and existing no-VTA routes consistent.
- [x] Compare named/direct calls and ordinary/native function payloads from
  source. Cover runtime indices, contexts, sharing, effects and demand. Pin
  conflicting choices, undetermined indices, extra-only anchors, scopes,
  kinds, dictionaries and linear use. Trace once-only elaboration on failure.
- [x] Update the owning Notes and pinned Agda/GHC provenance. Rebuild both
  stages, run the focused regressions, and preserve the frozen M30 evidence.

### M32: outer equality evidence under a retained Pi

The pre-M32 six-case baseline agrees in both stages. Ordinary function transport under
`a ~ b`, identical retained functions, pointwise retained transport, and global
family-axiom normalization compile with Core and STG Lint. Whole retained
function transport under `a ~ b` formerly rejected with GHC-83865 at the returned `f`:

```haskell
transportElement :: forall a b. a ~ b
                 => (foreach (n :: Nat) -> Vec a n)
                 -> (foreach (n :: Nat) -> Vec b n)
transportElement f = f
```

The pointwise definition `transportElement f n = f n` compiles. Removing the
equality proof rejects. This isolates an evidence boundary under the Pi.
It is separate from argument-prefix inference and source signature support.

- [x] Pin Agda's conversion/context rules and the GHC inert-rewrite paths.
  Audit the existing `DepFunCo` proof and its free runtime Ids.
- [x] Admit only outer ordinary equality evidence that cannot contain the new
  runtime binder. Preserve the fixed closed domain, role and location context.
  Track all active retained runtime Ids in the rewrite environment. Require a
  Given candidate's predicate, canonical RHS and evidence coercion to have no
  deep free occurrence of these Ids. Check variable kinds and reject coercion
  holes in this metadata. An outer runtime index can remain in the evidence.
  Preserve `eqCanRewriteFR` and ordinary dependency recording. Keep the ordinary
  empty-scope path free of these additional scans.
  Canonicalize a Given with a type-variable LHS and native RHS without mutable
  assignment. Accept valid opaque index Ids only in this explicit checking
  mode. Disable family cycle breaking there. Preserve all Wanted, unification
  and concreteness restrictions. Record this distinction in Note
  `[Rigid retained indices]`.
  Make the outer-index call expressible with a complete visible type argument,
  such as `@(Vec Int k)`. Reuse lexical runtime-name capture from local
  signatures, preserve erased-variable shadowing and VTA hole checking, and
  reject captured partial types and quotations. Report invalid captures as
  type-argument errors. Preserve the native-meta restriction.
  Keep plugins, family-cache access, fresh dependent Wanteds and mutable native
  fills gated. Update Note `[Rewriting retained dependent functions]`.
- [x] Compare direct and pointwise transport through a separate interface.
  Check proof scope, absent proofs, nested binders, demand and sharing. Pin
  located failures, rebuild both stages and run the registered regressions.

Optimizer support remains separate. Audit substitution, occurrence scope and
proof-case reconstruction before enabling transformations.

### M33: retained function types as equality operands

The pre-M33 six-case baseline agrees in both stages. Ordinary-arrow Givens
and identical retained functions compile. These three retained equality
operands reject with GHC-43582 at `foreach`: a Given whose RHS is a retained
Pi, a `:~:` witness with that RHS, and a Given whose retained RHS captures an
outer runtime index. A return without proof rejects with GHC-91028 at `f`.
Both stages pin the diagnostic code, fragment and source position.

```haskell
fromGivenPi :: forall value.
               value ~ (foreach (n :: Nat) -> Vec Int n)
            => value -> (foreach (n :: Nat) -> Vec Int n)
fromGivenPi f = f

liftExposedPi :: forall value.
                value ~ (foreach (n :: Nat) -> Vec Int n)
             => (foreach (m :: Nat) -> (value, Vec Bool m))
             -> (foreach (m :: Nat) ->
                   ((foreach (n :: Nat) -> Vec Int n), Vec Bool m))
liftExposedPi f = f
```

The first obstacle was source formation in both renaming and typechecking.
Commits `6963da76a7` and `826f47d880` reuse the existing retained telescope
checkers. Admit checked Pi expressions in ordinary type-argument positions.
Do not special-case equality constructor names. Preserve the caller's index
environment, fresh binder Names, complete types, staging, closed domains,
`Many` multiplicity, lifted bodies and source spans. Check the whole visible
type argument for named, anonymous and generated holes. Keep ordinary
wildcard inference unchanged.

This formation rule also admits type synonyms. Existing native scanners
inspect synonym RHSs. Ordinary native-meta filling remains blocked through
aliases. Literal `foreach` kind syntax remains gated. A complete closed Pi
alias can be used as a kind through an explicit specified kind argument.
GHC already treats kinds as types. Implicit kind-meta filling with that alias
still rejects. These controls do not establish general dependent inference
or introduce universe levels.

Direct Given and `Refl` transport already compile after source formation.
The direct Given can match as an irreducible constraint. It does not require
a solver change. In contrast, `liftExposedPi` rejects because the Pi Given
remains impredicative and cannot rewrite beneath `m`. Commit `050d928510`
checks its RHS recursively on the existing immutable Given path. It forms
the scoped reduction with `mkDepFunRedn`. It preserves occurs and coercion
checks, opaque runtime Ids and the M32 inert-rewrite filter. Native family
cycle breaking and mutable native assignments remain blocked.

The transitive-Given signature needs `AllowAmbiguousTypes`. Its ordinary
ambiguity check attempts a native meta fill. Explicit type arguments are
also required for native `Refl` operands and a bottom equality value made
with `error`. Keep these restrictions instead of widening inference.

- [x] Pin Agda's Pi sorts and equality transport. Preserve GHC's non-tau
  classification of retained Pi and erased forall types.
- [x] Implement mode-preserving renaming and checked type formation. Preserve
  index scope, kind checks, complete arguments and located diagnostics.
- [x] Trace direct matching and the later beneath-binder failure separately.
  Check existing native Pi Givens without assignment. A recursive Pi Given
  remains an occurs-check failure and cannot justify a rewrite.
- [x] Pass 562 source observations in both stages through separate interfaces with
  Core and STG Lint. Preserve M32's 532 outputs as an exact prefix. Check
  finite IORef inputs, proof demand, ignored indices, partial spines and
  function/vector sharing. Pass 42 strict source cases in each stage:
  21 positive and 21 negative.
- [x] Rebuild the self-hosted stage. Pass both strict source matrices, 156
  retained located rejections, API checks and all five registered tests.
  Preserve M32 evidence. The full GHC suite was not run.

See [the pinned prior art](REFERENCES.md#retained-function-types-as-equality-operands).
This task adds no runtime index evaluation, function eta equality,
structural retained unification or optimizer transformations.

### M34: qualified retained function types

The frozen M34 checkpoint passed declaration checking and transport. It left
construction and demand of a qualified-Pi equality witness open. M35 below
completes these checks.

The measured six-case baseline agrees in both stages. An ordinary qualified
arrow compiles. Direct and beneath-binder Givens, a `Refl` witness and a
recursive qualified-Pi assumption reject with GHC-91510 at the signature's
`forall`. The missing-proof body rejects with GHC-91028 at `f`. These results
locate the first boundary in source validity. They do not establish an
immutable Given-rewrite failure.

The follow-up seven-case probe enables `ImpredicativeTypes`. Both stages then
accept the ordinary qualified arrow and ordinary qualified Given. The five
native cases pass signature validity but reject with GHC-83605 at their
bodies: Quick Look does not support the retained type. Preserve this result
before changing Given checking.

First add a checked-variable route that needs no Quick Look instantiation.
Require an actual variable head, zero arguments, a checking context, no leading
invisible forall or context, and no free Quick Look instantiation variables
in either zonked type. Require native syntax in the actual or expected type.
Use `NoQL` throughout that application, with the existing final subsumption
and evidence checks. Applied and polymorphic heads retain their guards.
This intermediate step does not complete the caller or runtime work below.

The first checked-head build accepts direct Given transport. The
beneath-binder case reaches GHC-83865 with `cteImpredicative`. The `Refl`
case instead needs inference of its already-typed scrutinee. Extend the
monomorphic variable route to return that existing type through `InferResult`.
Read an available expected type without allocating a new cell. Preserve
promotion, level, representation and branch checks. This does not infer a
new native function type or instantiate a polymorphic head.

- [x] Pin the source-validity rule and GHC's ordinary dictionary elaboration.
  Compare the Agda hidden/instance distinction and the paper binder properties.
  Keep visibility, evidence kind and runtime retention separate. The D/DC/DR
  coercion quantifier is not a general Haskell class context.
- [x] Specify the checked qualified-Pi subset before changing admission.
  Preserve ordinary erased-forall restrictions, complete arguments, dictionary
  scope, roles, occurs checks and the native-meta guard. Trace formation first;
  change immutable Given recursion only if a later probe demonstrates a need.
- [x] Run direct and beneath-binder transport through separate interfaces.
  Both stages pass 26 source cases: 13 accepted and 13 rejected or currently
  unsupported. Check dictionary demand, bottom-function WHNF, ignored indices,
  payload sharing and function identity with 22 runtime observations. Preserve
  the 562 existing source observations. The API fixture and all five focused
  registered regressions pass. The full GHC suite was not run.
- [x] Complete the source witness callers and check proof demand. M35 passes
  the `Refl` and bottom-proof callers through complete expected arrows. Both
  stages pass proof order, function identity and the original tuple-under-Pi
  runtime shape. The combined qualified/container oracle has 48 observations.

See [the paper reading](papers/README.md) and
[the measured baseline](REFERENCES.md#retained-function-types-as-equality-operands).

### M35: exact matching through algebraic containers

Before M35, the imported `withQ` helper typechecked. Its callers could not
instantiate `QPi :~: QPi` through the complete expected type. The result
eligibility predicate in `GHC.Tc.Gen.App` did not traverse a non-tau algebraic
container. This blocked the existing exact immutable matching route before
proof demand could be tested. The function-identity client already worked;
it was not evidence for the container extension.

The fresh three-case baseline agrees in both stages. A known qualified copy
compiles. The `Refl` caller reports GHC-91028 at both `withQ` and `Refl`.
The bottom-proof caller reports GHC-91028 at both `withQ` and `error`.
Keep these commands, sources and locations under `native-pi-container-*`.
The M34 prefix is frozen.

The classifier build accepts both original witness callers in both stages.
The runtime and shared matrix producer then stop with GHC-83605 at a leading
ordinary newtype pattern. `tcRetainedMatches` selects constructor coverage by
syntax before checking whether the first expected argument is retained.
Restrict that selection to an actual first `DepFunTy`. Check a single ordinary
constructor prefix through the existing ordinary pattern route. Keep the
unconsumed Pi result, arity, usage scaling and newtype laziness. This does not
add support for multiple ordinary constructor rows.

The bootstrap pattern build compiles both producers. The runtime client and
the newtype matrix client then panic at the newtype worker's LFInfo. The
restricted simplifier leaves its compulsory unfolding disabled. A newtype
constructor has no runtime binding. Permit its existing cast template through
the compulsory-unfolding route. Keep the activation guard and the native
substitution and beta rules. Do not enable general inlining. Check named and
lambda patterns for lazy newtypes and strict data constructors at runtime.

The first API run reaches an old expected rejection for `undefined` at a
complete `Maybe (foreach ...)` result. This now compiles through the intended
immutable expected-result match. Keep the same source as a positive control.
Do not restore that old rejection by excluding containers. Recount the source
inventory after this reclassification and the new container cases.

The next API run reaches an old expected rejection for `transportRefl Refl f`.
Its first argument is an ordinary equality witness. The corrected ordinary
pattern route now checks this GADT pattern and scopes its Given over the Pi
result. Keep the same declaration as a positive control. Keep the missing,
unrelated, branch-local and representational evidence rejections. Add runtime
checks for proof demand and function identity on this pattern route.

- [x] Preserve the measured `Refl` and bottom-proof failures. Pin the Agda
  argument-checking comparison and the existing GHC matching rules.
- [x] Extend only `isRetainedValueType` eligibility for exactly saturated
  algebraic containers of lifted result kind. Keep the ordinary `isTauTy`
  fast path. Recurse through each argument with the unchanged incoming
  `under_pi` flag. A Pi in one argument must not admit a qualified sibling.
  Exclude new class, family, unsaturated and nonlifted cases. This does not
  change `isTauTy`, Quick Look, source formation, candidate matching, coercion
  roles, scope checks or the mutable native-meta guard.
- [x] Cover both existing callers: expected result matching and worker
  specialization. Use an imported tuple consumer and complete nested-container
  expected arrows. Keep erased forall, wrong-index, predicate, scope and
  nominal/representational rejection controls. Check newtype containers for
  wrapped bottom, ignored boxes and function identity.
- [x] Run `Refl`, ignored bottom proof and demanded bottom proof through
  separate interfaces. Add the original tuple-under-Pi runtime case. Update
  the existing API fixture and run both compiler stages and the focused
  regressions before closing M34.

Both stages pass the 26-case container matrix: 17 accept and nine reject at
11 located diagnostic envelopes. Both pass the 48-observation public example,
the previous 562 observations and all 26 earlier qualified-Pi source cases.
The existing API fixture passes with 229 package and 309 total rejection
programs and 53 quiet positives. Its 37-line golden is unchanged. All five
focused registered tests pass across 39 cases with 34 skips. The full GHC
suite and the full earlier standalone rejection corpus were not run. The quiet
total includes the existing standalone splice control. Earlier reported quiet
subtotals omitted that control.

### M36: native worker arguments after full application

Before M36, `withQ (error ...) f` rejects at `f` with GHC-91028. After both arguments
are consumed, the result `QPi` does not determine the erased `value` parameter.
The partial form has expected type `QPi -> QPi` and determines that parameter.
Keep this difference as the measured baseline.

The fresh seven-case baseline agrees in both compiler stages. The two partial
witness helpers compile. Full `Refl`, full bottom proof, full Given transport
and both overapplied runtime-index forms reject with GHC-91028. Preserve these
sources and diagnostic positions under `native-worker-arguments-*`. The M35
prefix is frozen.

Use one native-worker mode in the existing cache-producing argument route.
Detect native syntax in the original body, including its leading constraints.
Classify the complete worker type with the existing retained-value predicate.
Infer only original native-free tau domains. Use their known native anchor or
an available expected-result choice. Match kinds before types with the same
restricted immutable substitution. Require all invisible choices and reject
template escape. Skip callback inference and fresh ordinary parameters in
this mode. Keep every inferred cache entry if matching fails. Use the existing
ordered plan for index parameters, nested contexts and leading VTA.

- [x] Pin the scoped argument-checking comparison in Agda and the existing
  GHC argument-cache rules. Do not transfer Agda's general Pi-meta policy.
- [x] Add a cache-producing `NoQL` route for a native worker. Select erased
  parameters only from independently inferable named arguments whose formal
  domains contain no native syntax. Check the native-template proof argument
  after substitution. Infer each selected argument once.
- [x] Preserve complete choices, kinds, runtime scope, exact cached domains,
  original instantiation, final subsumption and failure-cache ownership. Keep
  native mutable-meta assignment and general native Quick Look disabled.
  Do not infer inside `tcRetainedWorkerResultMatch`: it cannot return caches
  and also runs after visible type application.
- [x] Check full and partial caller behavior, proof demand and order, forged
  witnesses, conflicting arguments, scope escape, visible type application
  and once-only cache use. Run both compiler stages and focused regressions.

Both compiler stages build and pass the 52-client source matrix: 27 compile
and 25 reject or remain unsupported at 35 located diagnostic envelopes.
A separate closed-proof control reports GHC-83605. Named proofs come through
an ordinary `ProofBox` pattern. Consecutive forall/context groups before the
first visible arrow pass, including joint Nat and Type choices. A forall
after a visible arrow remains outside the collected telescope. The failure
trace records one elaboration of the cached `prefixProbe` argument.

Both stages pass 57 runtime observations and the prior 562 observations.
The previous container matrix has one changed admission: the full bottom-proof
call now compiles. Its other 25 rows keep their codes and locations. All 26
earlier qualified-Pi cases are unchanged. The existing API fixture passes with
232 package and 312 total rejection programs and 61 quiet positives. Its
37-line golden is unchanged. All five focused registered tests pass across
39 cases with 34 skips and no framework warnings. The full GHC suite and the
full earlier standalone rejection corpus were not run.

### M37: applications with ImpredicativeTypes

The M36 baseline reports GHC-83605 for all four new application controls
with `ImpredicativeTypes`.
This includes explicit `withQ @QPi`, an incoming named proof, and an already
monomorphic `withKnownQ` head. Their source formation succeeds, but the applied
Quick Look route is blocked. Preserve these measured sources before changing
that route. A qualified Pi type argument without `ImpredicativeTypes` reports
the separate source-formation error GHC-91510.

The fresh M37 baseline agrees in both compiler stages: 61 clients produce
16 accepts and 45 source errors at 50 located diagnostic envelopes. Both
support providers compile. The original four application controls and closed
`Refl` control still report GHC-83605. Preserve these results under
`native-ground-applications-*`. The M36 prefix is frozen.

Separate checked type arguments from value checking. Reuse the existing
IALL/ITYARG loop for the leading type, pragma and wrapper prefix. Preserve
its original Quick Look allocations, type arguments and evidence. Check
each written type argument once. Stop prefix preparation before final
IRESULT actions, including the unsaturated-newtype check. Perform preparation
inside `tcRetainedAppEvidence`.

Select `NoQL` when the supported native signature has no remaining erased
choices and no free QL variables. A complete immutable expected-result match
can supply another such choice, including the inner `Refl` or `error` call.
Check deep metadata in the signature, expected type and prepared prefix.
Keep ground constraints as obligations for IALL and `instCall`. Types may
mention valid rigid outer variables and runtime indices. Preserve ordinary
payload inference and the native mutable-meta guard.

Use the existing immutable algebraic candidate fallback for a supported native
proof type. Repeated `Refl` endpoints can differ before nominal entailment uses
a local Given. Preserve the actual substituted result. A representational
Given or a false equality must still fail the final check.

Resume with the original head and checked prefix accumulator. This keeps
constructor and concreteness information, evidence order and written-VTA
diagnostic positions. Failed mode selection continues with the same prefix
and its original QL variables. Do not infer arguments before mode selection:
ordinary Quick Look could otherwise elaborate a failed cache again. Keep
the public `tcInstFun` caller on its existing guarded path. Preserve final
subsumption, proof entailment, multiplicity and runtime demand.

- [x] Pin the Quick Look and Agda precedents for applications whose native
  types are already fixed. Identify when ordinary instantiation can still
  create a QL variable.
- [x] Choose a narrow route that keeps immutable choices, original evidence,
  final checking and failure-cache ownership. Keep general native Quick Look
  and mutable native-meta assignment blocked.
- [x] Run the four preserved controls with both compiler stages. Check proof
  demand, function identity and the existing polymorphic-head guards.

Both stages now agree on 64 source clients: 42 compile and 22 report source
errors at 28 located diagnostic envelopes. All 31 predefined oracles pass.
Of the original 61 clients, 39 compile. Three added prefix-metadata controls
also compile. The cache-failure trace still elaborates `prefixProbe` once.
The unresolved phantom-kind trace retains `DoQL`; the filled-kind trace
selects `NoQL` after ordinary kind checking fills the same variable.

Both stages pass 76 raw and public runtime observations. The old 57-line
prefix and the separate 562-line packed-vector result are unchanged.
The API checks 233 package and 312 total rejection programs, plus 76 quiet
positives. Its 37-line golden is unchanged. Five registered tests pass.
The full GHC suite and older standalone corpora were not rerun.

The selected compiler is `89c98cf7b4a23b048f8ffd77170537f27a434224`, with
37 archived inputs. Evidence uses `native-ground-applications-*`.
The M36 prefix and its earlier preservation chain remain frozen.

### M38: native choices from known arguments with ImpredicativeTypes

M37 fixes explicit type arguments and complete expected results. Next,
measure calls such as `withQ Refl f` under `ImpredicativeTypes`, where a known
native argument can determine the erased parameter. Preserve the current
failure before choosing an implementation. A failed preflight must not cause
the value argument or a saved cache to be elaborated twice.

The initial 39-client baseline agrees in both stages: 12 compile and 27
report located errors. Three isolated targets, including `withQ Refl f` and
the complete witness annotation, report GHC-83605. Evidence uses the new
`native-known-arguments-*` prefix. The M37 prefix is frozen.

Read declared types of bare value variables without calling `tcInferId`,
elaborating an expression or collecting usage. Permit parentheses. Require
a monomorphic type without a leading context. Zonk existing assignments and
exclude free QL variables in deep type metadata. Global lookup can load an
interface; it must not instantiate the value.

Match eligible declared arguments against the ordinary value prefix with the
existing restricted kind-first retained matcher. Match only outer erased
binders. Require complete choices, substituted kinds, no template escape,
valid runtime scope and a native anchor. Keep the no-concreteness guard.
Use an expected seed only at declared ordinary saturation. An overapplied
call cannot use its final expected type as the declared intermediate result.
A conflicting exact match retains the guarded fallback.

Carry the choices through the existing `RetainedInstPlan`. Preserve the
original residual signature and prepared type-argument prefix. IALL consumes
each original forall/context group and emits evidence in that order. Skip
eager argument inference when this plan is present. Check every actual
argument normally at its selected domain. Retain result subsumption, linear
usage and nominal proof entailment. No Core change or native meta assignment
is proposed.

- [x] Preserve source baselines for bare local and global native arguments,
  signed witnesses and applied heads. Identify actual ambient QL variables.
- [x] Check whether declared argument types can supply a complete immutable
  choice before any value elaboration. Follow pinned application rules and
  retain ordinary kind, scope, concreteness and evidence checks.
- [x] Preserve all M37 demand, identity, diagnostic and failure-cache checks.
  Keep general native Quick Look and mutable native-meta assignment blocked.

Both stages now agree on 41 new source clients: 27 compile and 14 report
located errors at 15 diagnostic envelopes. All 16 predefined outcomes pass.
Compared with the preserved baseline, 15 additional clients compile. Linear
single use compiles; double use reports GHC-18872. Scope, class, role and
constructor errors reach their ordinary final checks. The failure control still
elaborates `prefixProbe` once. On the new declared-choice path, this is ordinary
checked elaboration after selection, not a failed cached-inference fallback.

The prior 64-client matrix passes both stages with 43 compilations and
21 located errors. `GroundAmbientApply` is newly accepted because a declared
argument and the saturated expected result provide complete choices. All 31
predefined controls pass. The other five exploratory signature/list/applied-head
forms still report GHC-83605. Existing API checks admit one former boundary,
`observe @Int xs`, and preserve its exact source as a quiet positive.

Both stages pass 94 raw and public runtime observations. The old 76-line prefix
and separate 562-line packed-vector output are unchanged. The API fixture passes
243 package and 322 total rejection programs, plus 95 quiet positives. Its
37-line golden is unchanged. All five registered tests pass: 39 cases, 34 skips,
and no unexpected or framework failures or warnings.

The selected compiler is `0e6d2f5ebcdcb231f7f5c42a27da563d049ac8af`, with
37 archived inputs. Bootstrap and self-hosted builds pass on their first launch.
All 12 selected validation phases pass on their first attempt: 11 launches and
one reused bootstrap matrix. Evidence uses `native-known-arguments-*`; the full
M37 and earlier preservation chain remains frozen. The full GHC suite and older
standalone corpora were not rerun.

## Further work

Follow the [whole-program roadmap](ROADMAP.md). Its next task is C1: map the
native Core representation and coercions to the published calculi, state the
remaining obligations, and check the next transformations. The map also tracks
retained quantifiers, first-class sums, evaluation, promotion, and language
cleanup. Source-inference improvements support that program but do not replace
its Core and type-computation work. SMT reasoning and proof-certificate checking
remain separate tasks.

### M39: checked beta exposure in the native Core index bridge

M38 is frozen and published at `a80eac9875be9f9757554390b5f3108c9d5c3b87`.
Use a new `native-index-beta-*` evidence prefix. Never write an M38 artifact.
This increment advances ROADMAP C1/C2 with one checked Core reduction. It does
not implement a general evaluator or source lambda-index elaboration.

Preserve the existing variable/constructor conversion. Add a shared checked
conversion that returns both a normalized runtime expression and its index.
For an immediate `App (Lam b body) arg`, require an eligible local Many binder,
one unchanged supported closed domain, and matching argument/body domains.
Require the argument to normalize to a local variable or nullary constructor.
The body and result must stay in the supported finite index grammar. Do not
unfold an Id, consume a cast as equality, evaluate a producer, introduce a
residual expression type, or add eta conversion. Caller Core Lint must still
check the original expression and all actual scopes and metadata.

Normalize before CorePrep names the argument. A result variable must stay that
actual variable; do not force it with an alias case. A result constructor must
use the existing checked constructor naming and transport. Keep type recovery,
substitution, `mapNativeIndex`, and naming on the same checked bridge. Preserve
ordinary code paths and reject unsupported expressions through existing gates.

Initial examples, with scoped Nat variables, are identity applied to `n`,
successor applied to `n`, and constant zero applied to a bottom-valued variable.
The unused bottom must stay lazy. A demanded bottom must retain its marker.
The initial atomic-argument restriction avoids duplicating a constructed
argument when a lambda uses it more than once.

- [x] Preserve the M38 Core/source baselines and add quiet checks to the existing
  Core API fixtures. Add no new tracked tests or golden output.
- [x] Implement the shared checked bridge and CorePrep normalization. Add one
  owning GHC Note with paper and implementation obligations.
- [x] Check type recovery, capture avoidance, nominal roles, wrong domains,
  invalid multiplicity/scope, opaque producers, runtime demand, and sharing.
- [x] Build both stages and run the focused existing regressions serially under
  the resource caps. Preserve the 94 and 562 source output prefixes.
- [x] Record the Core-only boundary and plan source admission after the kernel
  and naming checks pass. Keep general promotion and open evaluation unchecked.

Completed on 2026-11-15. Compiler source `707e681f284061f18b3e2bca590b27d153f2146a`
changes only `GHC.Core` and `GHC.CoreToStg.Prep`. The selected fixture commit is
`b0469bca8a4b160b8e5103ebe47ee7ac63628527`. The original 37 archived inputs match.
An independent audit found that this inherited list omitted both changed Core
files. A post-build supplement adds their exact committed/current bytes. The
complete archive now covers 39 selected inputs and 53 source paths. Preserve
the original archive and this timing qualification. The pre-build Core record
and later caller hashes agree; no source mismatch or compiler change was found.
The existing fixtures add seven Nat and two Bool normalization cases, a separate
shared-Id assertion, 12 converter denials, and four original-expression Lint
denials. Binary and imported-unfolding beta evidence passes at all three
compression modes. Direct CorePrep identity, successor, constant, and `keep`
checks preserve exact types and tested demand. `keep` returns 42 with a bottom
index before any SimpleOpt pass.

Both stages build. All 13 selected phases pass: 11 launches and two reused
bootstrap API checks. Both stages preserve all 41 known-argument and 64 prior
source outcomes, including 47 predefined controls. Raw and public
programs preserve all 94 observations; the packed-vector program preserves all
562. Five registered tests pass across 39 cases with 34 skips. The full GHC
suite and older standalone corpora were not rerun.

Two initial unit attempts exposed old identity-lambda rejection expectations.
Those expressions are now valid by design. Keep them as positive raw/interface
checks. Replacement opaque-producer denials retain unfolding error thunks.
Preserve both failed attempts and the passing third attempt. Source syntax is
unchanged. The next increment must account for Strict pattern demand and the
actual lambda binder's final multiplicity before admitting source applications.


### M40: bounded source lambda-index applications

The M39 Core bridge and backend are the implementation baseline. Add no Core
form or evaluator. Use a new native-source-beta-* evidence prefix. Preserve
all frozen M39 and earlier files, including the qualified post-build source-
coverage correction.

#### Source rule

Pass the known retained binder domain to tcRetainedArg. Resolve only an
immediate single-variable, unguarded LamSingle application with no local
bindings. Parentheses and existing expansion wrappers are transparent. Reject
signatures, other patterns, residual lambdas, lets, cases and opaque function
heads. Reject this new form under Strict with a specific existing-family
diagnostic at the lambda span.

Use an explicit renamed-Name-to-provisional-Id environment. A provisional
binder has the actual renamed Name/Unique, Many multiplicity and the known
closed native domain. Resolve an application's argument in the outer
environment and its lambda body in the extended environment. Constructor
fields use their actual worker argument domains. Do not install these Ids in
the real Tc environment or record usage during resolution. The shared Core
bridge validates the finite expression, domains and atomic beta arguments.

Keep the original renamed argument as EValArg. Ordinary tcValArg checks it
exactly once and records usage. Do not insert public annotations, infer then
recheck, mutate a checked binder, unfold a producer, or assign native syntax
to a meta. The bounded grammar permits no explicit One constraint; final
zonking should yield Many. Verify actual zonked GhcTc lambda Ids before
desugaring, including LinearTypes, instead of inferring this from a later
simplified Core dump.

#### Checks and example

Five independent source workers currently fail with GHC-83605 at M39:
identity, constant, successor, nested beta and lexical shadowing. Preserve
this measured baseline. Admit them and check the six actual lambda binder
Names/Unique values, Nat domains and Many multiplicities. Add Bool and mixed-
constructor-domain controls to exercise the resolver's domain propagation.

Reject Strict, bang/signature/non-variable patterns, non-atomic beta
arguments, opaque producers, wrong domains and escaping source names with
located diagnostics. Retain the existing source errors and normal Haskell
controls. Check usage and one-elaboration behavior where source hooks can
expose it.

Add one small public source client, separate from the known-worker inference
client. Exercise IORef-fed Nat values 0/2/3, exact vector indices, constant-
zero bottom avoidance, demanded bottoms, partial successor fields, nested
beta, lexical shadowing and retained-function WHNF. Keep the old 94 public
output lines as a prefix and all 562 packed-vector lines unchanged. Build both
stages and run focused existing checks under the established serial caps. Add
no new tracked tests or golden lines.

#### Provenance and limits

Reuse M39's pinned Agda application/substitution implementation and its GHC
Note. Extend Note [Retained argument application] to own the source resolver's
scope, multiplicity and demand rules. Keep GHC-83605; add
RetainedIndexLambdaStrict and update the generic expression grammar text. This
increment admits a bounded runtime argument expression. General term syntax in
types, promotion, definition unfolding, Strict lowering and ordinary optimizer
support remain open.

- [x] Preserve the measured source rejection baseline and complete M39 input coverage.
- [x] Add the bounded resolver and owning Note without a second argument check.
- [x] Check actual zonked lambda metadata and located rejection diagnostics.
- [x] Run the public source example with runtime demand and prefix checks.
- [x] Build both stages, run focused regressions, freeze evidence, and record the boundary.

Both stages pass the 41-client matrix: 16 compile and 25 reject at the expected
locations. Each stage also checks six actual zonked lambda Ids before
desugaring. Their renamed identities, Nat domains and Many multiplicities
match. The inherited 41- and 64-client matrices retain their M39 outcomes.
All 47 predefined controls pass. The raw and public examples pass 106
observations per stage, including the unchanged 94-line prefix. All 562
packed-vector observations remain unchanged. Five registered regressions pass
across 39 cases with 34 skips. The API source-program inventory remains
243 package/322 total rejection programs and 95 quiet positives.

The 17 final phases comprise 12 launches and five reused successes. Reuse
checks each phase's own input hashes. The selected compiler is
`5853b765f3319ed994bb088f4ce257a61bbeafc5`; the fixture source is
`366f94c32f5bea38c1a45916e0667aecaa52ac97`. The complete compiler archive has
39 inputs and covers all three changed compiler paths. The corrected bootstrap
build used the v2 snapshot. The v3 snapshot adds the diagnostic fixture change
before self-hosting; its 39 compiler inputs are identical. Source coverage is
54 paths. The M39 correction and older evidence remain frozen.

Bootstrap: 1m11s, peak 1818 MiB. Self-host: 17m18s, peak 5221 MiB. Both builds
use a 16 GiB cap. Final validation peaks at 1317 MiB under 4 GiB. Swap is zero.
Failed setup, AST, source-oracle and stale diagnostic attempts remain saved.
The unsigned captured-alias rejection is a measured pre-existing multiplicity
boundary; a signed alias works. The existing CoreRun diagnostic fragment now
matches the expanded grammar. Its 37-line golden remains unchanged.
The full GHC suite and older standalone matrices were not rerun.

### M41: checked native variable aliases

Remove one redundant local alias in the restricted native simplifier:
`let alias = actual in body` becomes `body[actual/alias]`. Keep an outer
`let actual = opaqueProducer x` lazy and shared. This is one C2 transformation.
It does not admit general inlining or an ordinary optimizer mode.

Match only a nonrecursive, non-top-level binding whose input RHS is literally
`Var actual`. Require both input Ids to be eligible local Many indices. Resolve
the target with the current `lookupIdSubst` only. Require a variable result of
the same substituted closed domain. A constructor substitution falls back to
the existing path. Do not unfold, beta-normalize or simplify an RHS to discover
an alias. Exclude joins, exported aliases, stable unfoldings, rules, inactive
inline activation and loop-breaker aliases. Do not inspect the target's unfolding.

Use `extendIdSubstWithClone` to store the same actual Id and maintain the
in-scope set. This helper allocates no Id. Remove a stale alias entry from the
inlining environment. Existing traversal substitutes terms, binder types,
Type nodes, coercions and native indices together. Surviving binders retain
the existing capture-avoidance rules. Keep recursive and top-level groups.
The rule runs only under `soe_native`.

Add quiet checks to the existing API fixtures. Lint input and output. Check
exact result types, deep free variables, repeated actual-Id identity, a
type-only alias dependency and a descendant binder that reuses a Unique.
Keep let-bound indices inside their scopes; the whole test result must not
expose a local alias. Test valid fallback cases separately from malformed Core.
Inspect direct simplifier output before execution. Check function WHNF,
ignored bottom returning 42, demanded bottom and a finite cyclic prefix.
Run both-stage public and packed-vector examples and the focused regressions.
Keep all existing goldens. Add no tracked test. Preserve M40 evidence unchanged
and use `native-variable-alias-*` for new evidence. Source alias defaulting,
function indices and the broader C1/C2 obligations remain open.

- [x] Preserve a measured pre-change alias-retention baseline.
- [x] Add the native-only substitution and owning Note.
- [x] Check scope, metadata, exact types and fallback cases directly.
- [x] Check runtime demand and both-stage source execution.
- [x] Build both stages, prepare the focused evidence freeze and record the boundary.

Compiler `25f7652dce2488c73115963a4396cf74a066c31c` implements this rule.
The fixture source is `ddd23db09cd2270c95d36879694ac0e1378868a1`.
The pre-build archive contains all 39 selected compiler inputs. Its source
manifest covers 54 paths. The exact compiler delta is
`compiler/GHC/Core/SimpleOpt.hs`.
The old-M40 baseline compiles, then fails exactly at the first alias-removal
assertion. Its earlier GHC-87543 import ambiguity was a setup failure.
Both attempts remain saved.

The unit fixture checks 17 valid cases: six transformation, identity and
capture cases, plus 11 fallbacks. Four malformed inputs fail input Lint.
Five direct runtime kernels check finite replication, bottom demand,
a finite cyclic prefix, ignored bottom and bottom-function WHNF. These
checks are separate from the unchanged source-program corpus: 243 package
and 322 total rejection programs, with 95 quiet positives. The existing
goldens remain one and 37 lines. No tracked test was added.

All nine final phases pass: seven launches and two bootstrap API reuses.
Reuse verifies each phase's own fixture and 39 compiler input hashes.
Both stages pass 106 raw/public observations with the old 94-line prefix,
plus all 562 packed-vector observations. Five registered regressions pass
across 39 cases with 34 skips. There are no unexpected failures or framework
warnings. The 41-client source matrix, inherited 41- and 64-client matrices,
and Tc metadata probe were not rerun. The full GHC suite and older standalone
corpora were not rerun.

Bootstrap: 25.76s, peak 1826 MiB. Self-host: 10m33s, peak 4302 MiB.
Both builds used 16 GiB. API checks peaked at 1219 MiB and final validation
at 1203 MiB under 4 GiB. Swap is zero. The prepared freeze preserves the
canonical M40 envelope, all 47 manifests and the older chain. It includes
a complete external manifest for all 34 future-function research files,
without changing existing cache manifests or claiming implementation.

### M42: scoped opaque function-call indices

Add `TermAppTy f arg` and nominal forward congruence `TermAppCo f co`.
The head is an actual scoped local Many Id. Its type is an ordinary visible
Many arrow from closed scalar domain A to closed scalar domain B. A and B
can differ. Exclude joins, polymorphism and function-valued scalar indices.
The type argument can be any scoped, well-kinded Type of A. In particular,
Given transport can introduce a family application or another call index.
The runtime conversion below has a smaller grammar.

The result kind is B. Congruence requires nominal argument evidence of kind A
and proves `f n ~# f m`. It does not provide inverse decomposition, evaluation,
function equality or head transport. Different stuck calls remain MaybeApart.
Compare equal heads and arguments under the actual renaming environment.
Do not inspect an unfolding. Agda 2.8.0 provides the operational reference:
`Var Int Elims` retains a neutral head and its applications; head substitution
reapplies the elimination list. The initial GHC head-substitution contract is
smaller: rename to a same-type eligible local Var, once. Keep an unsupported
function RHS in its lazy binding. Include function ranges in composition,
range free variables, freshening and all deep type/coercion traversals.

First execute a strict `case f arg`. Add a separate case-index bridge whose
argument uses the existing scalar variable, constructor and immediate-beta
grammar. Keep `exprToNativeIndex` unchanged. The case obtains either constructor
evidence `f arg ~# C fields` or DEFAULT evidence `case_binder ~# f arg`.
Consume that proof inside the branch and return an ordinary Int. Check both
Nat-to-Nat and mixed-domain heads. Preserve constructor-argument naming with
nominal congruence; do not insert a strict alias for an arbitrary opaque call.
Retain lazy function-head aliases through CorePrep and top-level Tidy.

Add explicit scope, kind, role, escape, scanner and interface boundaries.
An ordinary function lambda must not hide an escaping head in its result.
Source retained-call arguments and new interface wire forms remain gated in
this first slice. General lazy naming, higher-order retained binders, ordinary
optimizer admission and the rest of C1/C2 remain open. This milestone is a
partial implementation of the
[pinned comparison](REFERENCES.md#opaque-runtime-function-indices-unimplemented-c1-comparison).

- [x] Add the call node, forward congruence and complete scoped traversals.
- [x] Preserve head identity under cloning, substitution and composition.
- [x] Keep noninjective calls conservative in equality and family apartness.
- [x] Lower actual strict calls, including constructor-argument naming.
- [x] Check proof use, mixed domains, bottom order and type-only dependencies.
- [x] Build both stages and run existing fixtures without new golden lines.
- [x] Record complete input/output evidence and the verified boundary.

Use fresh `native-neutral-calls-*` evidence. Preserve all M41 and older
archives. No new tracked test is planned. Extend the existing quiet API and
runtime checks only. Compiler `0447f8a7a199675423cd7f5fafa94567afa40114`
implements this contract. The fixture source is
`428aa3795341e8628e258976adc6df434c17bda4`.

Snapshot v7 covers all 77 compiler inputs and 92 source paths. All 50 compiler
delta paths are included. Seven preserved snapshots use their own recorded
Git identities. The independent input audit checks 2069 facts with no mismatch.
Setup failures remain preserved. The corrected unit wrappers use valid
Coercion expressions. A separate measured baseline fails exactly when the
checker accepts a constraint-result arrow. The corrected checker requires
ordinary `FTF_T_T` value arrows and rejects raw open head metadata.

Nineteen quiet runtime invocations consume actual constructor or DEFAULT proofs.
Five check SimpleOpt-to-CorePrep directly. Unit controls cover substitution
composition, range free variables, capture, noninjectivity, raw formation and
three-role normalization. The source-program corpus remains 243 package and
322 total rejection programs, with 95 quiet positives. The new Core checks are
separate. Existing goldens remain one and 37 lines. No tracked test was added.

All nine focused final phases pass. Two reuse bootstrap APIs with their own
unchanged fixture and 77 compiler input hashes. Seven launch new jobs.
Both stages preserve 106 raw/public and 562 packed-vector observations.
Five registered regressions pass across 39 cases with 34 skips. There are no
unexpected failures or framework warnings. Source 41-client, known 41-client,
prior 64-client and Tc metadata matrices were not rerun. The full suite was
not rerun. Top-level Tidy protection has no separate isolated execution probe.

Bootstrap: 6m56s, peak 2899 MiB. Self-host: 20m58s, peak 5957 MiB.
Both builds use 16 GiB caps. API checks peak at 1223 MiB and final validation
at 1214 MiB under 4 GiB. Swap is zero. The prepared freeze binds all selected
inputs and outputs. It preserves the canonical M41 envelope, its 21 manifest
hashes and the earlier chain. Freeze and independent audit must pass before
publication. C1, C2 and the rest of the program remain open.

<a id="m43-strict-opaque-call-refinement-from-source-unimplemented"></a>

### M43: strict opaque-call refinement from source

Expose M42 through a checked source case. Keep `f` an ordinary Many function
with M42's closed scalar domains. Use an argument or a local binding with a
complete signature.
Keep the retained index nonvacuous in the signature. The first target is:

```haskell
probe :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
probe f n original =
  case f n of
    Z -> let ys :: Vec Int (f n)
             ys = VNil
         in size ys
    S k -> let ys :: Vec Int (f n)
               ys = VCons 42 (replicateNative k 42)
           in size ys
```

`size` is an ordinary vector length observer. Refine the whole `f n` call.
Do not refine `n` or assume that `f` is injective. M43 implements this subset.
The checked source runtime call requires a bare eligible variable argument, modulo
parentheses. Source type formation has a larger symbolic argument domain.

- [x] Register eligible lexical function heads in complete type contexts.
  Preserve actual Names, shadowing, staging, wildcard and signature gates.
- [x] Recognize a saturated source type application before generic type
  application. Build `TermAppTy f arg` at B after checking `arg` at A.
  Reject bare function indices; do not construct matchable `AppTy`.
- [x] Extend `Validity` and `tcRetainedInstScope` for checked local head
  dependencies. Read assigned metadata without changing Unique or Many.
  Keep native meta-assignment, inference and Quick Look safeguards.
- [x] Require the existing full constructor-refining subset. Exclude guards,
  wildcard, variable and lazy-only rows before introducing a strict alias.
  Leave ordinary irrefutable cases lazy. Detect branch-local call annotations
  as a new dependency trigger.
- [x] Classify the already-checked scrutinee after `tcInferRhoFRR`.
  Preserve its actual head/argument Ids, usage and wrappers. Do not check,
  expand or desugar its source twice. Reject unsupported forms at their spans.
- [x] Abstract exact whole-call occurrences in expected types, captured
  values, incoming predicates and branch-local annotations. Reuse nominal
  FC lifting and explicit transport. Preserve quotation and scope checks.
- [x] Emit one strict outer DEFAULT alias `q :: r ~# f n`. Reuse existing
  per-row retained constructor cases on `r`. Compose `sym q` with each
  `r ~# C fields` proof when the original call equality is needed.
  Keep the call once, the actual field Ids and ordinary row failure order.
- [x] Preserve this shape through desugaring and PMC. The ordinary HsCase
  lazy scrutinee binding cannot silently replace the checked alias case.
  Do not apply a retained motive function directly to the opaque call.
- [x] Check finite IORef inputs, constant-function noninjectivity, bottom
  head/argument order, lazy predecessor fields, exact-call captures and
  independent calls. Add mixed-domain, escape and located negative controls.
  Check producer/consumer execution with existing signature codecs; keep
  new call-bearing interface forms gated. Run mandatory Core/STG Lint.

No function-valued retained Pi binder or lazy `keep (f n)` admission is planned.
General Given rewriting, call evaluation and function extensionality stay open.
The full C1 and C2 obligations remain open.

Provenance: Agda v2.8.0 [with-abstraction 710–741, 910–936](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L710-L741)
(blob `ff0888af84f3101351c501534de087c180dc7afd`),
[LHS 1503–1532](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1532)
(blob `3779ffa6e418bca99e30c071e189106f82ecbe1b`), and
[Application 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
(blob `82ba94ac864d19b947d4c92b9af252b0fe81347a`). These support context
abstraction and checked substitution, not Haskell demand or an FC theorem.

Dependencies: head formation and scope checks precede whole-call abstraction.
The checker precedes the typed carrier and desugaring. Backend Lint precedes
source execution and interface checks.

The checked carrier is one typed-only `XXExprGhcTc` constructor. It stores
the checked scrutinee, fresh result Id r, nominal proof q, result type and
checked body. Desugaring emits `Case scrut r result [DEFAULT q -> body]`.
Only the body sees r and q. The result type stays outside their scope.
Zonking, expression type recovery, printing, HIE and PMC handle the form.
An enclosing source expansion preserves the original source for diagnostics.
Existing wrappers do not bind this strict DEFAULT proof.

The static carrier and demand reviews are in
`native-neutral-calls-source-carrier-review-v1.md` and
`native-neutral-calls-source-demand-addendum-v1.md`.
Selected compiler/source: `edbe394d03628de2900fff828b4137d72e5cc94c`.
Snapshot v5 records 82 compiler inputs and 98 source paths. All five historical
snapshots preserve their own Git identities. The M42 envelope and its 45
manifest hashes remain frozen.

Twenty-six quiet source controls pass at bootstrap and in the self-hosted
registered fixture. Six check `Strict`. Both stages pass the public nine-line
program and fourteen negative clients. A separate fifteen-line program keeps
the public nine-line prefix and adds six signed-local-head and `seq` controls.
The inherited 19 Core invocations and source corpus keep separate counts.
The one-line and 37-line goldens remain unchanged. No tracked test file or
golden line is added.

All nine final phases pass, with two checked API reuses and seven launches.
Both stages preserve 106 raw/public and 562 packed-vector observations.
The registered group reports 5 passes, 39 cases, 34 skips and no unexpected
results or framework warnings. The source 41-client, known 41-client, prior
64-client matrices, Tc metadata probe and full GHC suite were not rerun.

The `Strict` predecessor check exposed `seq` binder reuse that changed an
index identity in type arguments. A focused desugaring guard preserves the
outer scalar or function-head identity. Ordinary demand and row order remain.
Earlier import, scalar-dispatch, `seq`, finder and supplemental source errors
retain their actual failure records. The last was a wrong RuntimeRep type
argument in the probe, not a compiler feature gate.

The final bootstrap build takes 21.67s and peaks at 1825 MiB. Self-hosting takes
20m11s and peaks at 5791 MiB. Both builds use 16 GiB caps. API checks peak at
1243 MiB and final checks at 1185 MiB under 4 GiB caps. Swap is zero.
Evidence uses `native-source-neutral-calls-*`. Freeze and independent audit
must pass before publication.


<a id="m44-checked-constructor-arguments-in-source-calls-unimplemented"></a>

### M44: checked constructor arguments in source calls

M44 exposes the existing Core argument bridge through source `case f Z` and
`case f (S n)`. Keep M43's whole-call abstraction and all of its scope and
demand checks. No new Core type or coercion constructor is added.

- [x] Classify the already-checked scalar argument from actual `HsVar`,
  `ConLikeTc` and `HsApp` nodes. Validate a temporary constructor spine with
  `exprToNativeIndex`. Keep actual Ids, DataCons, types and multiplicities.
  Reject unsupported wrappers and producers. Do not call the renamed-syntax
  `tcRetainedArg` or emit usage a second time.
- [x] Generalize `tcRetainedCallCase` from one variable argument to the complete
  checked index. Seed substitution scope with all free dependencies.
- [x] Match branch annotations against that index structurally. Compare actual
  constructor and variable Names, arity and recursive fields. Parentheses may
  differ. Keep `f n`, `f (S n)`, other heads and shadowed leaves distinct.
  Do not infer an input from a call result.
- [x] Reuse `NativeCaseAlias`, nominal whole-call lifting, the retained motive,
  quotation and instrumentation gates, and `Strict` capture protection.
- [x] Reuse CorePrep's recursive argument naming and forward congruence.
  Preserve constructor WHNF without forcing predecessor fields.
- [x] Check constant and nested constructor arguments, mixed domains, captured
  consumers and function results. Include bottom-head order, ignored partial
  fields, cyclic prefixes and explicit predecessor forcing with Core/STG Lint.
- [x] Measure independent-call, wrong-domain, arity, shadowing, wrapper and
  scope diagnostics. Preserve M43 and older focused regressions at both stages.

Dependencies: checked argument classification precedes exact annotation
matching. Those checks precede source execution and both-stage regression
validation. The owning source Note remains [Source opaque call refinement].
Agda's pinned Application 774–808 and with-abstraction ranges above supply
checking and context-abstraction prior art. They do not prove Haskell demand.

Typed beta arguments remain separate work under M45 below. Their actual multiplicities
can still contain metas before zonking. Do not replace them with provisional
Many Ids. Nested opaque calls, function-valued retained binders, lazy retained
calls, general optimization and new interface codecs stay outside M44.
The preparation is `native-source-neutral-calls-next-boundary-v1.md`.

Selected compiler: `f76bba39b8bc61ea3e7ee48338086b1949fdce60`.
Selected source: `30fb3cca1eb2027b4ff490892ce69ec17ac57190`.
Snapshot v2 records 82 compiler inputs and 99 source paths. Only the Expr and
Match source checkers change. Both builds and the bootstrap API pair pass.
The one-line and 37-line goldens remain unchanged. Seventeen new quiet source
observations pass, separately from the inherited source 26 and Core 19 controls.

Both stages pass 17 CLI observations and reject 17 clients: eleven GHC-83865
and six GHC-83605 clients. There are 37 located envelopes, with 13 and 24
respective codes. The four added controls have a separate semantic review.
Four legacy checks pass public nine-line and supplement 15-line programs in
both stages. The nine final phases pass with two API reuses and seven launches.
Both stages retain 106 raw/public and 562 packed observations. Registered
results are 5 passes, 39 cases, 34 skips and no unexpected results or warnings.
No source 41-client, known 41-client or prior 64-client matrix, Tc metadata
probe or full GHC suite was rerun.

Bootstrap takes 33.59s and peaks at 1827 MiB. Self-hosting takes 7m25s and peaks
at 4335 MiB. Both use 16 GiB caps with zero swap. API2, legacy and final checks
peak at 1239, 221 and 1201 MiB under 4 GiB caps. Final swap is zero.
The old baseline, API1's unsupported imported `bottomNat` argument and CLI1's
13-client attempt remain recorded. Evidence uses
`native-source-call-constructors-*`. Freeze and independent audit must pass before publication.
C1 and C2 remain open.

<a id="m45-signed-beta-arguments-in-source-calls-unimplemented"></a>

### M45: signed beta arguments in source calls

M45 implements `case f (((\x -> S x) :: Nat -> Nat) n) of ...` through the
existing scalar Core beta bridge. It adds no Core node, equality rule or codec.

- [x] Require a surviving checked ExprWithTySig with one plain A -> A arrow.
  Both domains have the same bare, optionally parenthesized TyCon Name.
  Exclude aliases, applied domains, wildcards, forall and contexts.
- [x] Require one actual LamSingle row, one bare VarPat, one unguarded RHS and
  empty local binds. Preserve the actual Name/Unique through a scoped NameEnv.
- [x] Read existing type/multiplicity assignments with the during-TC zonker.
  Require Many in the actual Id and scaled MatchGroup argument, with matching
  domains/result. Leave Flexi untouched. Do not default or manufacture Many.
- [x] Reconstruct temporary Core classification data, then use the existing
  atomic beta converter. Keep the original scrutinee and one usage check.
- [x] Reuse canonical whole-call annotation matching and all M44 scope,
  quotation, instrumentation, constructor and nominal transport checks.
- [x] Return unsupported under Strict. Preserve ordinary nondependent fallback
  and its source demand. Do not introduce an early global Strict rejection.
- [x] Check signed identity/constant/successor/nested bodies, partial/cyclic
  fields, bottom-head demand and canonical annotation scope in both stages.
  Check the measured signature/Strict/producer and active-proof errors below.
  Keep actual graded guards distinct from an isolated unresolved-meta test.

Selected compiler `44e91a7618fe55c3713c85641b4a2d88d4536f13` changes only
Tc.Gen.Expr. Selected source is `0c1bb75a6ecaef5ecc62cf84450968ad21de67ca`;
snapshot1 records 82 compiler inputs and 100 source paths. Both APIs pass on
the first API attempt, including 15 quiet source observations. Goldens remain
1/37, separate from inherited source17/source26/Core19. The older source
corpus retains 243 package rejections, 322 total rejections and 95 quiet positives;
these are not counts of new raw beta checks.

Both stages pass public15 and reject eight CLI clients. Five have both codes,
one has only the earlier GHC-83605 local-signature scope error, and two have
only GHC-83865 under active refinement. Per stage, 15 located envelopes contain
nine GHC-83865 and six GHC-83605 diagnostics. Four supplementary runtime
observations per stage check Strict2 and same-spelling scope2. Trace markers
count 1/1 per stage; this is limited marker evidence, not an elaboration theorem.
Legacy6 preserves 9/15/17 observations at both stages. Final9 passes with two
API reuses and seven launches: raw/public106 and packed562 at both stages,
plus five registered tests. Registered totals are 39 cases, 34 skips and no
unexpected results or framework warnings.

Bootstrap2 passes in 9.67s at 1594 MiB; self-hosting in 7m24s at 4229 MiB,
under 16 GiB caps. API1/legacy/final peak at 1247/258/1203 MiB under 4 GiB
caps; final swap is zero. Bootstrap1 selected the wrong build target; preserve
that setup attempt. It is not a compiler failure. The M44 source baseline
rejects with 20 ordinary errors, ten GHC-83865 and ten GHC-83605.

Evidence uses `native-source-call-beta-*`; parent M44 remains preserved.
Freeze and independent audit must pass before publication. The full GHC suite,
source41/known41/prior64 matrices and Tc metadata probe were not rerun.
The exact signature/Many restriction is implemented. No separate experiment
with unresolved multiplicities or general unannotated inference is claimed.
See Note [Signed source call beta] and
[the primary readings](REFERENCES.md#signed-source-call-beta-prior-art).

<a id="m46-head-only-function-valued-retained-binders-unimplemented"></a>
<a id="m46-head-only-function-valued-retained-binders"></a>

### M46: head-only function-valued retained binders (compiler validation complete)

The implementation starts with complete local signatures and one equation
with bare variables in retained columns:

```haskell
keepCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat)
         -> Vec Int (f n) -> Int
keepCall f n _ = 42

idCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat)
       -> Vec Int (f n) -> Vec Int (f n)
idCall f n xs = xs
```

- [x] Specify a distinct retained-binder policy: a scalar Id or an existing
  eligible local Many function head with closed ordinary Many A -> B and
  independently closed scalar A/B. Keep TermVarTy and scalar predicates unchanged.
- [x] Admit the actual runtime function Name in a complete local telescope.
  Keep its type/coercion occurrences in TermAppTy/TermAppCo head slots.
  Preserve outer aliases,
  shadowing, ranks, staging and source errors. Start with bare variable patterns.
- [x] Select variable-only actual function operands at their exact domain.
  Check usage once. Reuse head-only substitution and binder masking; reject
  unsupported lambda/global/applied ranges, or keep a signed lazy local alias.
  The actual variable needs the exact checked type, not its own written signature.
- [x] Update blocked Core lambda/type/application binder checks. Verify the
  existing deep FVs, binder masking and capture avoidance. Preserve same-head
  identity. Do not represent a bare function as TermVarTy or matchable AppTy.
- [x] Extend fixed-domain DepFunCo, Lint, lifting, normalization and permitted
  Given checks. Permit only unchanged/reflexive head transport. Body congruence
  can use TermAppCo under that same head. Add no function equality, inverse
  call projection, native meta fill or heterogeneous domain rule.
- [x] Let CorePrep substitute an eligible local Var head directly. Do not use
  scalar markers or strict DEFAULT naming for a function argument. Preserve
  existing lazy head aliases, metadata protection and top-level Tidy behavior.
- [x] Run keep/id payloads through source, Core/STG Lint and execution. Check
  ignored bottom function/index/payload, actual call demand, distinct heads,
  alpha/capture cases and proof-only FVs. Pin invalid domains, scope and producer
  errors. Keep pure Pi matching opt-in and its escape checks intact.
- [x] Keep exported call-bearing signatures and all new wire forms gated.
  Do not extend the scalar interface reader as an accidental codec bypass.

Dependencies: binder policy and exact variable application precede source
checking. Coercion/type recovery and Prep scope precede execution. For an initial
case consumer, define a complete local caller :: Vec Int (f n) -> Vec Int (f n)
and caller = idCall f n outside the case. Existing M43 transport can capture
that instantiated caller. Direct probes2/3 reject payloads in both branches in
both stages: GHC-83865 at lines 33/37 compares Vec Int Z or Vec Int (S k)
with Vec Int (f n). These probes have no runtime result. The worker schema contains
different bound heads/indices before application. Keep this as the next-step
baseline; do not widen M43 Givens to promise successful direct composition.
Implementation and focused compiler/runtime validation are complete. Canonical
audit and publication have separate records. No new evaluator, general
optimizer or combined preservation theorem is claimed.
C1 and C2 remain open. Historical preparation and exact source hashes are in
`native-source-call-beta-next-function-pi-v1/`. See
[the primary comparison](REFERENCES.md#head-only-function-valued-retained-binders-prior-art).

Selected snapshot6 uses compiler/source
8fd22b97c74571c7d5508607f4f16e6611a84435. It contains 83 compiler inputs and
102 source paths. Snapshot5 passed bootstrap/API2, public14 plus ten negative
clients and constructor11 in both stages, and legacy8. The reviewed negative
split was nine GHC-83605 and one GHC-83865. API2 took 5.72s/17.14s with
unchanged 1/37 goldens. CoreRun adds ten quiet source positives and four errors.
The old M45 baseline2 rejects the same public bytes with seven GHC-83605.

Snapshot5 final1 passed its first eight phases, including raw/public106 and
packed562 in both stages. Registered tests passed four and mismatched one
existing scalar stderr expectation. Snapshot6 restores that scalar wording;
new function wording applies only when isPiTy holds. No golden changed. Two
quiet scalar assertion fragments were restored. Preserve all earlier import,
role, assertion and diagnostic failures with their own inputs.

- [x] Complete selected bootstrap6, API3 and self-host2. Bootstrap takes 2m48s;
  API3 Unit/CoreRun take 5.698s/17.910s. Self-hosting takes 11m03s with peak
  4966MiB under 16GiB and swap0.
- [x] Record CLI bootstrap4/self-host2. Each passes public14 and rejects ten
  clients with ten error envelopes: nine GHC-83605 and one GHC-83865.
- [x] Run constructor supplements3: all eleven pass in each stage.
- [x] Review direct probes3: two GHC-83865 errors per stage at lines 33/37;
  review-v3 confirms four errors total, no panic and no runtime result.
- [x] Complete legacy2: all eight phases pass.
- [x] Complete final2: nine phases pass, with two API3 reuses and seven launches.
  Both stages pass raw/public106 and packed562. Registered tests have five
  expected passes, 39 cases and 34 skips, with no unexpected/framework failures
  or warnings. The registered run takes 23.631763s. Final2 peaks at 1213MiB
  under 4GiB with swap0.

Source41, known41, prior64, Tc metadata and the full GHC suite were not rerun.
Freeze and independent audit follow the documentation commit. Record their
outcomes in `dependent-haskell-M46-final-audit-v1.json`; record publication in
`dependent-haskell-M46-publication-v1.json`. Do not infer their counters or
hashes from the passing compiler checks.

Keep the ordinary Phantom regression. A nonreflexive ordinary multiplicity
family exposes the same FunCo child-role defect as the dependent control.
Note [Function reduction multiplicity roles] owns the targeted correction.
Normalize multiplicity at Nominal; weaken only the evidence stored in a
Phantom FunCo. Do not weaken Core Lint or drop the failing role case.

### M47: remembered equality for direct call application

M47 compiler/runtime validation is complete. The selected Given-only
implementation uses the existing retained matcher and ordinary Wanted/Given
solving. It changes Match and the
public/quiet fixtures. It does not add application or environment machinery.

- [x] Create `q_motive :: i ~# f n` after whole-call generalization. Keep its
  RHS intact. Put it immediately after `i`, before old evidence and captures.
  Apply the motive to the strict alias `r`, then `q :: r ~# f n`, then those
  old arguments. Keep the motive Id and alias Id distinct.
- [x] Reuse existing constructor transport to derive `q_branch :: K ~# f n`
  under `cv :: i ~# K`. The existing `checkConstraints` covers the whole row,
  including `where` bindings. Ordinary Wanted solving records the dependency
  and keeps the required evidence binding.
- [x] Remove the experimental custom App cast and lexical refinement context.
  Restore all seven affected App/environment/quotation/splice paths to M46.
  Add no new NoQL/isTau transport gate or solver rule.
- [x] Preserve ordinary CoVar argument arity. `q_motive` uses the ordinary
  evidence-token ABI. Only `cv` uses the virtual constructor-proof slot.
- [x] Pass Given-only bootstrap3 in 2m01s. Selected self-host2 passes in 10m20s,
  peak 5196MiB under 16GiB, swap0.
- [x] Pass selected API4: eleven new values, three expected runtime markers
  and three located GHC-05617 source errors. Older quiet groups pass too.
  Unit/CoreRun take 5.364124714s/16.739944110s; goldens stay exactly 1/37.
- [x] Verify both 238-entry API4 input sets remain unchanged after the build.
  Reuse API4; no API5 rerun is needed.
- [x] Pass selected both-stage baseline3, public17 and annotated-where3 CLI
  checks with Core/STG Lint. Preserve the old fourteen-line public prefix.
- [x] Pass raw scope run2 with 36 assertions. It tests pure occurrence/type
  utilities, not a source-admission gate, TcM, native meta assignment or target
  runtime behavior.
- [x] Pass all eight legacy phases and eleven positive constructor controls
  in each stage.
- [x] Pass all nine final phases: two API4 reuses and seven launches. Inherited
  raw/public106 and packed562 pass in both stages. The registered run has
  39 cases, 34 skips and five expected passes, with zero unexpected/framework
  failures or warnings.
- [ ] Complete the separate artifact audit and publication. Infer no counters
  or hashes from the compiler/runtime passes.

Selected compiler is `3504f1be5342c9f3fbe12e2bf8dbe3bbc008e239`; source is
`f7f0e92e4a775d407189faa79ae0cc608eee7749`. Snapshot5 records 83 compiler
inputs and 102 source paths. The compiler delta is 22 Match lines. Preserve
unpublished custom-App attempts and API3's expected-code failure separately.
The latter needs only a fixture correction, not broader source acceptance.
These are focused checks; the full GHC suite was not run.

At M47, existing result, capture, Given or annotation information must activate
the retained case. M48 below adds a separate metadata-only trigger.
`where` support already follows from the complete row's Given scope.

This step adds no Core/interface form, inverse call decomposition, function
equality, native mutable-meta assignment, eta law, general solver rule or
evaluator. Existing checking, scope, usage and staging remain authoritative.
C1 and C2 stay open. See [the contract](CORE.md#remembered-equality-for-direct-call-application)
and [the primary comparison](REFERENCES.md#remembering-an-abstracted-call-prior-art).

### M48: known-worker case activation

M48 compiler/runtime validation is complete. Match adds one activation policy.
It reuses the existing retained case checker, motive equality and ordinary
Wanted/Given solver.

- [x] Keep the existing result, capture, Given and annotation triggers first.
  Consult metadata only when none applies.
- [x] Silently require complete, unique, unguarded constructor rows with bare
  field variables. Do not inspect quoted terms to find a new trigger.
- [x] Read a known checked lexical worker's leading retained telescope. Open
  only `DepFunTy` binders with eligible bare local actual Vars at exact domains.
  Use `applyTypeToArgs`, then require a whole-tau residual containing the exact
  free call. Do not guess erased choices or look up an unknown worker globally.
- [x] Keep discovery read-only with respect to inference, metas and usage.
  Do not elaborate source arguments or execute/expand splices. Decline unknown
  or unsupported metadata and preserve the ordinary case fallback.
- [x] Pass bootstrap1 in 25.56s. Fresh and freshwhere bootstrap attempts2 each
  compile/run 0/2/3 without payload annotations. Their baselines reject with
  GHC-83865/GHC-83605 and have no runtime result.
- [x] Preserve ordinary outputs 42/7/42 and duplicate-row output 42 plus warning
  GHC-53633.
- [x] Pass quote CLI1 with 42/7. Independent wrong-head and wrong-index CLI
  clients each reject with a located GHC-83865 and no runtime result.
- [x] Remove public payload annotations. Add ten quiet values: direct and where
  calls at inputs 0/2/3, an ignored undefined payload returning 42, guard outputs
  42/7 and the tested incomplete case returning 42. Keep the warning-producing
  duplicate control in its separate CLI only.
- [x] Pass selected API1 Unit/CoreRun in 5.616214630s/16.836089039s. Exact
  goldens remain 1/37. Add no new negative fixture assertions for the two
  independent CLI rejections.
- [x] Pass self-host1 in 6m14s, peak 5412MiB under 16GiB, swap0. Verify API1's
  own input maps are unchanged after the build; do not repeat the same checks.
- [x] Pass selected both-stage fresh3/freshwhere3 and ordinary/duplicate/quote
  controls. Each wrong-head/index client rejects with located GHC-83865 and
  no runtime. Public17 passes both stages with all expected outputs unchanged.
- [x] Pass legacy8 and all eleven positive constructor controls in each stage.
- [x] Pass all nine final phases: two own API1 reuses and seven launches.
  Inherited raw/public106 and packed562 pass in both stages. The registered
  run has 39 cases, 34 skips and five expected passes, with zero unexpected/
  framework failures or warnings.
- [x] Complete the separate M48 freeze, independent audit and publication at
  `3e6f9aa35fba64e9276433e490351d426bac0973`. The canonical audit SHA256 is
  `953f58351b6ed84eaaccf0a93e957fe7fc7e291ac7bfafef0161cb3516d16aa8`.
  It records 65 manifests and 35457 integrity checks.

Snapshot2 selects compiler `439fbf6396` and source `8480d83c98`, with 83
compiler inputs and 102 source paths. The compiler change is confined to Match.
These shape rules select a trigger; they impose no new global application restriction. Earlier
triggers preserve their existing diagnostics and behavior. Normal checking
still performs source checking and usage accounting once. Raw scope36 remains
M47 evidence and was not rerun; Core, Type and Utils do not change. These are
focused checks, not a full GHC suite run.

Complete signatures remain required. No App/Core form, evidence rule, solver
rule, backend transformation or evaluator is added. C1/C2 and the broader
roadmap remain open. See [the contract](CORE.md#known-worker-case-activation)
and [the prior art](REFERENCES.md#known-worker-case-activation-prior-art).


### M49: constructed-worker case activation

Extend only Match's known-worker metadata trigger. Keep actual application
checking and ordinary motive/row Given evidence unchanged.

- [x] Reconstruct resolved `HsVar`/`HsApp`/`HsPar` scalar constructor trees.
  Require real monomorphic constructors with no erased binders, equalities or
  contexts. Check the expected result domain and each field domain. Validate
  the resulting metadata with `exprToNativeIndex`.
- [x] Use the known leading `DepFunTy` telescope and `applyTypeToArgs`.
  Keep function actuals as eligible bare local Vars. Preserve the complete-row,
  whole-tau and quotation gates, and the existing activation triggers.
- [x] Add no source value recheck, usage accounting, meta assignment or splice
  execution to discovery. Keep App/Core/evidence/solver/backend/codecs unchanged.
- [x] Preserve the M48 baseline: the constructed client rejects normally with
  GHC-83605/GHC-83865 and has no runtime result. Its ordinary client runs
  42/7/42. Keep every attempt and source fingerprint.
- [x] Pass bootstrap1 in 25.35s and self-host1 in 5m48s. Self-host peak is
  5816MiB under 16GiB, swap0. Snapshot1 records compiler `a90757e6d9` and source
  `4ef6ba241e`, with 83 compiler inputs and 102 source paths.
- [x] Pass API1 Unit/CoreRun in 5.679607153s/17.552677284s. Preserve exact
  1/37 goldens. Add eight quiet values: 1/3/4/0 for one constructor layer and
  2/4/5/0 for two layers. Post-build input maps are unchanged, with 304 entries
  each and zero Stage1 paths.
- [x] Pass both-stage constructed4, nested4, demand2 and ordinary3 source
  programs. Their outputs are 1/3/4/0, 2/4/5/0, 0/0 and 42/7/42 respectively.
- [x] Reject the two independent wrong-key clients with located GHC-05617 in
  both stages. Keep them compile-only. Check distinct constructed keys and a
  bare key against the original `f (S n)` branch relation. Record the known
  generated `call_motive` name in relevant bindings as a rendering limitation.
- [x] Pass public21 in both stages. Preserve its exact old17 byte prefix and
  independently authored appendix 1/3/4/0. The constant-head case ignores
  bottom `n` beneath `S n`.
- [x] Pass selected legacy8 and constructor11 in both stages.
- [x] Pass final9 with two own API1 reuses and seven launches. Both-stage
  raw/public106 and packed562 pass. The registered run has 39 cases, 34 skips
  and five expected passes, with zero unexpected/framework failures or warnings.
- [x] Freeze, independently audit and publish M49 at
  `7029ed2a7d78e0b6ddc44ac3fa7c6b4e0b9cb7d5`. Canonical SHA256 is
  `ba310681ec8effaf4c6a84bc4e4dfe26ca902ee0545a89cd0aefe5f069cf25a7`,
  with 55 manifests and 35230 integrity checks.

Keep the eight new values separate from the inherited 243/322/95 source corpus
and the overlapping CLI programs. Inherit M48 duplicate/quotation controls and
transitive M47 utility36 evidence. Do not claim a new utility36, source41,
known41, prior64, TC-metadata or full GHC suite run. C1 and C2 remain open.
See [the contract](CORE.md#constructed-worker-case-activation) and
[prior art](REFERENCES.md#constructed-worker-case-activation-prior-art).

### M50: multi-constructor retained package patterns

The bounded source ADT slice is implemented. Each selected constructor owns
its field telescope; siblings do not add bindings to that telescope.

- [x] Remove only the parent-constructor count test in `tcRetainedDataConPat`.
  Preserve native constructor shape, result wrapper, domain, scope and Many
  checks. Keep lazy and let patterns unsupported.
- [x] Detect retained fields in any constructor group in `matchConFamily`.
  Compile each row with its own actual fields and a DEFAULT failure
  continuation. Preserve order within each group; outer constructor groups
  are disjoint. Leave the ordinary family path unchanged when no row needs
  retained-field or native-proof handling.
- [x] Update the structured package diagnostic to describe prefix syntax and
  unrestricted multiplicity. Add no Core, solver, wire or backend rule.
- [x] Preserve the M49 baseline producer rejection83605 without consumer/runtime.
  Record its passing Sigma3 as a pre-existing form, not a new M50 feature.
- [x] Pass bootstrap1 in35.81s and self-host1 in6m16s, peak5134MiB under16GiB,
  swap0. Snapshot3 selects compiler7e47932abc/source905554e642,83/104 paths.
  Its capture follows bootstrap1 and precedes self-hosting; retain snapshots1/2.
- [x] Preserve API1's missing DataCon-object linkage failure. Reuse the existing
  object-module runner for the new group; API2 passes in5.716686885s/17.922187794s
  with eight genuine observations and exact1/37 goldens. Actual296-entry maps
  per phase stay unchanged after self-hosting and contain zero Stage1 paths.
- [x] Pass both-stage public17, Sigma3, mixed17 and twin17 through actual
  interface/object imports. Cover both constructor orders, ordinary sibling
  fields/GADT evidence, different retained telescopes, guard/field failure,
  bottom/cyclic ignored fields, outer-bottom demand and selected payload demand.
- [x] Reject lazy/linear clients with located83605/18872 in both stages.
  Keep both compile-only; no main-is, link or runtime phase.
- [x] Pass all eight selected legacy programs.
- [x] Preserve constructor attempt1's stale-parent manifest lookup failure
  before targets. Correct only the runner provenance; attempt2 passes eleven
  observations in each stage.
- [x] Pass final9 with two own API2 reuses and seven launches. Both-stage
  raw/public106 and packed562 match. The registered run retains five tests,
  39 cases,34 skips and five expected passes, with zero unexpected results
  or framework failures or warnings. Keep existing goldens1/37 unchanged.
- [x] Freeze, independently audit and publish M50 at
  `d918069c802b84490eec9dc175b77bfc25545c85`. Canonical SHA256 is
  `85d0f9827b9fd444049a40ff11cc24bfe3b9a1eb62988c50df258fe9470fbb15`.
  The audit passes 35874 integrity checks over 69 manifests. Preserve every
  baseline, preparation, capture and failed API and freeze attempt.

Do not aggregate overlapping CLI programs with the eight new object-module
observations or the inherited243/322/95 source corpus. No new utility36,
source41, known41, prior64, TC-metadata, old177 or full-suite run is claimed.
C1, C2 and R22 remain open. See [the contract](CORE.md#multi-constructor-retained-package-patterns).

### Next source candidate: multi-constructor retained packages

The bounded candidate is now M50 above. First-class dependent sums and general
Sigma formation/elimination remain unimplemented. Additional multi-constructor
incomplete-coverage and index/proof-escape programs still need their own
measured controls; existing PMC remains enabled.

### Next requested source program: maintained Showcase

- [x] Add `docs/dependent-haskell/Showcase.hs` as a separate change after the M50
  source snapshot. Demonstrate completed source capabilities with one program.
- [x] Map each group to its source/library support in the README. Keep the build
  recipe in the source note. Pass both compilers with Core/STG Lint and the
  independently specified 48-line output. Preserve all eleven source inputs
  across the self-hosted run. Keep unsupported forms explicit.

Add a small labeled example in the same change that completes a source feature.
Keep the program finite. This showcase does not close R22, C1 or C2 and does
not replace focused regression evidence.


### M51: function-valued retained package fields

Status: completed, independently audited and published.
The target stores the actual function used by the later payload type:

```haskell
data FunctionPack a where
    FunctionPack :: foreach (f :: Nat -> Nat)
                  -> foreach (n :: Nat)
                  -> Vec a (f n) -> FunctionPack a
```

The erased parameter `a` remains ordinary. The stored `f` and `n` remain lazy
runtime fields. A later field refers to their actual constructor-local Ids.
Reuse `DepFunTy`, `TermAppTy` and the existing sequential field metadata.
Reuse the M46 closed, visible Many function policy. Do not make functions
scalar indices or infer input/function equality from equal call results.

- [x] Preserve the M50 baseline compiler and matching reader metadata. The
  generic producer is rejected with located GHC-83605 at its function binder.
  Baseline attempt1 failed dependency-interface lookup before this check;
  attempt2 copies the actual dependency interface and reaches the source gate.
  The saved old reader reads the actual old PackedVecLibrary interface.
- [x] Use `isNativeRetainedBinderType` in source field checking and deferred
  named-field import. Use `isNativeRetainedBinder` in DataCon validation and
  source pattern fields. Preserve all existing declaration, scope, Many,
  wrapper, ordinary-field and scalar constructor-pattern checks. A stored
  function pattern must remain a bare variable.
- [x] Encode `IfaceTermAppTy` with fresh type tag12, a local head name and an
  argument type. Decode through the lexical Id environment. Check the exact
  closed function domains and argument kind before constructing `TermAppTy`.
  Keep the whole-field check for references to earlier fields only.
- [x] Preserve diagnostic free call heads in a separate non-serializable
  `IfaceFreeTermAppTy`, following `IfaceFreeTermVar`. Cover printing and all
  type traversals. A free head must not become a serialized local spelling.
- [x] Set tracked `VERSION_DATE` to20261115.1 and reconfigure the fork as
  10.1.20261115.1, header1001202611151. Rebuild matching compiler/libraries.
  Check old-reader/new-file and new-reader/old-file rejection, including
  IgnoreHiWay. Keep unknown tags explicit failures. Keep generic exported
  function-valued Pi, coercion encoding and native unfoldings gated.
- [x] Extend the existing API fixtures without new test files or golden lines.
  Check binary round trips, lexical freshening, free/forward/sibling heads,
  wrong kinds/domains/multiplicities, noninjectivity and deferred malformed
  datatype knots. Check field order and runtime arity with Core/STG Lint.
- [x] Pass separate producer/consumer source imports in both compilers with
  the 24-line independently specified finite oracle. Cover IORef0/2/3,
  identity/successor/constant heads, a Char payload, repacking, row scope,
  package WHNF, ignored fields and distinct demanded errors. A producer
  failure must not count as a negative client rejection.
- [x] Run the existing relevant API, source, constructor, native pipeline and
  registered checks. Preserve historical failures and exact selected inputs.
  Add a labeled function-field example to Showcase.hs when this slice passes.
- [x] Update the contract, references and current status with actual results.
- [x] Freeze, independently audit and publish the completed checkpoint.

The preparation lives under `native-function-package-*`. M50 and its
`native-multicon-package-*` namespace remain frozen. Prior art is recorded
[with the references](REFERENCES.md#function-valued-retained-package-fields-prior-art).
This slice does not close R22, C1 or C2 and does not establish totality.


The selected compiler is `e238c74541bcb7c4061cd446f8bc91767f34aae8`.
The actual source/fixture snapshot is `37634e1568` (snapshot3). It covers83
compiler inputs and110 tracked paths. Snapshot1 precedes the first build.
Snapshot2 follows bootstrap and the API-only fixture change. Snapshot3 follows
both builds and the Showcase-only change. All selected target checks use
snapshot3. Final documentation has separate committed hashes; test execution
does not claim final prose bytes.

Measured results: source24 and mixed14 in both stages; Showcase51 with twelve
actual modules in both stages; four logical body errors and two policy guards
per stage; actual old/new reader checks; API goldens1/37 plus24 quiet source
observations; legacy8; constructor11 in each stage; final9 with two exact API
reuses and seven launches; raw106/public106/packed562 in each stage; registered
39 cases/34 skips/5 passes with zero unexpected failures. Counts overlap.
No full source-matrix, old raw36 or whole GHC-suite replay is claimed.

Preserve baseline1's dependency-lookup failure, the failed formatter attempts,
the sandbox bus launch failure and Showcase1's invalid library-query option.
The corrected showcase runner passes bootstrap2 and selfhost1. No compiler
source change follows the successful builds. Existing goldens stay unchanged.

The audited and published checkpoint is `b92d3f1595`. The canonical local
record is `dependent-haskell-M51-final-audit-v1.json`, SHA256
`4cc77414d28e947f26301bc59168d9306671f04144a09c9fadc1535d033a7fa2`.
It pins72 manifests. Auditor-v3 passes55914 checks and146121 overlapping
manifest-entry checks. The frozen cohort keeps110 source paths, nine
documents,45 harness files and1509 outputs. This status change follows the
verified push and changes documentation only.

The first freeze attempt failed before writing manifests because its quiet
oracle parser captured strings after the list. Freeze-v3 bounds and decodes
the exact24 strings. The first independent audit failed because baseline2
contains a third successful old-reader step. External auditor-v3 verifies that
step and passes the full audit. An external repair manifest pins the original
failed report, frozen auditor-v2, actual auditor-v3, exact diff and repaired
result. The frozen namespace stays unchanged. No failed attempt is relabeled
as a pass.

### Next candidate: exported function-valued retained signatures

The bounded target is an exported ordinary value signature:

```haskell
idCall :: forall a. foreach (f :: Nat -> Nat)
       -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)
idCall f n xs = xs
```

Use the existing M51 function package to supply actual local function/input
fields across an object boundary. Research is in
`native-function-signature-next-research-v1/report.md`. The source signature
gate and generic `IfaceDepFunTy` reader still reject this exported form.
Existing tags10/12 and the O0 omitted-unfolding ABI are candidates for reuse.
Do not broaden inference, pattern activation or coercion encoding without
separate evidence. Write the implementation specification before changing
these gates. Synchronize the older scalar-field sentence in Note [Source
retained signatures] in `GHC.Tc.Gen.HsType` when that section next changes.
M51's actual field predicates and its owning DataCon/interface Notes already
state the function-field policy. This stale sentence is a documentation
follow-up, not a failed compiler check.

### M52: exported function-valued retained value signatures

Status: complete. The source is validated, independently audited and published
at `00b128403ce1e31b15bd960b89690f1d6e99fc1f`. The full program goal stays active.
The exported value has a complete ordinary signature:

```haskell
idCall :: forall a. foreach (f :: Nat -> Nat)
       -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)
idCall f n xs = xs
```

A separate client opens an actual M51 `FunctionPack` and calls
`idCall @a f n xs`. Preserve the actual local function/input Ids in the result.
Use ordinary object imports with O0, omitted unfoldings, Core Lint and STG
Lint. Reuse `DepFunTy`, `TermAppTy`, tags10/12 and the three-argument runtime
ABI. The erased payload parameter stays erased. Add no singleton or Core node.

- [x] Preserve the actual M51 compiler/reader before rebuilding. Measure the
  current producer rejection after its real dependencies compile.
- [x] Replace the private signature Name permission with an ordinary-value
  signature flag. Grant it only in the complete named branch of
  `tcUserTypeSig`. `FunSigCtxt` is a diagnostic context, not a permission:
  instance method signatures and `SPECIALISE` also use it. Pass `False` from
  hs-boot checking and the generic `tcHsSigType` entry point. Keep the class
  path, general type-argument checks, staging, partial signatures, captured
  indices and result-kind checks unchanged. Use the existing closed visible
  Many `A -> B` function-domain policy. Update the diagnostic and owning Note.
- [x] Reconstruct generic `IfaceDepFunTy` through `isNativeRetainedBinder`.
  Allocate the actual local Id before decoding its body. Preserve the exact
  result-kind check. Keep bare `IfaceTermVar` scalar-only and reject diagnostic
  free heads. Keep Pi/call coercion encoding and native unfoldings gated.
- [x] Keep `VERSION_DATE` at20261115.1. The wire layout and tags do not change.
  State the admission boundary accurately: an old reader can display the raw
  signature but must reject its typed import at the old function-binder gate.
  Force the imported Id type in that control; an unused import is insufficient.
- [x] Update existing API fixtures without adding test files or golden lines.
  Replace the generic function-Pi rejection with both-compression round trips,
  two fresh imports, alpha equality and nested same-spelling scope controls.
  Retain malformed domain, multiplicity, missing/forward head, bare function
  index and coercion-codec negatives. Replace only obsolete source negatives.
- [x] Compile and link the actual producer/client with both compilers. Check
  finite IORef inputs, generic Char payloads, exact head/input scope, payload
  sharing, partial application and ignored/demanded bottoms. Record a separate
  literal oracle. A dependency/producer failure cannot count as a client error.
- [x] Measure omitted VTA and an explicitly typed local alias separately. Do
  not widen `App.isRetainedValueType`, imported-worker case activation or
  native metavariable assignment solely to bypass the direct target.
- [x] Add a small maintained Showcase example. Run the relevant API, source,
  native pipeline and registered regressions. Keep exact input provenance and
  historical failures. Record actual diagnostic codes and source spans.
- [x] Update current contracts, references and status.
- [x] Audit and publish the source milestone. Keep the full program goal active.
  The canonical record is `dependent-haskell-M52-final-audit-v1.json`, SHA256
  `6af17c2536a0aba1f41d3ec413a246c4120157ddef94b2d44d0fcda92b5614da`.

M52 evidence uses `native-exported-function-signature-*`. This namespace is
permanently frozen. M51's frozen namespace and external repair files remain
unchanged. The next-research cache
is prior read-only evidence. Record its original reading context separately.
The primary comparison is
[with the references](REFERENCES.md#exported-function-valued-retained-signatures-prior-art).
This increment does not close R22, C1 or C2 or establish totality.

Validation uses source `d8c605b6c63dd8f68920bcd83103fcd2ca389009` and compiler
`3d4994c68ba42b8fb1bd43d3f780780384edd0da`: 84 compiler inputs and 113 source
paths. Both stages pass the 19-result object program and the 53-line Showcase
from 13 actual source modules. Existing API goldens stay at 1/37 lines, with
19 additional quiet object observations. The final registered run has 39
cases, 34 skips, five expected passes and no unexpected results or framework
failures or warnings. Nine source context controls reject with GHC-83605; two
logical clients reject at the payload with GHC-83865 in each stage. Omitted
visible type application and a complete local alias compile in separate
probes. No optional-probe runtime result is claimed.

The first registered run exposed a partial-list warning. An explicit list
case removes that warning in the public and embedded clients. Both completed
compiler builds use the same 84 compiler bytes after this source-only change.
The final cohort reruns against snapshot2. Historical launcher, context and
reader setup failures remain recorded.

Six synthetic ordinary-domain interface knots raise `<<loop>>`. Three scalar
comparisons reach the unchanged scalar domain check. No old executable ran
these synthetic cases. This malformed-interface validation gap remains open;
these observations are not successful defensive rejections. Native coercion
encoding, native unfoldings, general optimization, C1, C2 and R22 stay open.

### M53: native dead local binding analysis

Status: complete. The source is validated, independently audited and published
at `a50950d55c50625cc4fb962bb1d35045944299f3`. The full program goal stays active.

Run a restricted native analysis before `simpleOptNativeExpr`. Use
`GHC.Core.FVs.freeVars` and its existing deep `FVAnn` invariant. Reconstruct
the annotated tree without binder substitution or new occurrence counts.
Remove a local `NonRec` only when its binder is absent from the original
body annotation and the existing binding policy permits removal. Require a
lifted type and multiplicity `Many`. Keep CoVars, TyVars, joins, exported Ids,
stable unfoldings, rules, inactive bindings and loop breakers. A scalar or
function head that occurs only in a type must remain in scope.

Keep all remaining binders, types, coercions, ticks, case alternatives and
recursive groups in their original positions. Visit nested expressions but
do not reorder or split recursive groups. Preserve top-level identities
through the existing per-RHS program entry. Keep native SimpleOpt inlining,
dropping and eta reduction disabled. Do not enable `-O1`, native unfoldings
or the general occurrence analyser. Original annotations can retain an outer
binding after an inner binding is removed. This is a conservative single
pass; it does not promise complete dead-code elimination.

The GHC implementation precedent is its deep free-variable annotation pass
and `oa_can_drop` policy. Agda v2.8.0 separates complete free-variable
queries from relevance filtering and later Treeless usage analysis. This
supports the separation of scope from runtime demand. It does not justify
Haskell eta reduction or a claim about totality. See the
[prior art](REFERENCES.md#native-dead-local-binding-analysis-prior-art).

- [x] Measure the current compiler's structural failure to remove an ordinary
  dead lifted let. Require valid input Core and the exact expected failure.
  The source fixture at `cf21b7ac8af3452dc0459335fe737b0aa80c4dce` compiles
  against M52. Both Lint checks pass before the expected structural failure.
  `native-occurrence-analysis-baseline-v1/results.json` preserves that result.
- [x] Add the native entry and its owning GHC-style Note. Use existing deep
  FV annotations and the existing removal policy. Add no new FV algorithm.
- [x] Update the existing API fixture without new test files or golden lines.
  Check dead-let removal and retained scalar/function-head dependencies in
  erased arguments, later binder types, case results and coercions. Check
  scope masking, recursive order, metadata and unlifted/token exclusions.
  Lint input and output and compare the exact expression type.
- [x] Build both compiler stages. Run the existing API, source, Showcase and
  selected registered regressions. Preserve type-only laziness and strict
  case demand. Record structure separately from runtime observations.
- [x] Update current contracts and status. Record actual source provenance,
  failed attempts and limits.
- [x] Freeze, audit and publish the increment. Keep the full program goal active.
  The canonical record is `dependent-haskell-M53-final-audit-v1.json`, SHA256
  `ece09f9d7fb15fb96766138e324ac22201076efdc8d57536c6dbea5f3d3ed8aa`.

Validation uses compiler `d740b247a8bae21bbf741e8b24fdbf1c54f871f4` and source
`2225c8f09c52b74556d34e87d19afa2ff4f5b176`: 85 compiler inputs and 114 source
paths. Both compiler builds pass. The API goldens stay at 1/37 lines. Actual
input hashes are unchanged after the selfhost build, so those API results
remain valid. Both stages pass the 19-result object program and the 53-line
Showcase from 13 modules. Registered selfhosted validation has 39 cases,
34 skips and five expected passes, with no unexpected results or framework
warnings or failures. The ten new API expressions include the Core-only
`One` control; no source linearity acceptance follows from that control.

The direct stable-metadata probe passes on attempt3. Attempt1 omitted two
imports. Attempt2 used `cheapEqExpr`, which does not compare lambda nodes.
Attempt3 checks the template constructors and actual binder/index identities.
All attempts remain. The original final cohort's metadata failure stays
recorded alongside its five successful independent source checks. No compiler
or tracked source change was needed for the probe repairs.

M53 evidence uses `native-occurrence-analysis-*`. This namespace is permanently
frozen. M52 and earlier namespaces remain frozen. C1, C2, R22 and the full
program goal stay open.

### M54: diagnostics for detected recursive interface-domain failures

Status: complete. The source is validated, independently audited and published
at `91d582bb945cd0f37c4dede7ee168e24c2ba4d81`. The full program goal stays active.

Force the existing native interface eligibility results inside an IO exception
boundary. Catch only `NonTermination`. Report the failure through `failIfM`.
Keep all existing guards and their order. Return no placeholder type and do
not retry an active field decoder. Apply the boundary to generic retained
binders, scalar indices, opaque call heads and retained field checks.

This is diagnostic containment. It is not a loader staging fix or a termination
guarantee. The runtime does not guarantee detection of every loop. A separate
shape-first design must expose raw declaration and synonym shapes before it
checks recursive fields. Keep valid first-order recursive domains and ordinary
fields with valid dependent function types. Do not reject recursion as syntax.

GHC's lazy interface Notes and Agda v2.8.0's declaration stages explain the
construction boundary. Their guarantees do not justify unchecked Type heads.
The research and primary-source provenance are in
`native-domain-loader-next-research-v1` and `native-domain-loader-review-v1`.

- [x] Record the current six failures without modifying frozen M52 evidence.
  The current M53 library produces six actual `NonTermination` exceptions.
  `native-recursive-domain-diagnostics-baseline-v1/results.json` records the
  typed classification, source/library hashes and unchanged inputs.
- [x] Add the narrow boundary and its owning GHC-style Note.
- [x] Update the existing API fixture. Require the normal interface error and
  diagnostic text. A timeout, raw `NonTermination` or unrelated exception fails
  the check. Include valid recursive domains, a synonym-hidden failure and an
  ordinary field with a valid retained function type.
- [x] Build both stages and run focused API, source, Showcase and registered
  checks. Keep the existing goldens and separate structural/runtime evidence.
- [x] Update references, contracts and publication status. Preserve failed
  attempts. Keep shape-first loading, C1, C2 and R22 open.
  The canonical record is `dependent-haskell-M54-final-audit-v1.json`, SHA256
  `6843fd43d979a85acecfa2b5f1cd74cca0eec0427c8fda94ce01833aaa341073`.

Validation uses compiler `4ab0e9f75a469fc4384d04f9cd18656216757a64` and source
`a75239d10b109f3608da6c2e133c01b7c3ffc0bb`: 86 selected compiler inputs and
115 source paths. The final bootstrap build passes in 16.88 seconds. The
self-hosted build passes in 6 minutes 54 seconds, with peak memory 4,492 MiB
under the 16 GiB cap and zero swap. API goldens stay at 1/37 lines. The 334
actual inputs per API phase are unchanged after self-hosting. Both stages
pass the 19-result object program and the 53-line Showcase from 13 modules.
Registered self-hosted validation has 39 cases, 34 skips and five expected
passes. It has no unexpected results or framework warnings or failures.

The fixture checks nine invalid declaration graphs in both compression modes.
It also checks nine valid graph and forcing-order cases in both modes. The
six earlier rejection graphs remain. The initial bootstrap warning and
successful API attempt remain recorded. Final validation includes the later
field-synonym case and its explicit domain-classification demand. No old
executable, broad historical matrix or full GHC suite was run.

The independent static artifact audit passes 2,733 checks over 735 pinned paths.
All `native-recursive-domain-diagnostics-*` files and directories are permanently
frozen. The full program goal stays active.

### M55: inspect native interface domains before decoding fields

Status: complete. The source is validated, independently audited and published
at `b5695d28a0b882eaa2da378ff91535702ad0344f`. The full goal stays active.
M54 remains the detected-loop fallback for unclassified forms.

Derive a small domain-shape view from each raw data declaration and nullary
synonym. Attach the view to the reconstructed `TyCon` before fields are forced.
Resolve references through the existing lazy interface environment. Rebuild
the nullary type cache when attaching the view. Add no interface tag, trusted
wire flag, global name cache or unchecked `Type` head.

Before each native interface eligibility check, inspect the reachable shapes.
For generic Pi import, check raw multiplicity and domain before `bindIfaceId`.
Its allocator expands aliases to distinguish Ids from coercion variables.
Reject only shapes that cannot satisfy the existing domain rules. Distinguish
scalar domains, closed function domains and the two multiplicity policies.
Allow data cycles. Track synonym expansion separately and reset that stack
when entering data fields. Keep the existing typed checks after this precheck.
Applied synonyms, casts, constraint arrows and raw variable/call-index forms
use the typed path. This preserves the existing scoped-index diagnostics.
A raw retained Pi is not scalar. Its function classification stays unknown,
because a vacuous Pi can become an ordinary eligible arrow.

This follows GHC's lazy interface construction and Agda's staged declaration
checking. It does not use a temporary empty constructor list as a checked
domain. The design and selected primary-source provenance are in
`native-shape-first-loader-design-v1` and
`native-domain-loader-next-research-v1`. This step does not guarantee termination
before the declaration header exists or for unclassified normalization paths.
C1, C2, R22 and the full program goal stay open.

- [x] Record the shape representation, ownership, fallback and limits.
- [x] Add lazy `TyCon` metadata and rebuild its nullary cache on attachment.
- [x] Derive raw views and check them before the four typed eligibility sites.
- [x] Update the existing API fixture. Distinguish early shape rejection from
  M54's detected-loop diagnostic. Keep recursive data, alias and multiplicity
  controls. Keep the existing golden output.
- [x] Build both stages. Run focused API, source, Showcase and registered checks.
- [x] Update contracts and references. Freeze, audit and publish this increment.

Validation selects compiler and source
`b0b876db25e087b286022931df03223abd499059`: 90 compiler inputs and 119 source
paths. Both compiler stages pass.
The two exact API goldens remain 1/37 lines. All 346 own inputs per API phase
are unchanged after self-hosting. Both stages pass the 19-result one-shot
interface client and the 53-line Showcase from 13 modules. Registered self-hosted
checks report 39 cases, 34 skips and five expected passes, with no unexpected
results or framework failures or warnings. C1, C2, R22 and the full goal stay open.

The earlier scope failures and OS-interrupted alias cycle remain in the evidence.
The trace isolated alias expansion before Id allocation. The final raw precheck
runs before that operation. This does not prove termination for arbitrary
interfaces. No old executable, full GHC suite or broad historical matrix was run.

The independent static audit passes 3,789 checks over 1,094 pinned paths.
It covers the complete frozen namespace. All `native-interface-domain-shapes-*`
files and directories are permanently frozen. The canonical record is
`dependent-haskell-M55-final-audit-v1.json`, SHA256
`67c0af1483bffda59b9fc577ba30dfbcb78da11a34fcec6839a0d96b39b30877`.

### M56: retain closed parameterized algebraic values

Status: complete, independently audited and published.
This increment addresses C1's domain and constructor contract.
Keep C1, C2, R22 and the full program goal open.

Use a runtime `[Bool]` as a retained index. A closed `Count` family maps its
promoted constructors to the length of a `Vec`. Check ordinary `[]` and `b:bs`
source patterns. Add `Maybe Nat` and `Tree Bool` controls. Export a builder and
its package through a real interface and object file. Keep the static `Count`
family explicit; this increment does not evaluate arbitrary term definitions
in types.

Accept saturated boxed data types with closed monotype arguments of kind
`Type`. Instantiate constructor universals with those arguments. Require each
runtime field to have a supported scalar domain. A phantom function parameter
can qualify; an actual function field cannot. Keep existential variables,
equality specifications, constructor contexts, wrappers, newtypes and retained
constructor telescopes outside index domains.

Check recursion along the active declaration path. A return to the same
`TyCon` must have nominally equal type arguments. Separate siblings can use
different instances. This is uniform recursion. It rejects growing instances
and parameter permutations; it does not establish runtime termination.

Constructor indices retain ordinary type arguments before runtime indices.
Branch evidence relates the actual scrutinee index to that complete promoted
constructor application. Instantiate pattern fields and Core Lint checks from
the same parent arguments. CorePrep keeps ordinary type applications erased,
preserves actual field identifiers, and transports only checked nominal
evidence. Keep opaque call congruence forward and nominal. Add no Core type,
coercion form, interface tag or inverse call rule. Keep M55's raw applied
interface shapes unknown and retain the typed eligibility check.

The prior-art comparison uses the existing D/DC, System DR and GraD reading
ledger. Their conversion, role and usage rules do not prove this datatype
extension. Record the selected Agda parameter-instantiation and erasure source
alongside the implementation. Review substitution, scope, constructor identity,
runtime demand and erasure together before extending each affected path.

- [x] Record the domain, recursion, constructor and erasure contract.
- [x] Extend domain checking and constructor-index conversion together.
- [x] Instantiate source patterns, nominal proofs, Lint and backend lowering.
- [x] Check source construction, recursion, separate imports and lazy fields.
- [x] Update existing rejection/API controls and the maintained Showcase.
- [x] Build both stages and run focused source/API/registered checks.
- [x] Record evidence and references, review the change and publish it.

Selected source and compiler: `3131ebc644c580632b8ba6f8776d3dab512c95f3`.
Both builds pass. Both stages pass the 14 source observations and three domain
rejections, the 19 prior public programs, and Showcase 59 from 14 modules.
The focused API preserves its 1/37 goldens. A separate backend probe checks a
constructed list input and optional result with two assertions per stage.
The five registered tests have 39 cases, 34 skips and no unexpected results.
See the [checkpoint](README.md#m56-closed-parameterized-index-domains) and
[domain contract](CORE.md#closed-parameterized-runtime-index-domains).

Source matching uses the full instantiated parent. Family matching uses nominal
normalization views. It preserves the checked expression cache and its single
usage check. Empty-list and infix constructor patterns reuse ordinary pattern
checking. Nested nonempty list patterns and overloaded pattern expansion remain
outside this selected constructor-pattern subset.

The M55 shape-first interface classifier retains its unknown fallback for applied
domains. The typed eligibility check enforces the full instantiated contract.
Keep C1, C2 and R22 open. C1 remains the next program priority.

Audited documentation: `0d9c1a1a0b935c8caed53bf44b31591362b2f45a`. The final static audit passes
7,138 checks across 1,710 pinned paths. Canonical evidence is
`_build/verification/dependent-haskell-M56-final-audit-v1.json`, SHA256
`f9af3265afcfea79878ecc87886d7cfcb915a0e417df21a081695ea092269d66`. It preserves 92 cumulative manifests.
The complete `native-parameterized-domains-*` evidence namespace is permanently
frozen. The later publication commit changes only README and PLAN. The full
program goal remains active.

## M57: nominal domain conversion for retained functions

Status: implemented and independently audited. This increment addresses C1's conversion and
substitution contract.
Keep C1, C2 and the full program goal open.

Extend retained Pi congruence with nominal evidence `delta : A ~N B`.
Its body proof uses one actual unrestricted binder `x : A`. The left endpoint
binds `x : A`. The right endpoint binds `y : B` and replaces body occurrences
of `x` with `y` cast by `sym delta`. Preserve both endpoint kinds as `Type`.
Keep fixed-domain construction as the reflexive specialization. Use optional
nominal domain evidence in `DepFunCo`, as `ForAllCo` does for kind evidence.

Use the existing `CastTy` form for converted index kinds. A runtime index cast
must retain its nominal justification when it becomes a representational Core
value cast. Do not treat arbitrary representational evidence as nominal.
Extend the shared expression/index bridge and apply-once substitution together.
Substitute embedded conversion proofs during beta exposure. Permit independent
closed scalar input and result domains after checking both. Preserve nominal
evidence through value-cast substitution and native cast composition.
Check scope, capture avoidance, actual Id metadata and the cast direction.
Function-valued binder domains retain their fixed-domain restriction.

Implement checked application for explicit retained congruence and its symmetry,
transitivity and role weakening. Cast the actual argument back to the source
domain. Instantiate the body proof with that same converted index. Preserve a
cast on a function value until application. Do not eta-expand it. Opaque
coercion assumptions need a separate projection design and keep their gate.
Keep native coercion serialization and general optimization gated.

The nonreflexive control uses `data Token a = Token`, `A = Token (F Bool)`
and `B = Token Bool`, with nominal family evidence `F Bool ~ Bool`.
Both domains qualify under M56. An opaque `f :: A -> Nat` makes the codomain
`Vec Int (f x)` depend on the converted actual input. This checks a domain
conversion that ordinary `eqType` cannot perform.

Follow D/DC Figure 8 An-PiCong and Figure 10 An-Push, author-PDF pages 16 and 19.
The asymmetric rule supplies the backward domain substitution. Agda v2.8.0
Conversion.hs lines 746–761 and 827–849 supplies the domain/context comparison
precedent. It does not supply a Haskell eta law. The 2019 roles and 2021 usage
boundaries in the paper ledger still apply. Record selected source ranges,
hashes and deviations. No combined preservation theorem is claimed.

- [x] Specify the domain, index-cast, congruence and application contract.
- [x] Extend the coercion representation and all structural traversals.
- [x] Implement nominal index conversion, substitution, endpoints and Lint.
- [x] Lower checked casted applications without changing runtime demand.
- [x] Update existing API/runtime controls, including symmetry and composition.
- [x] Run seven bytecode observations and one injected-Core native object control.
- [x] Check wrong role/direction, capture, proof-only dependencies and bottoms.
- [x] Build both stages and run focused API/source/registered checks.
- [x] Update contracts, references and progress records; audit and publish.

M56 remains frozen at canonical SHA256
`f9af3265afcfea79878ecc87886d7cfcb915a0e417df21a081695ea092269d66`.
The maintained Showcase keeps its 59 source observations until a new source
feature uses this conversion rule. The Core controls cover this increment.

The selected compiler is `0c5699265f14826a0294d7a2b0ca9be2fefd0cbe` and fixture
source is `cd304ad0d1af2f4f7a5502de3601fe0aaf080111`. Snapshot6 covers
92 compiler inputs and 125 source paths. Bootstrap4 and selfhost1 use their own
producer metadata; both have the same selected compiler leaves. Final API3
replays both fixtures and pins the actual target libdir. Source checks pass
public19, parameterized14 plus three rejections, and Showcase59/14 in both
stages. Registered checks pass five tests across 39 cases with 34 skips and no
unexpected or framework failures. The nine-phase cohort only aggregates actual
executions. It reruns no target.

Preserve bootstrap1/2 failures, intermediate bootstrap3, the first API fixture
failures, the narrower API2 pass, and the Showcase baseline preflight failure.
The last failure used an obsolete inherited53/13 assertion; fresh helper2 checks
the unchanged59/14 oracle. The final artifact check passes 46,199 unique input
hashes. Earlier failure reports are not compared against current source files.
All seven formatter attempts remain; generated Cabal formatting is unchanged.

Next, specify a source conversion that uses ordinary nominal family reduction
between closed scalar domains. Preserve the proof in wrappers before desugaring.
Require an exact aligned-body comparison and a real adapted worker from source.
Keep opaque projection and general dependent inference as separate tasks.

Audited documentation: `2351b533905fd9a237403f073e5aa619abec1614`.
Canonical evidence is `_build/verification/dependent-haskell-M57-final-audit-v1.json`,
SHA256 `27bddd7057cb0b6f26f220a461935bb2aed943ca7a804abbb4a2f398a9b60107`.
The independent audit passes 101,598 static checks over 47,184 pinned paths.
All 96 cumulative manifests are preserved. The complete
`native-domain-conversion-*` namespace is permanently frozen. Publication changes
only README and PLAN. C1, C2 and the full program goal remain open.


## M58: scoped source domain conversion

Status: implemented, checked, independently audited and published.

The M57 compiler accepts the unchanged-domain source control. It rejects
`adapt f original = original` when its retained domain changes from
`Token (F Bool)` to `Token Bool`, although `F Bool` reduces to `Bool`.
The explicit form `adapt f original y = original y` reaches a substitution
panic. Preserve these three observations in
`_build/verification/native-source-domain-conversion-baseline-v1/results.json`.
The baseline does not execute an adapted worker.

Use the existing nominal `DepFunCo` rule. Do not add a new Core constructor.
This follows D/DC An-PiCong and the domain-before-body context discipline in
Agda. Neither source proves preservation for this combined implementation.
See [nominal domain conversion](REFERENCES.md#nominal-retained-domain-conversion).

- [x] Check both complete binder domains with the closed scalar admission rule.
      Use ordinary nominal family reduction outside the new runtime binder.
      Use the solver reduction-depth limit. Disable inert evidence, plugins and
      the family cache for this reduction. Use reflexivity when the original
      domains already agree. The recursive-family probe in
      `native-source-domain-conversion-domain-depth-v1` times out after eight
      seconds with the pure normalizer, despite `-freduction-depth=10`.
      Require the normal domains to agree. Keep changed function domains gated.
- [x] Freshen a shared runtime binder against both types and the domain proof.
      Align the target body with the forward nominal cast of that binder.
- [x] Compare the bodies with ordinary unification in a private nested solve.
      Follow `can_eq_nc_forall`: track unifications, retain used Given evidence,
      and return a proof only when all body Wanteds are solved. On failure,
      retain the original whole equality. Do not emit a residual constraint
      with an unbound runtime index. Exact body equality can remain a fast path.
- [x] Propagate the active runtime-binder scope through `TcS` into rewriting.
      Keep the native meta-assignment, family-cache and external evidence
      restrictions. Suppress solver plugins within this scope. Do not put a
      runtime Id in `ic_skols`, which binds type variables.
- [x] Preserve nominal provenance in scalar source value casts through wrappers
      and zonking. Do not decide scalar admission from unzonked metadata.
      Keep composition order and ordinary representational coercions unchanged.
- [x] Resolve a retained source variable at its expected domain with a checked
      nominal cast. Do not substitute a variable of a different domain directly.
      Report a located error when its domains cannot be equated.
- [x] Run real source workers at runtime. Check nominal family conversion,
      result transport, ignored bottom indices, and bottom function WHNF.
      Check unrelated scalar domains and payloads with located rejections.
      Extend the maintained Showcase only after these source runs pass.
- [x] Build the bootstrap compiler and the compiler built by that compiler.
      Run the focused API, runtime, source and registered checks. Parse changed
      Haskell files and check the existing Cabal formatting boundary.
- [x] Update the contract, roadmap and references. Pin the selected inputs,
      producer metadata, commands and outputs. Audit and publish the result.

General dependent inference, open dependent domains, opaque Pi projection,
changed function-valued domains and native coercion serialization remain open.
M58 does not complete C1 or the complete roadmap. It replaces the earlier
research suggestion to restrict the body comparison to exact equality with a
scoped ordinary body solve. The existing forall solver supplies the control
pattern; runtime binders still need their own solver scope.

## M59: bodies beneath fixed function domains

Roadmap owner: R03 dependent products. Related card: R05 dependent Core.

Status: implemented, checked with both compiler stages, and independently audited.
The implementation checkpoint is `5c527cd8b92a`; source fixtures are at
`5e33b194f1e6`. The checkboxes below separate implemented steps from final
verification. Documentation updates do not replace those source identities.

M58 rejects `specialize = polyBuild` when `polyBuild` has type
`forall a. Num a => foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n)`
and `specialize` selects `Int`. The saturated form
`specialize f = polyBuild f` compiles and returns `[42,42]` at a runtime length.
The two observations are recorded in
`_build/verification/native-function-body-comparison-baseline-v1/results.json`.
The nonreflexive function-Pi equality still reaches a scalar-only solver guard.

Use the existing `DepFunCo` rule with `MRefl` for an unchanged function domain.
Both source binders must qualify as opaque native call heads. Their complete
function types must agree. Substitute one fresh bare function variable into
both bodies. A cast around that variable is not a supported call head.
Changed scalar domains retain the M58 nominal proof and argument cast.
This follows the same D/DC An-PiCong and Agda shared-context comparison rules
recorded in [REFERENCES.md](REFERENCES.md#nominal-retained-domain-conversion).
The existing Core and Arity rules already support this fixed-domain case.

- [x] Add a distinct fixed-function-domain branch before the scalar branch.
      Keep changed function domains outside nominal domain normalization.
- [x] Make domain evidence optional in the shared body solver. Use `MRefl`
      for unchanged domains. Use the same bare variable on both sides in that
      case. Include deep domain-proof free variables when freshening.
- [x] Retain the private body solve, active function-head scope, nominal body
      proof, zonked endpoint checks, evidence liveness and escape checks.
      Do not infer function equality or emit a body constraint outside its scope.
- [x] Measure the independent known-value selection gate before changing it.
      The measured implicit `seq` and error consumer under
      `ImpredicativeTypes` still rejects a known function-Pi value.
      Classify eligible fixed function binders with `isNativeRetainedBinder`.
      Keep the existing immutable matching, kind, scope and usage checks.
      Use an ordinary lifted forcing helper to check full worker WHNF.
      Direct `seq` retains its existing concrete RuntimeRep restriction.
      Under `ImpredicativeTypes`, annotate the complete action as `IO ()`
      before outer Quick Look examines its native function signature.
      Direct function-Pi visible type arguments remain outside this step.
      Test an explicit ordinary payload wildcard separately, so the completed
      value selection path cannot hide the body solver in the trace.
- [x] Check imported worker specialization at runtime with identity, successor
      and constant local functions. Check ignored bottom functions and indices,
      returned function WHNF, function-head demand order and scoped shadowing.
      Keep wrong payloads, local-index escape and changed domains rejected.
- [x] Trace payload allocation and assignment inside the new function-body
      solve. Extend the existing fixtures and maintained Showcase after the
      new source programs pass. Keep old goldens and observations unchanged.
- [x] Build both compiler stages. Run the focused API, source, runtime and
      registered checks. Record exact selected inputs and actual executions.
- [x] Update the Core contract, references and roadmap with the measured scope.
- [x] Audit the checked result and prepare its publication checkpoint.
      Keep the longer-term goal active.

This step adds no Core constructor, wire form, singleton translation or
function eta expansion. Open domains, changed function domains, opaque Pi
projection, native coercion serialization and ordinary optimization remain open.

The selected compiler is `5c527cd8b92a`; executable fixtures are at
`5e33b194f1e6`. Snapshot 5 adds the documentation checkpoint `7cae7c49bf`
without changing the 95 compiler inputs or executable sources. Both stages
pass public19, parameterized14 with three rejection controls, and Showcase79/14.
The registered run passes five tests across 39 cases, with 34 skips and no
unexpected failures or framework warnings. Both stages agree on fourteen
focused source controls. Three programs run, one Given control compiles,
and ten programs reject with located errors. Four rejections are existing
source-context boundaries, not evidence of the later semantic check.

Self-hosting changed twelve pinned GHC package artifacts. Fresh bootstrap API2
checks therefore replace the earlier API1 result for the final selection.
Goldens remain 1/37. The original bootstrap Showcase pins none of those changed
artifacts; its cumulative 414-input/product map remains valid. Revalidation
checks 46,346 unique paths and permits only exact project-documentation deltas.
The two final specialization baselines each return `[42,42]`.
No broader GHC suite or general preservation theorem is claimed.

The frozen checkpoint is `a5bdd43f6764`. The independent audit passes 408,573
static checks over 47,517 pinned paths and preserves 104 cumulative manifests.
The canonical record is
`_build/verification/dependent-haskell-M59-final-audit-v1.json`, SHA256
`1e844411bfd7420dbabae96f71d0e3857f0b436946958100f037da3b2e018601`.
All `native-function-body-comparison-*` paths are permanently frozen.
Two earlier auditor failures remain recorded. They concerned historical fixture
pins and trace formatting. The final audit checks each execution against its
own source commit. This publication checkpoint changes only status documents.

## M60: closed-list safe indexing

Roadmap owner: R03 dependent products. Related card: R05 dependent Core.
Status: implemented and checked with both compiler stages at `d26e6a2fb4`.
The source composition adds no compiler rule.

Compose the existing retained `[Bool]` domain and saturated `Count` family with
an ordinary `Fin` GADT. Agda's finite-position datatype is the prior art; see
[REFERENCES.md](REFERENCES.md#closed-list-safe-indexing). Use the existing
`ParameterizedIndexLibrary`. Do not add a compiler rule or a singleton layer.

```haskell
data Fin :: Nat -> Type where
    FZ :: forall (n :: Nat). Fin ('S n)
    FS :: forall (n :: Nat). Fin n -> Fin ('S n)

indexClosed :: foreach (xs :: [Bool]) -> Fin (Count xs) -> Bool
mkFinClosed :: foreach (xs :: [Bool]) -> Int -> Maybe (Fin (Count xs))
lookupRuntime :: [Bool] -> Int -> Maybe Bool
```

`mkFinClosed` checks the signed position at runtime. It returns `Nothing` for
negative or out-of-range requests. Its negative branch must not inspect the
list. `lookupRuntime` uses the same actual list to construct and consume the
position. Static checking verifies both operations for every accepted input.
It does not evaluate an IO result during compilation.

The bound is a list length. It is not a proof of list identity. A proof of equal
counts permits position reuse. It must not imply equality of the lists.
The empty-list equation uses the ordinary empty case on `Fin Z`. A bottom
witness must still be bottom. Haskell remains partial.

- [x] Add `Fin`, `indexClosed`, `mkFinClosed` and `lookupRuntime` to the existing
      library. Explain bounds, family reduction and demand in one owning Note.
      Use explicit `FZ @(Count rest)` and `FS @(Count rest)` applications.
      The measured implicit `FZ` route cannot select its native-dependent bound.
- [x] Append five observations to Showcase's existing 79-line prefix. Include
      runtime list and position inputs, invalid bounds, ignored tails and finite
      access to a cyclic list. Keep fourteen source modules.
- [x] Compile the producer separately with both compiler stages. Compile the
      nineteen-observation client with only copied interfaces available.
      Run bounds, lazy-field, cyclic-input and empty-case demand checks.
- [x] Check rejection of `Fin Z`, a position at an unrelated bound, and list
      equality inferred from equal counts. Check the current generic-domain
      and implicit-constructor restrictions separately. Accept explicit reuse
      under equal-count evidence. Preserve located diagnostics.
- [x] Run the maintained Showcase with both compilers and Core/STG Lint.
      Check unchanged compiler inputs. Record the exact sources and outcomes.
- [x] Update the roadmap, references and example instructions.
- [x] Complete the separate evidence audit and prepare the publication checkpoint.
      Keep R03 and the whole program open.

No compiler rebuild is required when only these examples and documents change.
The existing API and registered compiler checks remain M59 evidence. This
increment adds no generic `[a]` domain, ordinary `length` promotion, totality,
single-pass algorithm or bounds-check elimination. Construction and selection
traverse the required prefix separately. General optimization remains gated.

The selected verification is
`_build/verification/native-closed-list-indexing-verification-v2/results.json`.
Both stages pass nineteen runtime observations through copied interfaces,
five located rejections, equal-count reuse and Showcase84/14. The rejections
are GHC-83865 for the empty bound, unrelated bound and implicit `FZ`;
GHC-05617 for list equality from count equality; and GHC-83605 for generic `[a]`.
The generic-domain and implicit-constructor restrictions remain separate from
ordinary bound safety. Equal-count reuse is a compile-only control.
All 173 recorded inputs remain unchanged. The serial 4 GiB/no-swap run peaks
at 243 MiB and returns zero. The earlier runner's interface-path failure remains
recorded. No source change was needed for the retry.

The installed Fourmolu parser rejects `ExperimentalDependentTypes` before it
can format either changed Haskell file. The added code follows the surrounding
layout. No Cabal file changes, compiler rebuild, API rerun or full-suite run
belongs to this example-only increment.

The separate evidence audit passes 616 checks. It binds executable source bytes
to `d26e6a2fb4` and audited documentation to `286a1592ed`. The canonical record is
`_build/verification/dependent-haskell-M60-final-audit-v1.json`, SHA256
`cbdbd39de43703ddb258faba8ef67f10731ddc035c6b7ca78dbe433ac339da8e`.
Its manifest freezes all 316 files in `native-closed-list-indexing-*`.
The unchanged compiler's full package/API provenance remains M59 evidence.
This publication checkpoint changes only status documents.

## M61: opaque whole-function cast lowering

Roadmap owner: R05 dependent Core. Related card: R03 dependent products.
Status: implemented, checked with both compiler stages, and independently audited.

Determine whether the existing whole-function cast already supports application
through opaque nominal evidence. `pushCoRetainedArg` can decline a coercion
without rejecting the program. CorePrep can keep a whole cast; CoreToStg erases
casts. Measure that path before adding a dependent instantiation primitive.
The selected D/DC An-PiSnd and An-Push rules supply the prior comparison.
See [the evidence references](REFERENCES.md#nominal-retained-domain-conversion).
They do not establish this implementation's preservation or demand behavior.

Use the following mathematical abbreviation. It is not a new source synonym:

```text
Phi a = foreach (f :: Nat -> Nat) -> foreach (n :: Nat)
     -> Vec a (f n) -> a -> a
Lambda a. Lambda b. lambda (q :: Phi a ~N Phi b). lambda (h :: Phi a).
    (h |> SubCo q)
```

The vector input makes both retained binders nonvacuous. Keep `a` and `b`
distinct and abstract during typed preparation. The proof must remain a scoped
nominal `CoVar`, not a visible `DepFunCo` or reflexivity proof. Use actual `f`
and `n` identifiers distinct from the endpoint templates. Test the cast alone,
one retained application, and both retained applications. Do not eta-expand
the cast-only expression. It must preserve a bottom worker's WHNF behavior.

- [x] Run the prepared candidate in scratch first. Check input/output Lint,
      exact expression types and scoped opaque evidence after direct CorePrep.
      Close it with `a = b = Int` and legitimate reflexivity only for execution.
- [x] Fix the measured native function beta-reduction failure. A bare eligible
      function argument must substitute through terms and native types together.
      Require the same complete function type. Retain the application when the
      optimized argument is not an eligible bare function variable. Do not
      introduce a let alias that escapes through the partial result type.
      This is the function-head counterpart of existing scalar beta substitution.
- [x] Extend the existing API fixture with nine quiet bytecode observations:
      three good-worker WHNF checks, three bottom-worker WHNF checks, ignored
      bottom inputs returning 42, vector demand, and scalar-index demand.
      Preserve the existing 1/37 goldens. Add no committed test file.
- [x] Extend the existing cast object control with a distinct abstract
      `NOINLINE` top-level kernel and a separate runtime entry. Check scope
      after simplification and tidying. Retain actual CorePrep dump evidence
      for that binding. Execute its legitimate closed caller through NCG.
      Preserve the earlier scalar-domain object's 42 observation separately.
- [x] Check whether any required operation fails. Retain a working whole cast
      when possible. Specify a new proof primitive only for a measured or
      separately established calculus obligation. Do not reinterpret `InstCo`
      or derive nominal domains from representational function equality.
- [x] Build the self-hosted compiler. Run fresh API goldens, both-stage public
      source and Showcase controls, and the existing registered tests.
- [x] Record actual compiler/package inputs, diagnostic or runtime failures,
      and the limits of abstract structural checks versus closed execution.
      Update R05, CORE and references. Keep general R05 work open.
- [x] Audit the complete evidence and prepare its publication checkpoint.

This increment adds no source syntax, evaluator, function equality or generic
domain. It establishes no normal-optimizer, sharing or preservation theorem.
Opaque evidence projection remains a separate typed transformation obligation
even if whole-cast execution succeeds. Compiler rebuilds depend on a compiler
change; fixture-only checks use the existing matching compiler and libraries.

Scratch discovery v1 stops at missing explicit coercion-constructor imports.
Version 2 corrects those imports, compiles, and reaches the bytecode pipeline.
Core Lint then reports a let-bound function index in the partial result type.
The current scalar-only beta guard lets the function binder enter ordinary
`simple_bind_pair`, which creates that escaping alias. The measured logs are in
`_build/verification/native-opaque-whole-cast-discovery-v2/`. They do not show a
need for opaque coercion projection. Version 3 passes all nine runtime checks
after compiler `f0b45ad34b` substitutes eligible bare function arguments.
Bootstrap build 1 passes in 30.12 seconds under 16 GiB/swap0/-j6.

Object discovery v1 stops at a misspelled dump flag. Version 2 corrects it to
`-fno-dump-with-ways`. Its actual NCG pass keeps the abstract kernel and scoped
`Sub cv` cast in the recorded CorePrep dump. The executable prints `42` for
the scalar entry and `42` for the opaque entry. Fixture checkpoint `b03cfae43b`
contains both paths. The selected API and registered runs pass the unchanged
goldens and execute the new quiet controls.

Snapshot 2 selects compiler `f0b45ad34b`, fixture `b03cfae43b`, and source/docs
checkpoint `c45b0b7bf0`. Its 95 compiler and 128 source inputs remain unchanged.
Selfhost attempt 1 stops during concurrent package configuration because
`ghc-pkg` cannot stat a `ghc-platform` registration file. The file is present
after the failed run. Attempt 2 uses identical source and resource settings
and passes in 194.13 seconds, with peak 5,696 MiB under 16 GiB/swap0/-j6.

All nine selected cohort results are fresh and run after selfhosting. API
goldens 1/37 pass in 27.79/39.70 seconds and pin all 45,837 target-library files.
Both stages pass public19, parameterized14 with three rejections, and
Showcase84/14 modules. Registered tests report five expected passes across
39 cases, with 34 skips and no unexpected or framework outcomes. The serial
4 GiB/no-swap cohort peaks at 1,557 MiB and returns zero. Object partial-WHNF
behavior is not claimed: those three checks and demand markers are bytecode
observations. NCG checks the fully applied keep caller and prior scalar entry.
No full GHC suite or general optimizer validation was run.

Audited documentation is `f8ce379bdd`. The independent audit passes 218,015
checks over 46,963 pinned paths. Its external canonical record is
`_build/verification/dependent-haskell-M61-final-audit-v1.json`, SHA256
`610bbc45c5abd78ccd48da01edc587adbc2b6f82766429ea3466c8903a2af615`.
The evidence manifest freezes all 491 files in `native-opaque-whole-cast-*`.
The unchanged 16-work, 22-version paper ledger retains its reading scopes.
This publication checkpoint changes only status documents. R05 and the main
goal remain open.

## M62: complete retained-function type arguments

Roadmap owner: R03 dependent products. Related card: R05 dependent Core.
Status: complete bounded increment; independently audited. The original
lifted-consumer baseline rejected two complete explicit type arguments with
located GHC-83605 errors in both rebuilt stages.

Retained function types already have kind Type. Named complete signatures can
form them, and immutable known-value matching can select them. A written type
argument rejected its function binder in the baseline `tcHsType` path, which
called `tcCheckRetainedType False`. The baseline uses ordinary lifted
`forceValue` and `raise` helpers. It isolates this formation gate from direct
`seq` representation checking. Its records are in
`_build/verification/native-complete-function-type-arguments-baseline-v1/`.

Use the caller's existing type-checking mode and lexical index environment.
Record whether the complete visible type argument has named or anonymous
wildcards. Grant function-binder permission only for a complete explicit
argument in TypeLevel mode. Carry this context through ordinary nested type
checking. Do not select permission from the outer printed syntax alone.
Kinds and other signature contexts retain their current policies.

Move the existing anonymous-wildcard syntax predicates from `Tc.Gen.Sig` to
`GHC.Hs.Type`. Reuse them for signatures and type arguments. Keep their complete
walk through binder kinds, multiplicities, contexts and nested types. Named
wildcards remain a separate check of the renamer's wildcard list.
An ordinary visible wildcard argument without native indices or function
binders must retain its existing behavior.

The renamer already rejects many partial retained arguments. That check alone
is insufficient for required type arguments. `tcVDQ` translates a surrounding
expression into a type after renaming. It can combine a complete `(type Phi)`
fragment with a separate anonymous wildcard, then call `tcHsTypeApp` on the
whole partial type. Check this source path explicitly. Do not remove the shared
whole-type scan because ordinary `@` rejection controls fail in the renamer.

```haskell
forceValue :: forall a. a -> IO ()
forceFull :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat)
             -> Vec Int (f n)) -> IO ()
forceFull worker =
    forceValue @(foreach (f :: Nat -> Nat) -> foreach (n :: Nat)
                 -> Vec Int (f n)) worker
```

- [x] Share the existing anonymous-wildcard predicates. Preserve ordinary
      signature completeness. Add no second syntax traversal for the same rule.
- [x] Carry whole-argument completeness in the private `TcTyMode`. Default it
      to absent outside the visible type-argument entry point. Keep it distinct
      from the wildcard mode and runtime-index environment. Reset it in fresh
      kind-checking modes. Preserve caller scope and source spans.
- [x] Permit complete retained-function binders through this path, including
      nested ordinary type applications and kind-annotated value types.
      Retain closed-domain, Many, staging, body-kind and native-meta checks.
- [x] Report incomplete native type arguments accurately. Check anonymous and
      named wildcards before, inside and after a retained telescope. Keep
      ordinary visible wildcards, unsupported contexts and wrong kinds separate.
      Include required-type-argument translation with an outside wildcard;
      that case must reach the new type-checker completeness guard.
- [x] Measure lifted good/bottom worker WHNF, ignored function arguments and
      runtime applications after explicit selection. Check nested container
      demand, ambient runtime indices, distinct names and scope rejection.
      Measure direct `seq` and outer Quick Look as separate controls.
- [x] Extend the existing example library, API fixture and maintained Showcase.
      Preserve the old output prefixes and add no committed test files.
- [x] Build both stages and run the selected API, source, runtime, rejection,
      Showcase and registered checks. Bind records to their actual inputs.
- [x] Update the roadmap, Core contract and references. Audit and publish the
      bounded result. Keep R03 and the full goal open.

The prior art is GHC's existing type-application and completeness checking,
the selected VTA paper, and Agda's Pi formation and expected-type rules. See
[the reference ledger](REFERENCES.md#complete-retained-function-type-arguments).
This increment adds no inferred runtime quantifier, mutable native-meta
assignment, general Quick Look, generic index domain, singleton layer or Core
constructor. Existing matchability and generic-domain obligations stay open.

### M62 validation

Compiler `dc8d5635a8` and source checkpoint `4c31ce0145` pass both builds.
Bootstrap takes 253.31 seconds and peaks at 3,338 MiB. Self-hosting takes
868.72 seconds and peaks at 6,289 MiB. Both use six jobs, a 16 GiB limit and
no swap. Their selected input hashes remain unchanged.

Both stages pass the same eighteen focused source cases. Six programs produce
fourteen exact runtime observations per stage. They check Maybe, list and
tuple field demand, kind-annotated value types, ambient scalar and function
capture, actual IO inputs, complete required type arguments and ordinary
wildcards. Twelve programs reject with located diagnostics. The three T2T
cases reject an outside hole before, after or around an embedded complete Pi.
The other cases check direct holes, unsupported domains, kinds, scope,
distinct actual heads and partial signatures.

The fifteen compatibility cases agree in both stages: five runtime programs,
one compile-only Given control and nine rejections. Direct explicit `seq`
works with NoImpredicativeTypes. The separate outer Quick Look controls still
reject. This does not implement general Quick Look checking.

All nine selected cohort rows pass after self-hosting. API goldens stay 1/37
with 45,837 fresh target-library file pins. Both stages pass public19,
parameterized14 with three rejections, and Showcase88 through fourteen source
modules. The Showcase preserves its 84-line prefix. Five registered tests
pass across 39 cases with 34 skips and zero unexpected, framework or missing
library outcomes. The serial cohort uses 4 GiB with no swap and peaks at
3,092 MiB. The final focused controls peak at 303 MiB under the same limit.

The first Showcase runner had an incorrect expected exception marker. The
first focused oracle expected the generic domain message instead of the
specific unsupported-function-domain message. Both failed records remain.
Corrected versions pass without compiler changes. Fourmolu cannot parse the
experimental extension in the source examples. The existing fixture has
unrelated formatting differences; its new block uses Fourmolu output.
Cabal-gild reports existing template formatting differences. No Cabal file
changed. No new test file, full GHC suite or proof replay is claimed.

Records use the fresh `native-complete-function-type-arguments-*` prefix.
The compiler snapshot has 95 files. The selected source snapshot has 128.
The M61 canonical record and 106 inherited manifest hashes remain pinned.

Audited documentation is `3c098f2c2a`. The independent audit passes 210,784
checks over 47,471 pinned paths. Its canonical record is
`_build/verification/dependent-haskell-M62-final-audit-v1.json`, SHA256
`caa711f8dc137f1f0c0c9ae2e2e95c83389ec70beb263f90c729686f15ced61b`.
The external evidence manifest freezes all 1,314 files in
`native-complete-function-type-arguments-*`, including the failed verification
attempts. The audit uses 4 GiB with no swap, peaks at 196 MiB and returns zero.
The paper ledger remains sixteen works and twenty-two PDF versions with the
same reading scopes. This publication changes status documents only. R03,
R05 and the full goal remain open. The work queue next selects the R23
matchability foundation before generic index domains.


## M63: ordinary-arrow matchability

Roadmap owner: R23 unsaturated type families. Related card: R05 dependent Core.
Status: ordinary-arrow implementation and selected checks pass at source
`95a19a409e`. Publication remains pending. M62 is the published checkpoint.
Bootstrap v13 passes in 292.80 seconds. Selfhost v7 passes in 964.35 seconds.
Both use 16 GiB with no swap. Their peaks are 3235 and 6677 MiB.
The selected rebuild regenerates all eight primitive Typeable outputs.

Focused v3 passes thirteen wrappers. Each stage checks source16, Given6,
selector13, pattern-synonym9 and TH3, plus the old interface header.
The selected old TypeRep consumer also passes. TH preserves explicit M/U
aliases; the corrected fixtures compare those exact ASTs and reuse them.
Checks v1 passes eight wrappers and nine measured cohort rows: API1/37,
both-stage public19, parameterized14 with three rejections, and Showcase91/14.
The five registered tests pass. Their selection skips 34 of 39 cases and has
no unexpected result, framework failure or missing library. Focused and cohort
peaks are 3267 and 2312 MiB under 4 GiB with no swap.

The primitive Typeable correction aligns the stored module with its existing
fingerprint. The partial-FUN printer exposes distinct metadata. The official
kernel fixture passes without the diagnostic skip. Old .hi rejection is
measured; no old HIE fixture is claimed. The binary dependency retains the
documented one-line local decoder patch. Audit and publication are separate
exit checks. Earlier unsuccessful attempts remain immutable evidence.

The next foundation has two stages. M63 gives ordinary arrows explicit
matchability through their complete pipeline. A later stage gives dependent
erased quantifiers the corresponding metadata. Neither stage alone permits
unrestricted generic runtime domains. Keep those gates until both contracts
and the separate field/telescope transport obligations are satisfied.

Use proposal #242 and the reviewed prototype as the implementation reference.
The later proposal assigns U to ordinary runtime functions. The 2019 FCM paper
supplies the nominal inverse-application premise. Its earlier convention for
ordinary term arrows differs. Do not transfer the paper's preservation theorem
to this native kernel. Agda remains the source reference for telescope scope
and substitution. It does not supply the FC projection rule or a Haskell eta law.

### Representation and formation

Store type-valued matchability in FunTy and its congruence in FunCo. Use the
prototype's Matchability datatype and M/U aliases. Keep metadata
well-kinded, scoped and explicit. Bound metadata variables and explicit type
computation must survive maps and substitution. This is not permission to
infer a new matchability quantifier. The initial unannotated elaboration path
chooses the context's concrete classification.

Insert implicit metadata only when checking actually instantiates a binder.
Preserve an explicit polymorphic kind when the expected kind keeps its binders.
An inference path that preserves a forall must keep doing so.

Checked declaration kinds need a separate ordering. Use local metadata
metavariables for omitted annotations in that check. Constrain the anonymous
arrows consumed by the declared argument prefix to the declaration's M or U
classification. Then default remaining generated metadata to KindLevel M
before kind generalization. Do not infer new matchability quantifiers. Check
explicit or computed metadata with ordinary bounded equality solving. Reject
conflicts instead of replacing the supplied kind. Preserve the independently
specified result kind. The prototype's whole-signature M/U default is broader
than this selected rule.

Ordinary runtime arrows are U. Constructor kind arrows are M. Classify each
declared family or synonym argument by arity; preserve its independently
specified result kind. Visit makers that preserve a supplied full kind as
well as mkTyConKind. Keep runtime constructor workers/wrappers separate from
promoted constructor kinds. Determine their actual elaboration from the
prototype before changing the worker contract.

FUN gains the specified invisible matchability parameter before required
multiplicity. Its Core arguments are [matchability,mult,rep1,rep2,arg,res],
with roles N,N,N,N,R,R. Ordinary FUN One still applies One to multiplicity.
The implicit matchability insertion needs an explicit TypeLevel/KindLevel
policy; it must not leave an unresolved default in final Core. The other
three wired arrow constructors retain their four arguments and fixed U/Many.
Do not copy the superseded argument-shift claim from the first research draft.

ForAllTy and ForAllCo keep their current representation in M63. No new U
forall is formed. Current family-saturation and native polymorphic-value
gates remain. The later forall stage must preserve explicit invisible U,
nominal matchability evidence at all outer roles and the existing required/
invisible representational visibility bridge. Visibility cannot choose
matchability. The prototype's forced invisible-M shortcut is not the plan.

### Equality and transport

Permit application syntax inspection independently of inverse equality.
Given application projection, raw LRCo Lint and optimizer-created LRCo require
M at the consumed kind arrow. Forward AppCo congruence remains valid for U.
Stuck U application yields conservative uncertainty in immutable unification
and apartness. It cannot justify surely-apart or injective-variable analysis.
Classify oversaturated family applications by the result arrow being applied,
not by the family name. Preserve structural SelFun decomposition of the arrow
TYPE itself, including FUN U; this differs from inverting function results.

The FunCo matchability child follows N/N/P at outer N/R/P, as multiplicity does.
Normalization starts with nominal evidence and explicitly weakens for a phantom
parent. Selectors, endpoint recovery and coercion optimization use the same
rule. Do not obtain nominal metadata equality from an opaque phantom proof.

Account for metadata in kinds, equality, FVs, maps, folds, substitution, tidy,
sequencing, size, TypeMap keys, normalization, zonking, role inference and all
smart makers. Metadata proofs can contain outer runtime/coercion variables.
Preserve these dependencies until the existing erasure boundary.

Native TermAppTy/TermAppCo remain opaque, nominal and noninjective. They retain
the same scoped actual head. Native eligible function heads require U, Many
and the existing independently supported scalar input/output domains. A
vacuous DepFunTy becomes the same U arrow as an ordinary runtime lambda.
Preserve nominal DepFunCo domain evidence and the fixed-function-domain gate.

### Persistence, reflection and exit checks

Migrate the common interface payloads and readers together. Change the
interface wire version before writing new fields. Old payloads must reject
at the header. All existing ordinary source libraries must remain usable;
blanket rejection of ordinary arrow persistence is not a source compiler port.
Migrate TypeRep/KindRepFun, raw FUN decomposition, binary reflection and
fingerprints. Distinct well-formed metadata must not have identical reflected
identity. Keep current native Typeable and unfolding/codec restrictions.

- [x] Implement type-valued arrow metadata, wired constants and actual makers.
- [x] Select and implement concrete source insertion/defaults. Check supplied
      kind signatures, ordinary FUN One, partial FUN, explicit metadata and
      data constructor worker/wrapper use. Add no inferred polymorphism.
- [x] Preserve metadata and proofs throughout traversals and transformations.
- [x] Gate all inverse application evidence and injective/apartness consumers.
      Preserve forward congruence and structural arrow decomposition.
- [x] Migrate interfaces with a changed wire version, reflection and native
      lazy shape summaries. Preserve closed-domain gates and alias behavior.
- [x] Extend the existing API fixtures. Pair M/U inversion and forward cases;
      test malformed roles, explicit substitutions, proof-only FVs, metadata
      fingerprints and wire incompatibility. Keep the previous runtime checks.
- [x] Build both stages under the established resource caps. Run the selected
      API/source/Showcase/registered checks and meaningful M/U source controls.
      Update Showcase only when a source feature is completed.
- [ ] Audit and publish actual results. Keep R23 and the full goal open. Then
      specify and implement forall matchability before generic-domain widening.

Use a fresh native-arrow-matchability-* evidence prefix. All M57-M62 prefixes
remain frozen. This specification introduces no new source extension, family
unsaturation, ordinary function promotion or runtime evaluation in types.

Record selectors need a separate classification check. Ignore variables found
only under a U or unknown application in the result. Keep the existing
saturated-family and synonym treatment; a whole field type can be transported
without recovering each variable. Preserve the ordinary GADT list selector.
The initial rule can still reject a new opaque whole-field selector that could
support transport. General field-type transport remains open. Reflection must
compare the actual head with FUN or TYPE before it uses constructor injectivity.
Inspect transparent aliases for hidden opaque applications. Preserve ordinary
phantom and whole-field aliases. Scan the uninstantiated synonym definitions
before expansion, so ordinary repeated aliases retain their compact syntax.
Primitive Typeable bindings need the same module identity in the stored Module
and the fingerprint calculation. Keep their binding in GHC.Internal.Types,
but represent GHC.Internal.Prim. Do not weaken binary kind equality checks.

## M64: forall matchability

Roadmap owner: R23. Related card: R05. Status: complete within the selected
foundation contract. Publication remains pending. The full R23 card stays open.
This increment must carry matchability through erased dependent quantifiers.
The selector correction below is a prerequisite, not completion of that port.

Use proposal #242 and the pinned prototype identified in REFERENCES.md.
Preserve explicit invisible U. The prototype overwrites invisible metadata
in several basic and list makers; those shortcuts are not the contract.
Check source annotations outside the complete binder group. Preserve the
runtime type-lambda classification through the existing required/invisible
representational visibility bridge. Visibility does not select matchability.

### Phase 0: forall selector role boundary

The existing binder-kind selector returns nominal evidence. A phantom parent
can relate forall types with different binder kinds. It cannot justify that
nominal result. An executed API probe reproduces acceptance by both stock
GHC 9.14.1 and M63. This does not demonstrate source-program reachability.
Normal optimization currently avoids the opaque phantom eta route indirectly.

Require nominal or representational role for a residual raw SelForAll. Keep
representational selection for existing type-argument pushing. Core Lint must
reject raw phantom-parent selectors, including equal-kind endpoints. Smart
makers can instead return the actual nominal child of a structural ForAllCo,
its symmetry, or nominal reflexivity when the selected kinds are equal.
Apply the residual role precondition after those reductions. Keep the existing
debug-assertion convention; do not claim an all-build checked smart API.

Keep structural and reflexive eta fast paths. Explicitly exclude phantom from
opaque TyVar forall eta. Opaque CoVar forall eta requires representational
role. Preserve the existing nominal visibility check. The owning contract is
Note [Phantom forall selectors] in GHC.Core.TyCo.Rep.

- [x] Extend the existing quiet kernel fixture: reject the raw phantom
      counterexample and an equal-kind raw phantom selector; preserve the
      parent coercion, structural nominal-child extraction, symmetry,
      reflexive reduction and nominal/representational selector behavior.
- [x] Verify the new rejection against the unchanged M63 API before rebuilding.
      Red attempt v4 reaches the intended raw-selector assertion after the
      positive controls pass. Earlier fixture failures remain separate records.
- [x] Build the correction in an isolated checkout. Run the kernel fixture,
      required-type-argument controls and the maintained source/runtime cohort.

Source commit `28708e467e16263213a215e0ec234700fe3b4bf1` passes both isolated
builds and the final `native-forall-selector-boundary-cohort-v2` checks.
Bootstrap API goldens 1/37 pass against the isolated target libraries. Both
stages pass public19, parameterized14 plus three located rejections,
Showcase91/14, two higher-rank compile controls and required-visibility runtime42.
The standard Hadrian selection passes five tests: 39 cases, 34 skips and no
unexpected, framework or missing-library result. Inputs remain unchanged.
Builds use 16 GiB with no swap. Verification uses 4 GiB with no swap and runs serially.
The scoped compiler review found no actionable defect. These checks do not
establish source reachability of the original malformed selector or a
preservation theorem. Phase 0 is complete. At that checkpoint, the full forall
port remained open.

### Phase 1: explicit Core forall metadata

Use `ForAllTy ma binder body`. The annotation is outside the binder scope.
Generic makers take explicit metadata. Plural makers and forall splitters use
per-binder `(ma, binder)` pairs. Observation-only callers can discard metadata
explicitly. Reconstruction callers must preserve it. Keep the existing
metadata vector on the Pi views. No visibility-based overwrite is permitted.

Add nominal metadata evidence before binder-kind evidence in `ForAllCo`.
Both proofs are outside the binder scope. A structural forall proof can expose
its stored children at any body role. A residual opaque kind or metadata
selector requires an N/R parent and actual forall-shaped endpoints. Optimize
metadata before entering the binder. Preserve forward instantiation at U.

CoVar metadata depends on context. Runtime abstractions use U. Generic makers
preserve M, U or an unresolved expression. Convert a vacuous known-U CoVar to
a sort-aware anonymous arrow. Retain a vacuous M or unresolved forall. Revise
the occurrence invariant and its Lint check together. Endpoints of a structural
ForAllCo can then normalize to different syntactic forms. Smart child extraction
must precede the residual selector shape check.

The baseline CoVar probe records a rejected cached FunTyFlag from the current
unused-binder maker and an accepted sort-aware control. Its inputs remain
unchanged in `native-forall-matchability-covar-probe-v1`. This is API evidence;
no source reachability claim is made. The contract follows the proposal and
the pinned FC/role reviews in the research record. It is not a preservation
theorem.

- [x] Migrate Core representation, makers, complete views, FV analysis,
      substitution, tidying, comparison, normalization and transformations.
- [x] Migrate coercion formation, selectors, role checks, lifting, reduction,
      transitivity and nominal metadata equality before binder introduction.
- [x] Preserve metadata in interfaces and HIE. Read it outside binder scope.
      Bump both compatibility versions before emitting the new payload.
- [x] Check metadata variables, aliases, substitution, vacuous CoVars and
      structural versus opaque projection in the existing API fixture.
- [x] Build the migrated compiler and check ordinary polymorphic programs.

These tasks do not complete the source annotation, TH or reflection work below.
Keep the complete M64 goal open until those paths and the source showcase pass.

Development status, 2026-11-17: the six selected Core objects compile in
`native-forall-matchability-core-preflight-v4`. The snapshot has 98 changed
files at specification head `9a8d3bd1730913c795a387dbf2019339ce6ba49d`.
Its patch SHA256 is
`6f3d013fbb8a0638852fa4e10874f15f95016c4463202d44765a7f7a5f594229`.
The inputs remain unchanged during that compile. Main M63 remains clean.
This result does not validate the later source and TH integration changes.

The development tree now carries source annotations and additive TH wrappers.
The bootstrap compiler builds in
`native-forall-matchability-bootstrap-build-v10`. That snapshot has 140 changed
files, including the new Typeable boot file, at the same specification head.
Its patch SHA256 is
`5df9e86ebd5c13c4c787f87c50c1f7273db258c96b8aa9de84816b62319fe1da`.
Its inputs remain unchanged. This is compiler build evidence only.

The API development fixture compiles against that compiler. Its first run
rejects the old target libraries: interface version 63 differs from the new
version 64. The first target rebuild then finds a separate integration error.
Ambiguity analysis of an inline kind annotation introduces runtime U obligations
for a stored M forall. Analysis must retain skolemisation and constraint solving,
but it must not require a runtime abstraction. Keep both failed attempts in
their versioned records. Later development results appear below. Do not count
these features in Showcase.hs until the complete selected checks pass.

Untyped declaration-kind quotations must defer omitted forall metadata along
their main Pi path. Their consumed prefix has not been selected yet. The normal
declaration checker selects it after checking the complete kind and header.
Freeze explicit annotations, ordinary type-quotation defaults and actual typed
reification. Independent nested kind positions use the normal M default.
The `native-forall-matchability-th-contract-v2` research record corrects the
earlier design-only requirement to freeze every untyped declaration default.

Constructor contexts were an integration gap. A single `con_mb_cxt` could not
retain the lexical scope of contexts interleaved with forall groups. The new
ordered telescope contains forall, context and parenthesis nodes. Conversion,
renaming, checking and quotation process these nodes in order. Exact printing
returns the nodes in the same order. The final checks below validate these paths.

The typed DataCon and interface contracts still hoist fixed-U quantifiers before
the constructor constraints. A constructor-local equality cannot justify a later
metadata annotation under that representation. Do not install such Givens and
then discard the evidence dependency. Closed and computed-U annotations use
ordinary equality. Evidence-dependent annotations need explicit DataCon
transport or an interleaved typed telescope before they can be accepted.

Development validation, 2026-11-17: self-host build v3 passes in 1381.291 seconds.
It rebuilds the bootstrap compiler, target libraries and self-hosted compiler.
Its 141 changed files have patch SHA256
`b709eaeca40cf98827c1cdbeabb99b570ddfa89ab35c1e796094e2e0d19eefb4`.
Its inputs remain unchanged and main M63 is preserved. API development v2 passes
the quiet Core fixture, including safe used-CoVar and escape controls.

The source and TH supplements record independent outcomes after the first
failure. Both stages pass runtime identity, earlier-Given checking, computed-U
FFI execution, constructor shadowing, computed-M TypeRep/Binary reflection and
the selected malformed-wrapper rejection. They reject fresh M runtime
abstractions, own-group annotation capture and a later Given. The later-Given
oracle expected GHC-83865 but receives the appropriate rigid-variable error
GHC-25897. The first FFI fixture also needed an explicit kind for its unused
type variable. Keep these fixture corrections separate from compiler defects.

Three compiler defects remained after those measurements. Declaration-prefix
selection runs before formation casts are solved and misses F2's second
consumed argument. Binding checking skolemises an M-polymorphic result before
the RHS can reuse its existing value. The TH builders lack exports from
GHC.Essentials for known-occurrence lookup. The final snapshot corrects these
defects and executes the interface and HIE controls. The development supplements
alone did not establish M64 completion.

The additional quiet interface assertions pass against the preserved v3
artifacts in `native-forall-matchability-interface-preview-v1`. They check
metadata identity, lexical scope, coercion roles and malformed payloads through
all three compression modes. They do not establish HIE coverage.

The actual HIE-file regression reaches the intended outer-signature scope
failure against the same artifacts in `native-forall-matchability-hie-red-v2`.
Its preceding metadata, alpha-sharing and inner-scope assertions pass. The first
attempt compiled but did not place the generated file at the fixture's expected
name. Keep that harness failure separate. The final positive HIE run below
checks the correction against both rebuilt stages.

### Forall port acceptance

- [x] Select the CoVar-forall metadata contract. A CoVar passes a zero-width
      argument and is not identical to an erased TyVar. Record the role rule.
- [x] Add type-valued ForAllTy metadata and nominal ForAllCo metadata evidence.
      Preserve it in every maker, view, map, FV analysis, substitution,
      comparison, normalization, role analysis and transformation.
- [x] State and implement the role-indexed metadata projection rule. An
      arbitrary phantom parent cannot supply nominal metadata. The ordinary
      M-only application inversion rule does not establish this projection.
- [x] Implement source group annotations, outer scope, concrete context
      defaults, declared-prefix constraints and useful located diagnostics.
      Preserve explicit variables/computation and higher-rank kinds. Add no
      inferred matchability quantifier in this stage.
- [x] Migrate complete declaration kinds, constructor/pattern-synonym
      telescopes, interfaces, HIE, printers and compatibility versions.
- [x] Select and implement an explicit TH annotation representation with
      quotation, splice and reification round trips. An unsupported-case gate
      alone does not establish ordinary polymorphic quotation compatibility.
- [x] Preserve supported Typeable TyCon-kind prefixes. Keep general polytype
      reflection separate. Check both M63 interpreter paths and FUN controls.
- [x] Add a completed source observation to Showcase. Build both stages and
      run scope, role, inference, interface and source/runtime controls.
- [x] Review the selected compiler paths and validation evidence. Record
      conservative limits and keep publication separate from local validation.

Research and the executed comparator use native-forall-matchability-*.
The full R23 and project goals remain open. Generic native-domain widening,
family source unsaturation and the proposal's full inference policy need
their separate contracts and checks.

### Final M64 validation

The frozen source is specification head
`9a8d3bd1730913c795a387dbf2019339ce6ba49d` plus patch SHA256
`459cd7c30827784a52a49b7423f3071f9ca9fdfdbea1ae242eb1e62c4bd005be`.
Build v11 passes the bootstrap compiler, target libraries, self-hosted compiler,
Haddock, check-ppr and check-exact targets. Test-dependency build v1 also passes.
Both builds preserve their source inputs and main M63 checkout.

The final focused checks pass:

- Source development v3: 32 commands. Both stages check declaration-prefix
  defaults, existing M values, runtime42, computed-U FFI and the selected
  located rejections. Fresh M abstractions still reject.
- TH development v3: 18 commands. Both interpreter paths check declaration
  defaults, constructor context scope, all three quotation helpers,
  computed-M reflection/Binary and malformed type-wrapper rejection.
- HIE development v2: both stages write and read actual HIE files. Metadata,
  alpha sharing, Name identity and whole-group scope assertions pass.
- Printer development v3: six checks. Exact printing preserves source bytes
  and ASTs. Normal printing preserves ASTs. The controls cover annotations,
  comments, constructor contexts and adjacent forall groups.
- Maintained cohort v1: API goldens 1/37; both-stage public19, parameterized14
  plus three located rejections, Showcase95/14, two higher-rank controls and
  required-visibility runtime42. All 20 measured cases pass.
- Registered v1: seven passes, 61 cases and 54 skipped ways. There are no
  unexpected, framework or missing-library results. The selection includes
  HieVdq, EssentialsCoverage and the extended RetainedPrint fixtures.

The two existing printer fixtures were extended after the maintained cohort.
Their explicit source-equivalence receipt lists both changed inputs. Compiler
and library sources and artifacts are unchanged. The registered selection
checks the updated ASCII and Unicode fixtures. All check inputs stay unchanged
during their recorded runs. Historical failures remain separate records.

The bounded compiler review finds no acceptance defect in the selected paths.
It identifies an expression-checking limit: `pass f = f` can reuse an existing
M forall, but an application RHS such as `pass f = keep f True` can still take
runtime skolemisation even when its result has that exact type. Complete signed
non-variable expressions retain their scoped checking path. Generalize this
inference route separately. Do not weaken runtime-U checks to accept it.

Constructor-local evidence transport, general polytype reflection, the generic
native client, source family unsaturation and the full inference policy remain
open. These checks do not establish a preservation theorem or totality.
Publication still needs the separate binary dependency fork and branch.

Read-only formatter observation v2 covers 146 files. Fourmolu reports 96 format
differences and 48 parser failures; one file passes. Cabal-gild reports that
the unchanged compiler Cabal template is not formatted. The tools change no
source bytes. No formatter pass is claimed. Keep GHC formatting and the fork's
syntax instead of applying whole-file rewrites.

## M65: function-valued fields in closed native index domains

Roadmap owner: R05. Related cards: R03 and R23. Status: the bounded increment
passes the selected build, source, interface and registered checks.
M64 is the prerequisite. The full roadmap cards remain open.

An algebraic index can store an eligible opaque function. Use closed regular
boxed domains, with the existing constructor and representation restrictions.
Do not special-case Maybe. Function fields are leaves with an ordinary known-U
Many arrow between independently closed algebraic domains. Keep the old
function-free policy for those endpoints. In particular, a recursive data type
that stores a function over itself must not make eligibility checking loop.
Open parameters, polymorphic leaves and changed dependent domains remain open.

Use the existing local, unrestricted, non-join function Id policy. Admit that
Id as a `TermVarTy` inside a constructor index. Do not unfold the function or
compare its behavior. Function-field kinds must agree at their complete fixed
type. This does not introduce nonreflexive function-domain transport.

Canonical application is the bridge to existing native calls:

```
mkAppTy (TermVarTy f) a = TermAppTy f a
```

Apply the same rule in batched application, cast leftovers, substitution,
family reduction and interface reconstruction. A saturated family can remain
stuck until its container is refined. When it exposes f, application must agree
with direct native-call syntax. Do not keep two forms with different equality,
ordering or sharing keys.

Preserve forward evidence. With `q :: c ~N 'Just f`, lift q through the saturated
family and compose its equation to obtain `head_q :: Stored c ~N f`.
Use ordinary `AppCo head_q arg_q` to retain that head proof. Replacing it with
`TermAppCo f arg_q` would discard the family transport. Existing proofs must
retain their endpoints after a type variable is substituted by `TermVarTy f`.
Keep the current application role and argument-proof rules. M is still needed
for opaque inverse application. Equality of two U calls does not prove either
head or argument equality.

The first source client uses existing syntax:

```haskell
type BoolEndo = FUN @U 'Many Bool Bool
type family Stored (x :: Maybe BoolEndo) :: BoolEndo where
    Stored ('Just f) = f

data Box where
    Box :: foreach (x :: Maybe BoolEndo) -> foreach (b :: Bool)
        -> Flag (Stored x b) -> Box
```

`Flag` has ordinary False and True constructors. A producer explicitly cases
on `f b` to build its flag and packages `Just f`. A consumer matches
`Box (Just f) b proof`. Its later proof field must retain the evidence that
refines the whole container. A subsequent strict `case f b` exposes the same
call index. The type family is saturated before ordinary U application.
This client does not need source family unsaturation.

Use existing interface tags. Reconstruct ordinary applications through the
canonical makers. An applied Maybe remains unknown in the non-forcing shape
precheck; complete typed validation decides eligibility. Preserve scope, Many
and local Id checks. If direct nominal call congruence must be persisted,
encode it through the existing AppCo and reflexive function-value index.
Do not add a coercion axiom or imply support for DepFunCo persistence.
An older M64 reader must retain its positive controls and reject the new native
signatures during typed import. Bump the compatibility versions if wire fields
or tags change.

Acceptance checks:

- [x] Extend domain, field-kind, source and runtime reification admission
      together. Keep strict function endpoint checking separate.
- [x] Canonicalize single and batched application. Check family exposure,
      cast leftovers and substitution of pre-existing forward proofs.
- [x] Preserve later field types and proof scope through the constructor case.
      Keep same-spelling function names in separate packages distinct.
- [x] Check forward N/R/P application evidence under the existing role rules.
      Retain an M-positive inverse control and reject U/unknown inversion.
- [x] Compile separate producer and client modules using only interfaces.
      Check direct calls against family-exposed calls and imported proofs.
- [x] Check bottom container, function, argument and proof demand separately.
      Ignoring fields must stay lazy. Matching Just must not force its field.
- [x] Check located rejections for M/linear or polymorphic function leaves,
      open domains, invalid scope and unsupported function-domain casts.
- [x] Build both stages. Run the maintained API cohort and affected regressions.
      Add the completed source observations to Showcase.

Prior art: proposal #242 and the 2019 higher-order type-level programming paper,
Figure 5. They distinguish forward congruence from matchable inversion.
Agda's neutral application and scoped substitution supply the operational
comparison. These sources do not prove preservation for the native forms.
See REFERENCES.md and the selected source-reading records.

Generic open domains, arbitrary function indices, dependent function domains,
general native HIE/TypeRep, native unfoldings and normal optimization remain
separate work. Keep R23 and the full project goal open.

### M65 validation

The isolated full self-hosted build v12 passes in 946.92 seconds. It builds the
target libraries, both compilers and the selected compiler utilities. Its
27,298 source inputs remain unchanged. The source HEAD is
`1e5fd2ccfd7b6b5cb5aec41e4004430a9cf8e71f`; its compiled patch has SHA256
`dd0437dc73c93d10411712ba48c09acf52fb5d3d4b98953c6fceae9f1d4ac26e`.
The publication checkout remains at M64 during these checks.

Core/interface fixture v6 and the updated bootstrap source fixture pass their
unchanged 1/37-line goldens. The final registered selection checks both
maintained fixtures with the rebuilt self-hosted compiler.

Source v6 passes seventeen commands. Its separate producer and interface-only
client pass twelve runtime observations. The observations distinguish demand
for the container, function, argument and proof. Inaccessible and overlapping
pattern warnings are errors. Five unsupported domains reject with GHC-83605.
A cross-package proof mismatch rejects at its third argument with GHC-83865.
A looping family reports GHC-40404 at reduction depth thirteen with a limit of
twelve. The M64 reader rejects the new typed signature between two ordinary
positive imports. Focused matchable v1 retains GHC-40564 for a known-M shape
contradiction. All selected inputs remain unchanged during these checks.

The source case checker uses a bounded family view for activation and capture
discovery. It composes the full family reduction with the strict-call alias
proof. The solver retains U and unknown application equalities as unresolved;
it cannot infer a constructor contradiction from their shape. The imported
shape precheck retains raw Pi endpoints to reject recursive function domains
before forcing Type knots. Its positive controls include a regular recursive
container with a vacuous function alias.

The final source-both v1 run passes 36 commands across both rebuilt stages.
Each stage passes the twelve runtime observations and eight negative clients.
The negatives include the earlier five unsupported domains, cross-package proof
scope, known-M shape contradiction and bounded family reduction. Each stage
also passes the old-reader positive/rejection/positive sequence. Inputs and
artifact inventories remain unchanged.

Showcase includes the exercised providers and five additional observations.
The final showcase-both v1 run passes 100 lines from sixteen modules in each
stage. Its overlapping pattern warnings retain the existing nonfatal policy.
The function-field source controls still treat inaccessible and overlapping
warnings as errors.

Test prerequisites v1 pass. Registered v1 reports seven expected normal-way
passes, 54 skipped ways and zero unexpected failures or framework warnings.
The selected tests are NativeDependentCore, NativeDependentCoreRun,
RetainedKeywordOff, RetainedQuantification, RetainedPrint, HieVdq and
EssentialsCoverage. The maintained source fixture compiles both function-field
providers in fresh one-shot contexts against interfaces. It checks the twelve
runtime observations and located GHC-40404, GHC-83865 and GHC-40564 controls.
No new test file or golden is added.

Read-only formatter v2 observes 28 files without changes: nine Fourmolu format
differences, sixteen parser failures, two timeouts and one Cabal-gild failure
on the unchanged compiler template. Formatter v1's earlier timeout remains
recorded. No formatter pass is claimed. Keep the GHC source style and the
experimental source syntax.

Evidence is under `_build/verification/native-function-field-*`. Compiler and
library inputs are fixed during each run. Subsequent status edits change only
Markdown documents; the final receipt records their relation to this snapshot.

The inherited pure family normalizers in matchExpectedTyConApp and native
argument inference remain a separate follow-up. The bounded strict-call view
does not establish a bound for every native family-normalization path.
The ignored `native-bounded-family-research-v1` note pins twenty source files,
proposed trigger programs and the required nominal proof directions. Those
programs are not executed results. No totality checker is implemented.

## M66: bounded family views in native frontend inference

Roadmap owner: R05. Related card: R03. Prerequisite: validated M65.
Status: contract selected; baseline execution and implementation remain open.

The native shape matcher and immutable argument cache use a deep pure family
normalizer. It has no solver reduction-depth state. First execute the prepared
looping and finite controls on M65. Record actual outcomes before selecting a
source change. A predicted timeout is not an executed result.

Use the existing nominal family-only solver rewrite. Add one narrow TcM wrapper
in GHC.Tc.Solver and a corresponding hs-boot signature for the Unify import
cycle. Keep TcS internals behind that wrapper. Ordinary views retain their
current gates. Do not introduce a new equality, coercion or reduction rule.

For Unify, reduce the original native family view. Return the inverse nominal
proof only when the required algebraic TyCon is exposed. Keep the existing
fallback otherwise. Do not allocate a fresh meta for a native parameter.
Capture the current use-site context and the actual family name.

For App, keep each cache position and restore its original renamed argument
context before constructing CtLoc. Apply the same bounded view at initial,
extra and final matching. Retain the original typed expression, inferred type
and saved usage. Existing final subsumption constructs the actual family proof.
Do not infer the expression or emit its usage again.

Each view uses the configured reduction-depth limit. The existing runRewrite
starts the view at depth zero. Recursive family RHS reduction increments that
same location. This is not a global rewrite-work budget. The explicit flag
-freduction-depth=0 retains its existing meaning.

Acceptance:

- [ ] Observe both original native loop candidates on the fixed M65 compiler.
- [ ] Preserve ordinary GHC-40404 behavior for a nonnative recursive family.
- [ ] Produce located GHC-40404 for both native views under a finite limit.
- [ ] Check source argument context for the second cached argument and a leading
  explicit type application. Assert the code, useful type and location/context.
  Do not depend on one exact depth counter value.
- [ ] Preserve a terminating list shape whose element contains an actual runtime
  vector length. Check cached selection, returned payload scope and lazy fields.
- [ ] Build both stages. Run the existing API/source and registered controls, plus
  the maintained Showcase. Add focused controls to an existing fixture.
- [ ] Record source, diagnostic, runtime and artifact provenance separately.

Primary code evidence is in the ignored `native-bounded-family-research-v1`
and corrected `native-bounded-family-diagnostics-research-v1` records.
The proposed source programs are in `native-bounded-family-probes-preparation-v2`.
The probe runner v3 adds dependency and inventory checks. All controls remain
unexecuted. See [the reference ledger](REFERENCES.md#bounded-native-frontend-family-views).
This draft changes no generic-domain, optimizer, totality or erasure policy.
