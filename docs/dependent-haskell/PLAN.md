# Native dependent Core implementation plan

Implement dependent types in Haskell through a native extension to Core. Base the
work on Richard Eisenberg's historical `nokinds-pi` branch. Port its useful changes
to current GHC and complete the missing rules. The first working target is a
retained Boolean argument whose value determines the result type.

Last updated: 2026-11-14. Upstream compiler base:
`19ff36f2aede0ca2e554016ff22c71e73aa9803b`.

See [current status](README.md) and [primary references](REFERENCES.md).

## Current priority: execute from source

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

This target is unverified. Return a runtime sum with ordinary equality
evidence, then use that evidence to package an appended vector. Follow
the [pinned addition, vector append and congruence definitions](REFERENCES.md#computed-length-append-next-source-probe).

```haskell
data Sum (n :: Nat) (m :: Nat) where
  Sum :: foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m

sumNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Sum n m
appendPacked :: PackedVec a -> PackedVec a -> PackedVec a
```

- [ ] Probe the declaration and `Sum m Refl :: Sum Z m` separately.
  Construction must instantiate the erased `m` with the captured runtime
  index before allocating mutable instantiation variables. The current
  result matcher may stop at the constructor's retained argument. Establish
  that boundary before extending it. Prefer a constructor-only result match
  with existing kind, scope and final subsumption checks.
- [ ] Define `Add` and source append by recursion on the first length.
  Define `sumNat` with the same recursion. In the successor case, match
  recursive `Refl` before constructing `Sum (S p) Refl`. Reuse family
  reduction and ordinary equality congruence.
- [ ] Open both packages, match `Sum p Refl`, and repackage the appended
  vector at `p`. Reject forged sums, missing equality elimination, swapped
  indices without a lemma, and an escaping result index.
- [ ] Check independent runtime inputs and finite element order. Observe
  package, equality, length and vector demand separately. This proposed
  `sumNat` forces the entire first Nat spine before returning a constructor.
  Do not claim productivity for a cyclic first length. Check that a finite
  first length can leave the second length and vector fields lazy.
- [ ] Validate both compiler stages, existing controls and resource limits.
  Record provenance, located errors and reproducible artifacts.

## Further work

After M8, review broader retained kinds, nested and higher-order dependence,
source inference, arbitrary index expressions, roles involving dependent
functions, linear retained binders, and dependent runtime representations.
Use separate tasks for each extension. SMT reasoning and proof-certificate
checking remain separate work within the longer-term program.
