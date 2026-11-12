# Native dependent Core implementation plan

Implement dependent types in Haskell through a native extension to Core. Base the
work on Richard Eisenberg's historical `nokinds-pi` branch. Port its useful changes
to current GHC and complete the missing rules. The first working target is a
retained Boolean argument whose value determines the result type.

Last updated: 2026-11-12. Compiler source baseline:
`19ff36f2aede0ca2e554016ff22c71e73aa9803b`.

See [current status](README.md) and [primary references](REFERENCES.md).

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
dependent `case` expressions require a separate context-refinement rule.

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
Dependent source `case` expressions remain separate work.

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

These signatures and definitions are targets, not implemented syntax coverage.
The result uses the caller's actual runtime index. A number read from input can
select a vector length without an explicit existential package. `Add` uses
ordinary closed-family equations; this does not require arithmetic SMT solving.
The ordinary GADT helper keeps append independent of runtime length evaluation.
Its constructor pattern exposes an erased predecessor index, not a runtime Nat.
The retained wrapper must instantiate the helper with the caller's runtime
indices. `replicate` supplies the actual predecessor value through its Nat
pattern. See [the Agda Nat/Vec references](REFERENCES.md#agda-provenance).

- [ ] Generalize native index kinding and identifier validation from `Bool` to
      supported lifted algebraic domains. Keep scope and multiplicity checks.
- [ ] Represent saturated constructor indices such as `S n`. Preserve their
      runtime field identities through substitution, tidying, and type recovery.
- [ ] Refine constructor alternatives with both real fields and a virtual proof.
      Preserve the fields at runtime. Erase only the checked proof.
- [ ] Check recursive Core for vector replication with an unknown runtime Nat.
      This is an intermediate gate; it does not complete the source target.
- [ ] Extend frontend checking to scoped constructor-field indices, outer type
      quantification, and multiple retained binders. Support safe GADT constructor
      instantiation without allowing a runtime index to escape through a metavariable.
- [ ] Compile and run the source `replicate` target, then source `append`, with
      actual input values. Preserve the same index across producer and consumer.
- [ ] Reject inconsistent vector lengths and escaping predecessor indices with
      located diagnostics. Check lazy recursive fields and unused index arguments.
- [ ] Transport the resulting signatures and programs across modules. Keep format
      compatibility and any unfinished M8 unfolding work explicit. Pin a new
      configured format before emitting Nat signatures and check `.1` rejection.

Gate: a source program reads runtime lengths, constructs and appends vectors,
and consumes the statically indexed results. Check empty and successor inputs.
Check that observing the first element of a replication at `S undefined` does
not force its predecessor. A hand-built Core example alone does not satisfy M9.

Resolve these preservation rules in the implementation:

- In a native `S k` alternative, put the actual field binder before the virtual
  proof that mentions it. The current `cpt_dicts` insertion precedes runtime
  fields and is insufficient. Preserve ordinary GADT evidence positions.
- Generalize the fixed-domain check through eligible TyCon/DataCon structure.
  Start with closed, promotable, first-order data domains. Preserve lazy fields.
  Exclude existential, constrained, newtype, and unboxed index constructors in
  this increment. Do not give the user-defined `Nat` a wired compiler identity.
- Introduce the complete retained telescope before transporting an equation's
  ordinary argument and result types. Splitting the first index must not require
  the unsupported congruence for a remaining dependent function type.
- Give GADT instantiation an explicit runtime-index scope rule. The current
  metavariable guards are intentional. Removing them without a scope check can
  let a predecessor escape. Evaluate expected-result constructor instantiation
  and scoped meta-assignment against Agda's context rules.
- Check CorePrep naming of constructor arguments. Naming `f (S k)` as
  `let t = S k in f t` changes its recovered index to `TermVarTy t`. Specify and
  check alias transport before claiming end-to-end constructor-argument support.
  Do not use an unchecked cast or equality from an unfolding.
- Preserve partial and cyclic Haskell values. Check finite vector prefixes for
  `let n = S n`, a head from `replicate (S bottomNat) x`, and failure when its
  remaining spine needs `bottomNat`. Check `head bottomNat (VCons x undefined)`
  returns `x`. Agda's termination assumptions do not establish these properties.

Use one compiled worker with unknown `n` and `m` from IORefs. For lengths two
and three, `append n m (replicate n 42) (replicate m 7)` must observe
`[42,42,7,7,7]`. Also check zero and successor inputs. Reject a constant `VNil`
at unknown `n`, a vector at `n` where `S n` is required, and an unrelated runtime
index. Keep `Add n m` distinct from `Add m n` without a supplied proof.

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
compilation. Source `case` refinement, ordinary optimization, and cross-module
native interfaces remain open.

## Further work

After M8, review broader retained kinds, nested and higher-order dependence,
source inference, arbitrary index expressions, roles involving dependent
functions, linear retained binders, and dependent runtime representations.
Use separate tasks for each extension. SMT reasoning and proof-certificate
checking remain separate work within the longer-term program.
