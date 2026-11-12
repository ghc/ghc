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
proposal. Restricted source `keep` now checks and executes through the native
bytecode entry. Source constructor refinement for `choose` remains unimplemented.

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
scanner includes synonym definitions and original arguments. The normal program,
optimizer, and interface gates remain. JavaScript and native machine-code
execution are outside this increment.

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

Implement this task in two increments. First add the local experimental flag
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
checked bytecode entry. Source `keep` passes six runtime and laziness observations.
Parsing alone does not satisfy the full M7 gate; source `choose` remains open.

- [x] Add an experimental opt-in flag, syntax, AST representation, and diagnostics.
- [x] Support native zonking, rigid signature generalization, scoped validity,
      opaque index rewriting, and same-Id equality for the first source rule.
- [x] Extend renaming and checking for explicit retained Boolean signatures.
      Track one runtime binder when it occurs in both terms and types.
- [x] Start with one equation and a bare retained variable pattern. Reuse its
      renamed `Name` in the expected type and existing pattern/desugaring path.
      Support native zonking and require an identity result wrapper.
- [x] Elaborate constructor and retained-variable applications into native Core.
- [ ] Elaborate dependent Boolean patterns and source `choose`.
- [x] Accept ordinary runtime Booleans under shared bindings. Reject unsupported
      index expressions and inferred aliases with precise diagnostics.
- [x] Update source printers and extension documentation.

Gate: the source target and a runtime-unknown Boolean example compile and execute
directly. Their implementation uses no singleton ABI or reification step.

### M8: support interfaces and validate the compiler change

- [ ] Encode retained binders, indices, and evidence in interface types, unfoldings,
      fingerprints, and recompilation dependencies.
- [ ] Check cross-module calls, imported unfoldings, and interface round trips.
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
some values and laziness cases. Restricted source `keep` now executes. Source
Boolean refinement, ordinary optimization, and module integration remain open.

## Further work

After M8, review broader retained kinds, nested and higher-order dependence,
source inference, arbitrary index expressions, roles involving dependent
functions, linear retained binders, and dependent runtime representations.
Use separate tasks for each extension. SMT reasoning and proof-certificate
checking remain separate work within the longer-term program.
